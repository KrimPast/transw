open Sqlite3
open Printf

let dbName = "translates.db"

let dbPath =
  sprintf "%s%s%s"
    (Filename.dirname @@ Sys.executable_name)
    Filename.dir_sep dbName

exception TransBadReturnCode

let print_log ?(check = true) message rc =
  printf "LOG: %s [%s]\n" message (Sqlite3.Rc.to_string rc);
  Stdlib.flush stdout;
  if check then Rc.check rc

let get_current_time () = Unix.time ()

let decode_current_time float_time =
  let time = Unix.localtime float_time in
  let day = time.Unix.tm_mday in
  let month = time.Unix.tm_mon + 1 in
  let year = time.Unix.tm_year + 1900 in

  if Float.compare float_time 0.0 <> 0 then
    sprintf "%02d/%02d/%04d, %02d:%02d" day month year time.tm_hour time.tm_min
  else "null date"

let open_db () =
  let opened = Out_channel.open_gen [ Open_creat ] 0o644 dbPath in
  Out_channel.close opened;
  let db = db_open ~mode:`NO_CREATE dbPath in
  print_log (sprintf "Open %s" dbName) Rc.OK;
  db

let close_db db = db_close db

let run_utility string =
  let flag1 = "-show-alternatives" in
  let flag2 = "-show-translation-phonetics" in
  let lang = Misc.get_lang string in
  let stdout =
    Unix.open_process_args_in "trans"
      [| "grep"; flag1; "n"; flag2; "n"; lang; string |]
  in
  let output = In_channel.input_all stdout in
  let status = Unix.close_process_in stdout in
  match status with WEXITED 0 -> output | _ -> raise TransBadReturnCode

let init_translates_table db =
  exec db
    {|
  CREATE TABLE IF NOT EXISTS translates (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date REAL,
    key TEXT NOT NULL,
    value TEXT
)|}

let cache_data db key value =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO translates (date, key, value) VALUES (?, ?, ?)"
  in

  Fun.protect
    ~finally:(fun () -> ignore (finalize stmt))
    (fun () ->
      Rc.check
        (bind_values stmt
           [ Data.FLOAT (get_current_time ()); TEXT key; Data.TEXT value ]);
      Rc.check (step stmt))

let try_load_cached_data db key =
  let stmt =
    Sqlite3.prepare db "SELECT date, value FROM translates WHERE key = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (finalize stmt))
    (fun () ->
      Rc.check (bind_values stmt [ Data.TEXT key ]);
      match step stmt with
      | Rc.ROW -> Some (Sqlite3.column_double stmt 0, Sqlite3.column_text stmt 1)
      | Rc.DONE -> None
      | rc ->
          Rc.check rc;
          print_log "Got " rc;
          failwith "unexpected RC")

let () =
  let db = open_db () in
  print_log "Connect to translates table" (init_translates_table db);
  Sys.catch_break true;
  let rec loop () =
    try
      let line = Misc.preprocess_request (read_line ()) in
      if not (String.is_empty line) then (
        ignore (Sys.command "clear");
        let output =
          match try_load_cached_data db line with
          | Some value ->
              let time = decode_current_time (fst value) in
              print_log (sprintf "Retrieved from cache (%s)" time) Rc.OK;
              snd value
          | None ->
              let value = run_utility line in
              cache_data db line value;
              print_log "Cached successfully!" Rc.OK;
              value
        in
        print_endline output);
      loop ()
    with
    | End_of_file | Sys.Break -> ()
    | TransBadReturnCode ->
        print_log ~check:false "Translate app returned unexpected code."
          Rc.ERROR;
        print_log ~check:false "Check your internet connection." Rc.ERROR
  in
  loop ();
  print_log "Close database" (if close_db db then Rc.OK else Rc.ERROR)
