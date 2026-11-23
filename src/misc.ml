open Printf

let cmp_log exp act =
  if String.equal exp act then true
  else (
    printf "Test failed!\n";
    printf "Expected: `%s`\n" exp;
    printf "Actual  : `%s`\n" act;
    false)

let preprocess_request str =
  let is_space = Char.equal ' ' in
  String.drop_last_while is_space (String.drop_first_while is_space str)

let get_lang str_text =
  let russian =
    "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
  in
  let english = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" in
  let text = List.of_seq (String.to_seq str_text) in

  let rec helper bal = function
    | x :: xs ->
        if String.contains russian x then helper (bal - 1) xs
        else if String.contains english x then helper (bal + 2) xs (* magic. *)
        else helper bal xs
    | [] -> bal
  in
  if helper 0 text >= 0 then "en:ru" else "ru:en"

(** [preprocess_request] tests *)

let%test _ = cmp_log (preprocess_request "sample") "sample"
let%test _ = cmp_log (preprocess_request "dropped_end  ") "dropped_end"
let%test _ = cmp_log (preprocess_request "  dropped_start") "dropped_start"
let%test _ = cmp_log (preprocess_request "with between") "with between"

let%test _ =
  cmp_log (preprocess_request "  difficult  test   ") "difficult  test"

let%test _ = cmp_log (preprocess_request "  русский тест   ") "русский тест"

(** [get_lang] tests *)

let%test _ = cmp_log (get_lang "") "en:ru"
let%test _ = cmp_log (get_lang "hello") "en:ru"
let%test _ = cmp_log (get_lang "привет") "ru:en"
let%test _ = cmp_log (get_lang "hi привет") "ru:en"
let%test _ = cmp_log (get_lang "hello пока") "en:ru"
let%test _ = cmp_log (get_lang "hi! hi! пока") "en:ru"
let%test _ = cmp_log (get_lang "hi! hi! покаа") "ru:en"
