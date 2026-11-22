(lang dune 3.25)

(name transw)

(generate_opam_files true)

(source
 (github krimpast/transw))

(authors "Nikita Egorov <avelparmen@gmail.com>")

(maintainers "Nikita Egorov <avelparmen@gmail.com>")

(license MIT)

(package
 (name transw)
 (synopsis "CLI-translate wrapper with response caching")
 (description "A longer description")
 (depends ocaml sqlite3 unix))
