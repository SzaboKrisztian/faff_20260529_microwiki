(lang dune 3.23)

(name microwiki)

(generate_opam_files true)

(authors "Chris Szabo <mail@krisztianszabo.com>")

(license MIT)

(package
 (name microwiki)
 (synopsis "A simple wiki engine")
 (description "A simple project to learn OCaml by building a wiki engine with Dream and Caqti")
 (depends ocaml dream caqti caqti-lwt caqti-driver-sqlite3 str alcotest))
