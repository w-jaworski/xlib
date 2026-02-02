(lang dune 3.0)
(name xlib)
(generate_opam_files)

(package
 (name xlib)
 (synopsis "Biblioteka XLib")
 (description "Biblioteka pomocnicza XLib (string, list, json, etc.)")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 3.0))
  (zarith (>= 1.12))
  (camlzip (>= 1.0))  ; zip w OCamlu to zazwyczaj camlzip
  (camlbz2 (>= 0.6))  ; bz2 w OCamlu to zazwyczaj camlbz2
 ))
