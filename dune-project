(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/robin))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name robin)
 (synopsis "Robust binary encoding.")
 (description "Robin is a robust binary format, where robust means that one can easily extend encodings in a backward-compatible way. This library defines encoding and decoding functions for this format.")
 (depends (ocaml (>= 4.07))))
