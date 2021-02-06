(lang dune 2.7)
(name ctfp_exercises)
(generate_opam_files true)

; (source (github mirage/ocaml-cohttp))
(license MIT)
(authors "Shon Feder")
(maintainers "TODO")

(package
 (name ctfp_exercises)
 (synopsis "Notes and solutions to Category for Programmers in OCaml")
 (description "TODO")
 (depends
  (dune (> 2.7))
  (mdx :build)
  (qcheck :with-test)
  (qcheck-alcotest :with-test)
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
))
