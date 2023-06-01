(lang dune 2.0)

(name lqccs-lang)

(generate_opam_files true)

(authors "LIS")

(maintainers "LIS")

(license ISC)

(using menhir 2.0)

(package
 (name lqccs-lang)
 (synopsis "synopsis")
 (description "lqccs interpreter")
 (depends 
  ocaml
   dune
   menhir
   ppx_deriving
   owl))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
