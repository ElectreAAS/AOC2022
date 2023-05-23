(lang dune 3.7)

(name aoc2022)

(generate_opam_files true)

(source
 (github ElectreAAS/AOC2022))

(authors "Ambre Austen Suhamy")

(maintainers "Ambre Austen Suhamy")

(license GPL-3.0-only)

(package
 (name aoc2022)
 (synopsis "Advent of Code 2022")
 (depends
  (domainslib :dev)
  (eio
   (>= 0.8.1))
  (eio_main
   (>= 0.8.1))
  (mtime
   (>= 2.0.0))
  (notty
   (>= 0.2.3))
  (ocaml
   (>= 5.0.0))
  (ocamlformat
   (and
    (= 0.25.1)
    :with-test))
  (ocaml-lsp-server :with-test)
  (alcotest
   (and
    (>= 1.0.0)
    :with-test))))
