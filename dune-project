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
  (ocaml
   (>= 5.0.0))
  (domainslib :dev)
  (eio
   (>= 0.8.1))
  (eio_main
   (>= 0.8.1))
  (notty
   (>= 0.2.3))
  (mtime
   (>= 2.0.0))
  (alcotest
   (and
    (>= 1.0.0)
    :with-test))))
