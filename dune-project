(lang dune 3.6)

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
 (description "Advent of Code 2022")
 (depends
  domainslib
  mtime
  mtime.clock.os
  (alcotest :with-test)))
