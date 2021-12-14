(lang dune 2.9)

(name aoc2021)

(authors "Felix Brousseau-Tremblay")

(maintainers "Felix Brousseau-Tremblay")

(generate_opam_files true)

(package
  (name aoc2021)
 (depends
   (ocaml (>= 4.13.1))
   (base (>= 0.14.2))
   (core (>= 0.14.1))
   (dune :build)
   (ppx_inline_test :test)
   (merlin :dev)
   (ocamlformat :dev)
   ( ocp-indent :dev )
   ( utop :dev)
 )
)
