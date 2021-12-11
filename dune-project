(lang dune 2.9)

(name day1)

(authors "Felix Brousseau-Tremblay")

(maintainers "Felix Brousseau-Tremblay")

(generate_opam_files true)

(package
  (name day1)
 (depends
   dune
   merlin
   ocamlformat
   ocp-indent
   utop
   (ocaml (>= 4.13.1))
 )
)
