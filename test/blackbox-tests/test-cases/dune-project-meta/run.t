Test the various new fields inside the dune-project file.


  $ echo '(lang dune 1.0)' > dune-project
  $ echo '(github_project foo/bar)' >> dune-project
  $ echo '(license ISC)' >> dune-project
  $ echo '(authors "Anil Madhavapeddy" "Rudi Grinberg")' >> dune-project

The `dune build` should work.

  $ dune build


