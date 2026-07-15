Build and install the external package, but do not give it odoc files
(no odd-style `odoc compile` step for it).

  $ dune build --root ext @install 2>&1
  $ dune install --root ext --prefix $PWD/_install --display quiet 2>&1

Without odoc files for the external package, @doc still succeeds even
though the reference cannot resolve: dune's own plumbing (-I/-L/-P for
extpkg) degrades silently rather than erroring on the missing files -
the only diagnostic is odoc's own unresolved-reference warning, not a
build failure or a "missing root" warning (which would indicate we
wrongly passed --enable-missing-root-warning):

  $ export OCAMLPATH=$PWD/_install/lib
  $ dune build --root main @doc 2>&1
  Entering directory 'main'
  File "mainlib.ml", line 1, characters 10-29:
  Warning: Failed to resolve reference /extpkg/Extlib.v Path '/extpkg/Extlib' not found
  Leaving directory 'main'
