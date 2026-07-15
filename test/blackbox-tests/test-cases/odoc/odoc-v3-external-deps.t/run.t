Build and install the external package, then give it odoc files the
way odd does: compiled with --parent-id, placed next to the cmtis.
(extlib.ml has no .mli, so dune installs a .cmt rather than a .cmti.)

  $ dune build --root ext @install 2>&1
  $ dune install --root ext --prefix $PWD/_install --display quiet 2>&1
  $ odoc compile $PWD/_install/lib/extpkg/extlib.cmt \
  >   --parent-id extpkg/extlib -o $PWD/_install/lib/extpkg/extlib.odoc

The main project resolves references into the external package's tree.
The build is warning-free, proving the {!/extpkg/Extlib.v} reference
resolved:

  $ export OCAMLPATH=$PWD/_install/lib
  $ dune build --root main @doc 2>&1
