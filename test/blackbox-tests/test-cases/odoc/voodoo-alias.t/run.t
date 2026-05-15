End-to-end test for the @doc alias backed by voodoo.

The link command for the local lib carries one -P flag per pkg and one
-L flag per library found via voodoo's marker files (the trigger set
here is cmdliner plus whichever package provides the OCaml stdlib;
cmdliner has one lib so we just check it). html-generate carries a
single generic -R remap into ocaml.org's /p/ tree (one rule covers
every switch dep).

  $ dune build @doc --verbose 2>&1 | grep -oE -- '-([RPL]) [^ ]*cmdliner[^ ]*|-R p/[^ ]*' | sort -u
  -L cmdliner:../../_voodoo/_odoc/p/cmdliner/1.3.0/doc/cmdliner
  -P cmdliner:../../_voodoo/_odoc/p/cmdliner/1.3.0/doc
  -R p/:https://ocaml.org/p/

Local HTML is produced for the workspace's public library.

  $ test -f _build/default/_doc/_html/myapp/Myapp/index.html

The voodoo compile-only output for cmdliner exists.

  $ test -f _build/default/_doc/_voodoo/_odoc/p/cmdliner/1.3.0/__odoc_partial.m

@doc does not produce voodoo HTML — only compile-only outputs.

  $ test ! -d _build/default/_doc/_voodoo/_html/p/cmdliner
