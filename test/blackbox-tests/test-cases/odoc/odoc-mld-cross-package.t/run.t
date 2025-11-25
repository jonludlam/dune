Test that package mld files can reference modules from other packages.
Package A's index.mld contains a reference to {!Libb} from package B.

Build documentation (note: cross-package references in mld files currently show warnings):

  $ dune build @doc
  File "pkga/index.mld", line 5, characters 47-54:
  Warning: Failed to resolve reference unresolvedroot(Libb) Couldn't find "Libb"

Verify documentation was generated for both packages:

  $ find _build/default/_doc/_odocls/{pkga,pkgb} -name '*.odocl' | sort -n
  _build/default/_doc/_odocls/pkga/page-index.odocl
  _build/default/_doc/_odocls/pkga/pkga.lib/liba.odocl
  _build/default/_doc/_odocls/pkga/pkga.lib/page-index.odocl
  _build/default/_doc/_odocls/pkgb/page-index.odocl
  _build/default/_doc/_odocls/pkgb/pkgb.lib/libb.odocl
  _build/default/_doc/_odocls/pkgb/pkgb.lib/page-index.odocl

Check that HTML was generated for both package indexes:

  $ ls _build/default/_doc/_html/pkga/index.html
  _build/default/_doc/_html/pkga/index.html

  $ ls _build/default/_doc/_html/pkgb/index.html
  _build/default/_doc/_html/pkgb/index.html

Both libraries' HTML should be generated even though cross-package references don't resolve:

  $ ls _build/default/_doc/_html/pkga/pkga.lib/Liba/index.html
  _build/default/_doc/_html/pkga/pkga.lib/Liba/index.html

  $ ls _build/default/_doc/_html/pkgb/pkgb.lib/Libb/index.html
  _build/default/_doc/_html/pkgb/pkgb.lib/Libb/index.html
