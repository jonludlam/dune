Test that packages discovered through the library closure get -P flags
during odoc link, even when they are not listed as :with-doc dependencies.

Setup: pkga depends on pkgb (:with-doc) and liba depends on pkgc.lib
and odoc-parser. pkgc is a local package NOT listed as :with-doc.
odoc-parser is an installed package NOT listed as :with-doc.
Both should be discovered through the library closure.

Build documentation:

  $ dune build @doc

Verify documentation was generated for all three local packages
(pkgc is included even though it's not a :with-doc dep, because
liba depends on pkgc.lib):

  $ find _build/default/_doc/_odocl -name '*.odocl' | sort
  _build/default/_doc/_odocl/pkga/page-index.odocl
  _build/default/_doc/_odocl/pkga/pkga.lib/impl-liba.odocl
  _build/default/_doc/_odocl/pkga/pkga.lib/liba.odocl
  _build/default/_doc/_odocl/pkga/pkga.lib/page-index.odocl
  _build/default/_doc/_odocl/pkgb/page-index.odocl
  _build/default/_doc/_odocl/pkgb/pkgb.lib/impl-libb.odocl
  _build/default/_doc/_odocl/pkgb/pkgb.lib/libb.odocl
  _build/default/_doc/_odocl/pkgb/pkgb.lib/page-index.odocl
  _build/default/_doc/_odocl/pkgc/page-index.odocl
  _build/default/_doc/_odocl/pkgc/pkgc.lib/impl-libc.odocl
  _build/default/_doc/_odocl/pkgc/pkgc.lib/libc.odocl
  _build/default/_doc/_odocl/pkgc/pkgc.lib/page-index.odocl

Verify that Libc references in liba's HTML point to local paths:

  $ grep -o 'href="[^"]*Libc[^"]*"' _build/default/_doc/_html/pkga/pkga.lib/Liba/index.html | sort -u
  href="../../../pkgc/pkgc.lib/Libc/index.html#type-c_type"
  href="../../../pkgc/pkgc.lib/Libc/index.html#val-c_function"

Verify that Odoc_parser references are remapped to ocaml.org (installed pkg):

  $ grep -o 'href="[^"]*Odoc_parser[^"]*"' _build/default/_doc/_html/pkga/pkga.lib/Liba/index.html | sed 's|/[0-9.]*-*[a-z0-9]*/doc|/VERSION/doc|g' | sort -u
  href="https://ocaml.org/p/odoc-parser/VERSION/doc/odoc-parser/Odoc_parser/index.html#type-t"

Inspect the link rule for liba to verify that -P flags include packages from
the library closure. In particular, odoc-parser and pkgc must appear even
though they are not :with-doc deps - they are discovered as transitive library
dependencies:

  $ dune rules _build/default/_doc/_odocl/pkga/pkga.lib/liba.odocl 2>&1 | grep -E '^\s+\-P$' -A1 | grep -v '^\s*-P$' | grep -v '^--$' | sed 's|^ *||' | sed 's|:.*||' | sort -u
  astring
  camlp-streams
  odoc-parser
  pkga
  pkgb
  pkgc
