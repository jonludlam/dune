Assets listed in the (documentation (files ...)) stanza are compiled as odoc
assets and copied into the html output; pages can reference them.

  $ dune build @doc

The asset is compiled and linked alongside the package's pages:

  $ find _build/default/_doc/_odoc/mylib -name 'asset-*' | sort
  _build/default/_doc/_odoc/mylib/asset-logo.png.odoc
  $ find _build/default/_doc/_odocls/mylib -name '*.odocl' | sort
  _build/default/_doc/_odocls/mylib/asset-logo.png.odocl
  _build/default/_doc/_odocls/mylib/mylib/mylib.odocl
  _build/default/_doc/_odocls/mylib/page-index.odocl

The asset itself is copied into the html tree, next to the package's pages:

  $ cat _build/default/_doc/_html/mylib/logo.png
  not-really-a-png

The page's image reference resolved (the img tag points at the asset):

  $ grep -c 'logo.png' _build/default/_doc/_html/mylib/index.html
  1
