Test that odoc assets (images, etc.) are properly processed:

  $ dune build @doc

Verify the asset .odocl file was compiled:

  $ find _build/default/_doc/_odocl/mylib -name '*.odocl' | sort
  _build/default/_doc/_odocl/mylib/asset-logo.png.odocl
  _build/default/_doc/_odocl/mylib/mylib/mylib.odocl
  _build/default/_doc/_odocl/mylib/mylib/page-index.odocl
  _build/default/_doc/_odocl/mylib/page-index.odocl

Verify the asset was copied to HTML output:

  $ test -f _build/default/_doc/_html/mylib/logo.png && echo "logo.png exists in HTML output"
  logo.png exists in HTML output

Verify the index.html references the asset:

  $ test -f _build/default/_doc/_html/mylib/index.html && echo "index.html exists"
  index.html exists
