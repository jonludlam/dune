This test generates documentation for non-hidden modules only for a library:

  $ ocamlc -c -bin-annot foo__.ml

  $ dune build @doc

 Hidden modules should be compiled
  $ find _build/default -name '*.odoc'
  _build/default/_doc/_odoc/pkg/foo/page-index.odoc
  _build/default/.foo__private.objs/byte/foo__private__Bar.odoc
  _build/default/.foo__private.objs/byte/foo__private.odoc
  _build/default/.foo__private.objs/byte/foo__private__Foo__.odoc

 Hidden modules should not be linked
  $ find _build/default -name '*.odocl'
  _build/default/_doc/_odocls/foo/page-index.odocl

 We don't expect html for hidden modules
  $ find _build/default -name '*.html'
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/foo/index.html
