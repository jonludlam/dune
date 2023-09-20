This test generates documentation for non-hidden modules only for a library:

  $ dune build @doc-new

 Hidden modules should be compiled
  $ find _build/default/_doc_new/odoc/internal/ -name '*.odoc' | sort -n
  _build/default/_doc_new/odoc/internal//foo/foo.odoc
  _build/default/_doc_new/odoc/internal//foo/foo__.odoc
  _build/default/_doc_new/odoc/internal//foo/foo__Bar.odoc

 Hidden modules should not be linked
  $ find _build/default/_doc_new/odoc/internal/ -name '*.odocl' | sort -n
  _build/default/_doc_new/odoc/internal//foo/foo.odocl

 We don't expect html for hidden modules
  $ find _build/default/_doc_new/html/docs/foo -name '*.html' | sort -n
  _build/default/_doc_new/html/docs/foo/Foo/index.html
  _build/default/_doc_new/html/docs/foo/index.html
