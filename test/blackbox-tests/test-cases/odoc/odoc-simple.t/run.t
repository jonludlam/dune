This test generates documentation using odoc for a library:

  $ dune build @doc

This test if `.odocl` files are generated
  $ find _build/default/_doc/_odocl/{bar,foo} -name '*.odocl' | sort -n
  _build/default/_doc/_odocl/bar/bar/bar.odocl
  _build/default/_doc/_odocl/bar/bar/page-index.odocl
  _build/default/_doc/_odocl/bar/page-index.odocl
  _build/default/_doc/_odocl/foo/foo.byte/foo_byte.odocl
  _build/default/_doc/_odocl/foo/foo.byte/page-index.odocl
  _build/default/_doc/_odocl/foo/foo/foo.odocl
  _build/default/_doc/_odocl/foo/foo/foo2.odocl
  _build/default/_doc/_odocl/foo/foo/foo3.odocl
  _build/default/_doc/_odocl/foo/foo/page-index.odocl
  _build/default/_doc/_odocl/foo/page-index.odocl

  $ dune runtest
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>index (index)</title><meta charset="utf-8"/><link rel="stylesheet" href="odoc.support/odoc.css"/><meta name="generator" content="odoc 3.1.0"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><script src="odoc.support/highlight.pack.js"></script><script>hljs.initHighlightingOnLoad();</script></head><body class="odoc"><nav class="odoc-nav"></nav><header class="odoc-preamble"><h1 id="ocaml-package-documentation"><a href="#ocaml-package-documentation" class="anchor"></a>OCaml package documentation</h1><ul><li><a href="bar/index.html" title="index">bar</a></li><li><a href="foo/index.html" title="index">foo</a></li></ul></header><div class="odoc-content"></div></body></html>

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
