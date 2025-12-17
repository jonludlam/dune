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
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>index (index)</title><meta charset="utf-8"/><link rel="stylesheet" href="odoc.support/odoc.css"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><script src="odoc.support/highlight.pack.js"></script><script>hljs.initHighlightingOnLoad();</script><script>let base_url = '';
  let search_urls = ['db.js','sherlodoc.js'];
  </script><script src="odoc.support/odoc_search.js" defer="defer"></script></head><body class="odoc"><nav class="odoc-nav">OCaml package documentation</nav><div class="odoc-search"><div class="search-inner"><input class="search-bar" placeholder="🔎 Type '/' to search..."/><div class="search-snake"></div><div class="search-result"></div></div></div><header class="odoc-preamble"><h1 id="ocaml-package-documentation"><a href="#ocaml-package-documentation" class="anchor"></a>OCaml package documentation</h1><ul><li><a href="bar/index.html" title="index">bar</a></li><li><a href="foo/index.html" title="index">foo</a></li></ul></header><div class="odoc-tocs"><nav class="odoc-toc odoc-global-toc"><ul><li><a href="#" class="current_unit">OCaml package documentation</a><ul><li><a href="bar/index.html">bar index</a></li><li><a href="foo/index.html">foo index</a></li></ul></li></ul></nav></div><div class="odoc-content"></div></body></html>

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!/foo.byte/module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!/bar/module-Bar}.
