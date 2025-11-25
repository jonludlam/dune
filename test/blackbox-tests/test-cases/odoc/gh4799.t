Private libraries attached to packages shouldn't be displayed in the index

  $ cat <<EOF > dune-project
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ cat <<EOF > dune
  > (library
  >  (name foo)
  >  (package foo))
  > EOF
  > touch foo.ml bar.ml

  $ dune build @doc
  File "_doc/_mlds/foo/foo/index.mld", line 4, characters 0-25:
  Warning: Failed to resolve reference unresolvedroot(Bar) Parent_module: Lookup failure (root module): Bar
  $ cat _build/default/_doc/_mlds/foo/foo/index.mld
  @toc_status hidden
  @order_category libraries
  {0 Library [foo]}
  {!modules: Bar Foo Foo__}
  $ cat _build/default/_doc/_mlds/foo/index.mld
  {0 foo index}
