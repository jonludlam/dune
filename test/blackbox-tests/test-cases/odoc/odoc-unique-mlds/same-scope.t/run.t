Duplicate mld's in the same scope
  $ dune build @doc
  Error: Package root has two mld's with the same name
  _build/default/lib1/test.mld, _build/default/lib2/test.mld
  -> required by _build/default/_doc/_odoc/root/page-index.odoc
  -> required by _build/default/_doc/_odocls/root/page-index.odocl
  -> required by _build/default/_doc/_html/root/index.html
  -> required by alias _doc/_html/root/doc
  -> required by alias doc
  [1]
