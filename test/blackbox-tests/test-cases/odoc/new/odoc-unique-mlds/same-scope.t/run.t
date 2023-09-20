Duplicate mld's in the same scope
  $ dune build @doc-new
  Error: Package root has two mld files with the same basename
  _build/default/lib1/test.mld, _build/default/lib2/test.mld
  -> required by alias _doc_new/html/docs/doc-new
  -> required by alias doc-new
  [1]
