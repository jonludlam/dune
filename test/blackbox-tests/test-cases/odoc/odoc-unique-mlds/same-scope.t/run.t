Duplicate mld's in the same scope
  $ dune build @doc
  Error: Multiple rules generated for _build/default/_doc/_html/root/test.html:
  - <internal location>
  - <internal location>
  -> required by alias doc
  Error: Multiple rules generated for
  _build/default/_doc/_odoc/root/page-test.odoc:
  - <internal location>
  - <internal location>
  -> required by _build/default/_doc/_odocls/page-index.odocl
  -> required by _build/default/_doc/_html/db.js
  -> required by _build/default/_doc/_html/index.html
  -> required by alias _doc/_html/doc
  -> required by alias doc
  Error: Multiple rules generated for
  _build/default/_doc/_odocls/root/page-test.odocl:
  - <internal location>
  - <internal location>
  -> required by _build/default/_doc/_sidebar/index.odoc-index
  -> required by _build/default/_doc/_sidebar/sidebar.odoc-sidebar
  -> required by _build/default/_doc/_html/index.html
  -> required by alias _doc/_html/doc
  -> required by alias doc
  [1]
