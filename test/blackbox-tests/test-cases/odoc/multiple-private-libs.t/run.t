This test checks that there is no clash when two private libraries have the same name

  $ dune build --display short @doc-private
          odoc _doc/_html/highlight.pack.js,_doc/_html/odoc.css
  File "_doc/_html/test@6aabb9861046/Test/_unknown_", line 1, characters 0-0:
  Error: No rule found for _doc/_odocls/test@6aabb9861046/test.odocl
  File "_doc/_html/test@ea8c79305c05/Test/_unknown_", line 1, characters 0-0:
  Error: No rule found for _doc/_odocls/test@ea8c79305c05/test.odocl
  [1]
