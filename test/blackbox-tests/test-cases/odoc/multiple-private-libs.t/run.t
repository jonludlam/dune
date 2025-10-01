This test checks that there is no clash when two private libraries have the same name

  $ dune build @doc-private
  File "_doc/_odocls/test@38f98c954b37/_unknown_", line 1, characters 0-0:
  Error: No rule found for alias a/.test.objs/byte/.odoc-all
  File "_doc/_odocls/test@38f98c954b37/_unknown_", line 1, characters 0-0:
  Error: No rule found for a/.test.objs/byte/test.odoc
  File "_doc/_odocls/test@a34afd669310/_unknown_", line 1, characters 0-0:
  Error: No rule found for alias b/.test.objs/byte/.odoc-all
  File "_doc/_odocls/test@a34afd669310/_unknown_", line 1, characters 0-0:
  Error: No rule found for b/.test.objs/byte/test.odoc
  [1]
