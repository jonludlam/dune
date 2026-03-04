Source rendering test - verify impl artifacts are compiled, linked, and generate source HTML:

  $ dune build @doc 2>&1 | head -20

Check that impl .odoc files are generated:

  $ find _build/default/_doc/_odoc -name 'impl-*' | sort
  _build/default/_doc/_odoc/mypkg/mypkg.mylib/impl-mylib.odoc

Check that impl .odocl files are generated:

  $ find _build/default/_doc/_odocl -name 'impl-*' | sort
  _build/default/_doc/_odocl/mypkg/mypkg.mylib/impl-mylib.odocl

Check that source HTML is generated (look for any source-related files):

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/mypkg/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/mylib.ml.html

Check that source links are present in the module documentation page:

  $ grep -o 'class="source_link">[^<]*' _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/index.html | sort -u
  class="source_link">Source

Check that source links point to the source file:

  $ grep -o 'href="[^"]*mylib.ml.html[^"]*" class="source_link"' _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/index.html | sort -u
  href="../mylib.ml.html" class="source_link"
  href="../mylib.ml.html#type-t" class="source_link"
  href="../mylib.ml.html#val-greet" class="source_link"
  href="../mylib.ml.html#val-hello" class="source_link"
