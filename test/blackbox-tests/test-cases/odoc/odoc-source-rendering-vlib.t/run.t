Source rendering for virtual library implementations.
The virtual library provides the documentation (.cmti) while the implementation
library provides the source code (.cmt). Source links in the vlib's docs should
point to the implementation's source.

  $ dune build @doc 2>&1 | head -20

Check that impl .odoc files are generated for the implementation library:

  $ find _build/default/_doc/_odoc -name 'impl-*' | sort
  _build/default/_doc/_odoc/mypkg/mypkg.mylib/impl-mylib.odoc
  _build/default/_doc/_odoc/mypkg/mypkg.mylib/impl-mylib__Mymod.odoc
  _build/default/_doc/_odoc/mypkg/mypkg.mylib/impl-mylib__mylib_impl__.odoc

Check that impl .odocl files are generated:

  $ find _build/default/_doc/_odocl -name 'impl-*' | sort
  _build/default/_doc/_odocl/mypkg/mypkg.mylib/impl-mylib.odocl
  _build/default/_doc/_odocl/mypkg/mypkg.mylib/impl-mylib__Mymod.odocl
  _build/default/_doc/_odocl/mypkg/mypkg.mylib/impl-mylib__mylib_impl__.odocl

Check that source HTML is generated:

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/mypkg/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/Mymod/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/mylib.ml-gen.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/mylib__mylib_impl__.ml-gen.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/mymod.ml.html

Check that mymod.ml.html source file exists:

  $ test -f _build/default/_doc/_html/mypkg/mypkg.mylib/mymod.ml.html && echo "exists" || echo "missing"
  exists

Check that source links are present in the virtual library's module documentation:

  $ grep -o 'class="source_link">[^<]*' _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/Mymod/index.html | sort -u
  class="source_link">Source

Check that source links point to the implementation's source file:

  $ grep -o 'href="[^"]*mymod.ml.html[^"]*" class="source_link"' _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/Mymod/index.html | sort -u
  href="../../mymod.ml.html" class="source_link"
  href="../../mymod.ml.html#type-t" class="source_link"
  href="../../mymod.ml.html#val-create" class="source_link"
  href="../../mymod.ml.html#val-greet" class="source_link"
