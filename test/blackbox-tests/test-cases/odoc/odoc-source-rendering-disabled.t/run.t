Source rendering disabled test - verify impl artifacts are NOT generated:

  $ dune build @doc 2>&1 | head -20

Check that NO impl .odoc files are generated:

  $ find _build/default/_doc/_odoc -name 'impl-*' 2>/dev/null | sort

Check that NO impl .odocl files are generated:

  $ find _build/default/_doc/_odocl -name 'impl-*' 2>/dev/null | sort

Check that NO source HTML is generated (no .ml.html files):

  $ find _build/default/_doc/_html -name '*.ml.html' 2>/dev/null | sort

Module documentation should still be generated:

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/index.html
  _build/default/_doc/_html/mypkg/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/Mylib/index.html
  _build/default/_doc/_html/mypkg/mypkg.mylib/index.html
