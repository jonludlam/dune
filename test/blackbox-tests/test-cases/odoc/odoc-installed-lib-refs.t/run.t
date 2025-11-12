Test that references to installed libraries like Lwt work correctly.
This verifies that cross-library references in documentation comments
are resolved correctly when linking.

Build documentation:

  $ dune build @doc

Check that documentation was generated without broken reference warnings:

  $ find _build/default/_doc/_odocls/mylib -name '*.odocl' | sort -n
  _build/default/_doc/_odocls/mylib/mylib/mylib.odocl
  _build/default/_doc/_odocls/mylib/page-index.odocl

Check that HTML was generated for our library:

  $ find _build/default/_doc/_html/mylib -name '*.html' | sort -n
  _build/default/_doc/_html/mylib/index.html
  _build/default/_doc/_html/mylib/mylib/Mylib/index.html

Verify that Lwt documentation was also built (needed for cross-references):

  $ ls _build/default/_doc/_odoc/lwt/lwt/Lwt.odoc
  _build/default/_doc/_odoc/lwt/lwt/Lwt.odoc

Check that the generated HTML contains links to Lwt types:

  $ grep -o "href=\"[^\"]*Lwt[^\"]*\"" _build/default/_doc/_html/mylib/mylib/Mylib/index.html | head -3
  href="../../../lwt/lwt/Lwt/index.html#type-t"
  href="../../../lwt/lwt/Lwt/index.html#type-t"
  href="../../../lwt/lwt/Lwt/index.html#type-t"

Verify that the linked Lwt HTML file actually exists:

  $ ls _build/default/_doc/_html/lwt/lwt/Lwt/index.html
  _build/default/_doc/_html/lwt/lwt/Lwt/index.html
