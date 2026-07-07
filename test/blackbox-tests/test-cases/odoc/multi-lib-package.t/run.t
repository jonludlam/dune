A single package containing two libraries with a dependency edge between
them: library "b" depends on library "a" and documents a cross-reference to
a type defined in "a". Both libraries share the package's _doc/_odoc
directory, so building the docs must not deadlock on a self-referential
include, and the cross-reference must resolve.

  $ dune build @doc

The cross-reference from B to A resolves to A's page:

  $ grep -o '<a href="[^"]*">A.t</a>' _build/default/_doc/_html/p/B/index.html | head -1
  <a href="../A/index.html#type-t">A.t</a>
