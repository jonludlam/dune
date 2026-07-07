A single package containing two libraries with a dependency edge between
them: library "b" depends on library "a" and documents a cross-reference to
a type defined in "a". Building the docs must resolve the cross-library
reference.

  $ dune build @doc

The cross-reference from B to A resolves to A's page:

  $ grep -o '<a href="[^"]*">A.t</a>' _build/default/_doc/_html/p/p.b/B/index.html | head -1
  <a href="../../p.a/A/index.html#type-t">A.t</a>
