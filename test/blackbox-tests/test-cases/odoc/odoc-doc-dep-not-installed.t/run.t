Test that documentation dependencies on non-existent packages produce a
sensible error message.

  $ dune build @doc
  Error: Documentation dependency "nonexistent-package-that-does-not-exist" is
  not installed.
  -> required by _build/default/_doc/_html/db.js
  -> required by _build/default/_doc/_html/index.html
  -> required by alias _doc/_html/doc
  -> required by alias doc
  [1]
