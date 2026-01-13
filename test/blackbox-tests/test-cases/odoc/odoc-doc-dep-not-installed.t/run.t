Test that documentation dependencies on non-existent packages produce a
sensible error message.

  $ dune build @doc
  Error: Documentation dependency "nonexistent-package-that-does-not-exist" is
  not installed.
  -> required by alias doc
  [1]
