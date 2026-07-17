A path reference to a sibling library's tree resolves via the -L
arguments at link time:

  $ dune build @doc 2>&1 | grep -c "Xref_b"
  0
  [1]
