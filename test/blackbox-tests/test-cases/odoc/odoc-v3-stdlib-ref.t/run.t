References to Stdlib resolve against the stdlib odoc files installed in
the switch by odd:

  $ dune build @doc 2>&1 | grep -c "Stdlib"
  0
  [1]
