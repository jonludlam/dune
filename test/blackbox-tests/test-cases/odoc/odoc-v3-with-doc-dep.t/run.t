The with-doc dependency's page tree resolves even though docdep is not a
build dependency of main:

  $ dune build @doc 2>&1 | grep -c "docdep"
  0
  [1]
