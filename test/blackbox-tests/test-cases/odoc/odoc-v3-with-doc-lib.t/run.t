A with-doc dependency's module tree (-L) is in scope, so a module
reference into it resolves even though docdep is not a build dependency:

  $ dune build @doc 2>&1 | grep -c "docdep"
  0
  [1]
