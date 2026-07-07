We create two libraries `l.one` and `l.two` with a conflicting module.
They build fine, are not co-linkable, but documentation should be able to be
built. See #1645.

  $ make_dune_project_with_package 1.0 l

  $ mkdir one
  $ cat > one/dune << EOF
  > (library
  >  (name l_one)
  >  (public_name l.one)
  >  (wrapped false))
  > EOF
  $ touch one/module.ml

  $ mkdir two
  $ cat > two/dune << EOF
  > (library
  >  (name l_two)
  >  (public_name l.two)
  >  (wrapped false))
  > EOF
  $ touch two/module.ml

  $ dune build @install
  $ dune build @doc

Both same-named modules get their own documentation page:

  $ find _build/default/_doc/_html/l -name 'index.html' | sort
  _build/default/_doc/_html/l/index.html
  _build/default/_doc/_html/l/l.one/Module/index.html
  _build/default/_doc/_html/l/l.two/Module/index.html
