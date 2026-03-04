Test odoc prefix configuration via env stanza.
The prefix inserts path components between the type root and the package name,
e.g. _odoc/my/prefix/mypkg instead of _odoc/mypkg.

Setup a simple project:

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (package (name mylib))
  > EOF

  $ cat > mylib.opam <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkdir src

  $ cat > src/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > src/mylib.ml <<EOF
  > (** My library *)
  > let hello () = "hello"
  > EOF

Test 1: Without prefix (baseline)
==================================

  $ dune build @doc 2>&1 | { grep -v DEBUG || true; }

Check that odoc artifacts are in the standard path structure:

  $ find _build/default/_doc/_odoc/mylib -name '*.odoc' | sort
  _build/default/_doc/_odoc/mylib/mylib/impl-mylib.odoc
  _build/default/_doc/_odoc/mylib/mylib/mylib.odoc
  _build/default/_doc/_odoc/mylib/mylib/page-index.odoc
  _build/default/_doc/_odoc/mylib/page-index.odoc

  $ find _build/default/_doc/_html/mylib -name 'index.html' | sort
  _build/default/_doc/_html/mylib/index.html
  _build/default/_doc/_html/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/mylib/mylib/index.html

Test 2: With a multi-component prefix
======================================

  $ dune clean

Configure a multi-component prefix via dune-workspace env stanza:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (env
  >  (dev
  >   (odoc
  >    (prefix "my/universe"))))
  > EOF

  $ dune build @doc 2>&1 | { grep -v DEBUG || true; }

Check that odoc artifacts are under the prefix path:

  $ find _build/default/_doc/_odoc/my/universe/mylib -name '*.odoc' | sort
  _build/default/_doc/_odoc/my/universe/mylib/mylib/impl-mylib.odoc
  _build/default/_doc/_odoc/my/universe/mylib/mylib/mylib.odoc
  _build/default/_doc/_odoc/my/universe/mylib/mylib/page-index.odoc
  _build/default/_doc/_odoc/my/universe/mylib/page-index.odoc

Check that odocl files are under the prefix:

  $ find _build/default/_doc/_odocl/my/universe/mylib -name '*.odocl' | sort
  _build/default/_doc/_odocl/my/universe/mylib/mylib/impl-mylib.odocl
  _build/default/_doc/_odocl/my/universe/mylib/mylib/mylib.odocl
  _build/default/_doc/_odocl/my/universe/mylib/mylib/page-index.odocl
  _build/default/_doc/_odocl/my/universe/mylib/page-index.odocl

Check that HTML output is under the prefix:

  $ find _build/default/_doc/_html/my/universe/mylib -name 'index.html' | sort
  _build/default/_doc/_html/my/universe/mylib/index.html
  _build/default/_doc/_html/my/universe/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/my/universe/mylib/mylib/index.html

Check that generated mld files are under the prefix:

  $ find _build/default/_doc/_mlds/my/universe/mylib -name '*.mld' | sort
  _build/default/_doc/_mlds/my/universe/mylib/index.mld
  _build/default/_doc/_mlds/my/universe/mylib/mylib/index.mld

The old paths should NOT exist:

  $ test -d _build/default/_doc/_odoc/mylib && echo "ERROR: unprefixed path exists" || echo "OK: no unprefixed path"
  OK: no unprefixed path

Test 3: Single-component prefix
================================

  $ dune clean

  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > (env
  >  (dev
  >   (odoc
  >    (prefix "universe"))))
  > EOF

  $ dune build @doc 2>&1 | { grep -v DEBUG || true; }

  $ find _build/default/_doc/_odoc/universe/mylib -name '*.odoc' | sort
  _build/default/_doc/_odoc/universe/mylib/mylib/impl-mylib.odoc
  _build/default/_doc/_odoc/universe/mylib/mylib/mylib.odoc
  _build/default/_doc/_odoc/universe/mylib/mylib/page-index.odoc
  _build/default/_doc/_odoc/universe/mylib/page-index.odoc

  $ find _build/default/_doc/_html/universe/mylib -name 'index.html' | sort
  _build/default/_doc/_html/universe/mylib/index.html
  _build/default/_doc/_html/universe/mylib/mylib/Mylib/index.html
  _build/default/_doc/_html/universe/mylib/mylib/index.html
