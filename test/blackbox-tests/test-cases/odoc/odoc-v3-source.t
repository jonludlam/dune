Source rendering ((env (odoc (source_rendering enabled))))) compiles a
library module's implementation with [odoc compile-impl], links it, and
renders hyperlinked source HTML with [odoc html-generate-source].

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > (** Foo. *)
  > let x = 1
  > EOF

By default (no env stanza), source rendering is disabled: @doc builds the
usual module documentation, with no impl odocs and no source HTML.

  $ cat > dune <<EOF
  > (library (public_name foo) (name foo))
  > EOF
  $ dune build @doc
  $ find _build/default/_doc -iname 'impl-*'
  $ find _build/default/_doc/_html/foo -name '*.ml.html'
  $ test -f _build/default/_doc/_html/foo/foo/Foo/index.html && echo present
  present

Opting in with (env (odoc (source_rendering enabled))) produces a source
HTML page for the implementation:

  $ cat > dune <<EOF
  > (env (_ (odoc (source_rendering enabled))))
  > (library (public_name foo) (name foo))
  > EOF
  $ dune build @doc
  $ find _build/default/_doc/_html/foo -name '*.ml.html' | sort
  _build/default/_doc/_html/foo/src/foo/foo.ml.html

The page is real hyperlinked source, not a placeholder: it highlights the
`.ml`'s tokens and anchors the top-level binding.

  $ grep -c 'id="val-x"' _build/default/_doc/_html/foo/src/foo/foo.ml.html
  1

Every implementation compilation unit is rendered, not just a wrapped
library's entry module: each sub-module gets its own source page.

  $ mkdir -p sub
  $ cat > sub/dune <<EOF2
  > (env (_ (odoc (source_rendering enabled))))
  > (library (public_name foo.sub) (name subl))
  > EOF2
  $ cat > sub/aa.ml <<EOF2
  > let a = 1
  > EOF2
  $ cat > sub/bb.ml <<EOF2
  > let b = 2
  > EOF2
  $ dune build @doc
  $ find _build/default/_doc/_html -name 'aa.ml.html' -o -name 'bb.ml.html' | sort
  _build/default/_doc/_html/foo/src/foo.sub/aa.ml.html
  _build/default/_doc/_html/foo/src/foo.sub/bb.ml.html
