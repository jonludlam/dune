A non-.mld file in a package's (documentation (files ...)) stanza is an
asset: dune compiles it with [odoc compile-asset], links it, and copies it
into the package's HTML output with [odoc html-generate-asset].

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (library (public_name foo) (name foo))
  > (documentation (package foo) (files index.mld logo.png))
  > EOF
  $ cat > foo.ml <<EOF
  > (** Foo. *)
  > let x = 1
  > EOF
  $ cat > index.mld <<EOF
  > {0 Foo}
  > EOF
  $ printf 'PNGDATA' > logo.png

  $ dune build @doc

The asset is copied into the package's HTML output:

  $ test -f _build/default/_doc/_html/foo/logo.png && echo present
  present
  $ cat _build/default/_doc/_html/foo/logo.png
  PNGDATA

Dune no longer warns that it does not support building documentation for
assets:

  $ dune build @doc 2>&1 | grep -ci asset
  0
  [1]

@doc-json also builds, reusing the same generated file rather than
generating it a second time (no duplicate-rule error):

  $ dune build @doc-json

Two entries aliased to the same asset name would compile to the same
asset-<name>.odoc target. Dune reports a friendly error naming the two
colliding files rather than crashing with an internal "multiple rules"
error:

  $ cat > dune <<EOF
  > (library (public_name foo) (name foo))
  > (documentation (package foo) (files (a.png as logo.png) (b.png as logo.png)))
  > EOF
  $ printf 'AAA' > a.png
  $ printf 'BBB' > b.png
  $ rm logo.png
  $ dune build @doc
  Error: Package foo has two assets with the same name _build/default/b.png,
  _build/default/a.png
  -> required by alias doc
  [1]
