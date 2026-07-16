Documentation files nested in subdirectories of a package's
(documentation (files ...)) stanza (a hierarchical, non-flat, mld page or
asset) are compiled with a parent-id that includes the subdirectory, and
rendered at a nested URL, instead of being dropped.

Note: as with any install stanza, the destination ("in_doc" path) of a file
is its plain basename unless an explicit "as" clause gives it a nested
destination; that's what actually produces a non-flat hierarchy here.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (library (public_name foo) (name foo))
  > (documentation (package foo)
  >  (files index.mld (guide/intro.mld as guide/intro.mld) (guide/logo.png as guide/logo.png)))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = 1
  > EOF
  $ cat > index.mld <<EOF
  > {0 Foo}
  > See {!/foo/guide/intro}.
  > EOF
  $ mkdir -p guide
  $ cat > guide/intro.mld <<EOF
  > {0 Intro}
  > EOF
  $ printf 'PNG' > guide/logo.png

  $ dune build @doc

The nested page and the nested asset build at their nested URLs, alongside
the flat index.html. Before this change, dune dropped both entirely (with a
now-removed "non-flat hierarchy" warning) instead of building them at all:

  $ test -f _build/default/_doc/_html/foo/guide/intro.html && echo present
  present
  $ test -f _build/default/_doc/_html/foo/guide/logo.png && echo present
  present
  $ test -f _build/default/_doc/_html/foo/index.html && echo present
  present

The reference to the nested page from index.mld resolves (no
"unresolved reference"/"Failed to resolve" warning): the nested parent-id
makes the page's id [foo/guide/intro], matching what [{!/foo/guide/intro}]
looks up. This also proves the nested page's own [.odoc] file is visible to
the (sandboxed) link of the package's other, flat pages.

  $ dune build @doc 2>&1

Dune no longer warns that it does not support building documentation for a
non-flat hierarchy:

  $ dune build @doc 2>&1 | grep -ci "non-flat"
  0
  [1]

The nested page's content made it into the nested URL (not just an empty
placeholder), and the nested asset's content is copied verbatim:

  $ grep -c Intro _build/default/_doc/_html/foo/guide/intro.html
  1
  $ cat _build/default/_doc/_html/foo/guide/logo.png
  PNG

Two pages with the same basename in different subdirectories are not
treated as colliding (dedup keys on the full path, not just the basename):

  $ mkdir -p a b
  $ cat > a/page.mld <<EOF
  > {0 A}
  > EOF
  $ cat > b/page.mld <<EOF
  > {0 B}
  > EOF
  $ cat > dune <<EOF
  > (library (public_name foo) (name foo))
  > (documentation (package foo)
  >  (files index.mld (guide/intro.mld as guide/intro.mld) (guide/logo.png as guide/logo.png)
  >   (a/page.mld as a/page.mld) (b/page.mld as b/page.mld)))
  > EOF
  $ dune build @doc
  $ find _build/default/_doc/_html/foo/a _build/default/_doc/_html/foo/b -name '*.html' | sort
  _build/default/_doc/_html/foo/a/page.html
  _build/default/_doc/_html/foo/b/page.html
