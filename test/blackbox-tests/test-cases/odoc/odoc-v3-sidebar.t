A package gets a navigation sidebar for @doc: dune builds a .odoc-index from
the package's page and module odocls with [odoc compile-index], turns it
into a .odoc-sidebar with [odoc sidebar-generate], and passes --sidebar to
every html-generate. A packageless (private) library gets its own sidebar,
built the same way but scoped to its own modules.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (library (public_name foo) (name foo))
  > (documentation (package foo) (files index.mld))
  > EOF
  $ cat > foo.ml <<EOF
  > (** Foo. *)
  > let x = 1
  > EOF
  $ cat > index.mld <<EOF
  > {0 Foo}
  > See {!Foo}.
  > EOF
  $ mkdir priv
  $ cat > priv/dune <<EOF
  > (library (name priv))
  > EOF
  $ cat > priv/priv.ml <<EOF
  > (** Priv. *)
  > let y = 1
  > EOF

  $ dune build @doc @doc-private

The package's sidebar artifacts are built in its _odoc dir, indexing its
page and its library's module odocls (not assets or impls -- same filter as
the search db):

  $ test -f _build/default/_doc/_odoc/pkg/foo/index.odoc-index && echo present
  present
  $ test -f _build/default/_doc/_odoc/pkg/foo/sidebar.odoc-sidebar && echo present
  present

The sidebar reconstructs the package's module tree, not just the index page:
the package index's navigation links to the library's module.

  $ grep -o 'href="foo/Foo/index.html"' _build/default/_doc/_html/foo/index.html | head -1
  href="foo/Foo/index.html"

The package's page and module htmls both gain a global-toc navigation
element rendered from the sidebar (odoc's [--sidebar] populates the
[odoc-tocs]/[odoc-global-toc] navigation; without [--sidebar] this element
is absent -- see the before/after check below):

  $ grep -qc 'odoc-global-toc' _build/default/_doc/_html/foo/index.html
  $ grep -qc 'odoc-global-toc' _build/default/_doc/_html/foo/foo/Foo/index.html

Before/after: a bare [odoc html-generate] on the same page odocl, without
--sidebar, has no such navigation element:

  $ ODOCL=$(find _build/default/_doc/_odocls/foo -name page-index.odocl)
  $ mkdir sidebar-before-after
  $ odoc html-generate -o sidebar-before-after $PWD/$ODOCL
  $ grep -c 'odoc-global-toc' sidebar-before-after/foo/index.html
  0
  [1]
  $ rm -rf sidebar-before-after

A packageless (private) library is its own navigation scope: it gets its
own sidebar, built in its own object directory (not the package's), and its
module html also gains the global-toc navigation element:

  $ find _build/default -name '*.odoc-sidebar' | sort
  _build/default/_doc/_odoc/pkg/foo/sidebar.odoc-sidebar
  _build/default/priv/.priv.objs/byte/sidebar.odoc-sidebar
  $ PRIV_HTML=$(find _build/default/_doc/_html -path '*/Priv/index.html')
  $ grep -qc 'odoc-global-toc' "$PRIV_HTML"
