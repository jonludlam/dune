A module whose name contains a double underscore (e.g. [Eio__core], the
wrapper of a library named [eio__core]) is hidden by odoc convention: it must
not get a standalone html page, must not be listed in the sidebar/index, and
must not be linked from the package index. It is still compiled and linked,
since a visible library's expansion may reference it (e.g. [Eio] does
[-open Eio__core]).

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (library (public_name foo.core) (name foo__core) (modules core))
  > (library (public_name foo) (name foo) (modules foo) (libraries foo.core)
  >  (flags (:standard -open Foo__core)))
  > EOF
  $ cat > core.ml <<EOF
  > (** Something in the hidden library. *)
  > let x = 1
  > EOF
  $ cat > foo.ml <<EOF
  > (** The foo library. *)
  > let y = 1
  > EOF

  $ dune build @doc

[Foo__core]'s standalone html is not generated:

  $ test ! -e _build/default/_doc/_html/foo/foo.core/Foo__core && echo absent
  absent

[Foo__core] is not linked from the package index, and does not appear in
its sidebar/global-toc navigation:

  $ grep -c 'Foo__core' _build/default/_doc/_html/foo/index.html
  0
  [1]

The visible library [foo] is still documented -- proving we did not
over-filter, and that [foo.core] stays linked (building [foo]'s page requires
[foo__core.odocl], since [foo]'s expansion opens [Foo__core]):

  $ test -e _build/default/_doc/_html/foo/foo/Foo/index.html && echo present
  present

The same rule applies to markdown output: [Foo__core] gets no markdown page,
while the visible [Foo] does:

  $ dune build @doc-markdown
  $ find _build/default/_doc/_markdown -iname '*Foo__core*'
  $ test -e _build/default/_doc/_markdown/foo/foo/Foo.md && echo present
  present
