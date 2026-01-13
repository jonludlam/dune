Test that documentation-only dependencies (not compilation dependencies) are
correctly handled. The library references {!Cmdliner.Arg.t} in doc comments
but does NOT have cmdliner in its (libraries ...) stanza.

Build documentation:

  $ dune build @doc

Check that documentation was generated:

  $ ls _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html
  _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html

Check that references to Cmdliner types are remapped to ocaml.org:

  $ grep -o 'href="[^"]*Cmdliner[^"]*"' _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html | sed 's|/[0-9.]*-*[a-z0-9]*/doc|/VERSION/doc|g' | sort -u
  href="https://ocaml.org/p/cmdliner/VERSION/doc/cmdliner/Cmdliner/Arg/index.html#type-t"
  href="https://ocaml.org/p/cmdliner/VERSION/doc/cmdliner/Cmdliner/Cmd/index.html#val-v"
  href="https://ocaml.org/p/cmdliner/VERSION/doc/cmdliner/Cmdliner/Term/index.html#type-t"

Verify there are no unresolved cross-references:

  $ grep -c "xref-unresolved" _build/default/_doc/_html/testpkg/testpkg.lib/Testlib/index.html || true
  0
