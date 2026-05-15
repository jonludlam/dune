Smoke test for the shared _voodoo/_odoc/p/<pkg>/<version> rule layout.
Validates that dune dispatches to Odoc_voodoo.gen_rules, builds a
voodoo-prep-shaped symlink tree mirroring the switch's installed
files for the requested package, and invokes
odoc_driver_voodoo <pkg> --blessed --actions=compile-only producing
the per-package output tree.

  $ dune build _build/default/_doc/_voodoo/_odoc/p/cmdliner/1.3.0 2>/dev/null

The compile-only run produces a partial-build marshal file plus the package
marker; both live inside the directory target.

  $ find _build/default/_doc/_voodoo/_odoc/p/cmdliner/1.3.0 -maxdepth 2 \( -name '__odoc_partial.m' -o -name '.odoc_pkg_marker' \) | sort
  _build/default/_doc/_voodoo/_odoc/p/cmdliner/1.3.0/__odoc_partial.m
  _build/default/_doc/_voodoo/_odoc/p/cmdliner/1.3.0/doc/.odoc_pkg_marker

The library .odoc file shows up under the cmdliner-named subdirectory.

  $ test -f _build/default/_doc/_voodoo/_odoc/p/cmdliner/1.3.0/doc/cmdliner/cmdliner.odoc

A pkg's rule is fired only when something requests its directory target;
we do not walk transitive opam deps, so building cmdliner does not also
build its dependencies (e.g. the ocaml meta-package).

  $ test ! -d _build/default/_doc/_voodoo/_odoc/p/ocaml

Requesting a nonexistent package fails naturally without invoking voodoo,
since no rules are emitted.

  $ dune build _build/default/_doc/_voodoo/_odoc/p/does-not-exist-pkg/1.0 2>&1 | head -5
  Error: Don't know how to build
  _build/default/_doc/_voodoo/_odoc/p/does-not-exist-pkg/1.0
  [1]
