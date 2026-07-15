Each unit is compiled with --warnings-tag <package> and linked with
--warnings-tags <package>, so that a package's own doc warnings are shown
but warnings bubbling up from a dependency's documentation are filtered.

A single build is captured since a second `dune build @doc --verbose`
would be a no-op (nothing re-run, nothing to grep).

  $ dune build @doc --verbose > verbose.log 2>&1

  $ grep -oE -- "--warnings-tag [a-z_.]+" verbose.log | sort -u
  --warnings-tag dep
  --warnings-tag leak_dep
  --warnings-tag leak_user
  --warnings-tag user

  $ grep -oE -- "--warnings-tags [a-z_.]+" verbose.log | sort -u
  --warnings-tags dep
  --warnings-tags leak_dep
  --warnings-tags leak_user
  --warnings-tags user

Behavioural check: `leak_dep`'s interface has a reference that fails to
resolve, and `leak_user` re-exposes it verbatim via `include Leak_dep`, so
odoc re-encounters the same broken reference while expanding the include
for `leak_user`'s page. Without the tags, that expansion warning would
bubble up and be reported a second time, attributed to `leak_user`. With
the tags, `leak_dep` is compiled with `--warnings-tag leak_dep` and
`leak_user` is linked with `--warnings-tags leak_user` only, so the
`leak_dep`-tagged warning is filtered out of `leak_user`'s link and the
warning is reported exactly once, from `leak_dep` itself:

  $ grep -c "Failed to resolve reference" verbose.log
  1

  $ grep -c "expansion of include" verbose.log
  0
  [1]
