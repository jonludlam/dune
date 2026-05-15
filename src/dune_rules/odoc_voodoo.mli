(** Rules for invoking [odoc_driver_voodoo] on opam-switch packages.

    Each switch-installed opam package gets its own dune rule, but all
    invocations share a single [--odoc-dir] at
    [_build/<ctx>/_doc/_voodoo/_odoc/]. Voodoo writes each package's output
    into [_odoc/p/<pkg>/<version>/], which is the rule's directory target.
    A shared root is necessary so voodoo's cross-package partial discovery
    (via [.odoc_pkg_marker] / [.odoc_lib_marker] files) can find sibling
    packages compiled by other rules.

    Per-package rules declare their direct opam-package deps (parsed from
    the package's [.opam] file, excluding [post] deps to avoid cycles) as
    build dependencies on those packages' directory targets, so dune fires
    them in topological order. *)

open Import

(** [_build/<ctx>/_doc/_voodoo] *)
val root : Context.t -> Path.Build.t

(** [_build/<ctx>/_doc/_voodoo/_odoc] — the shared [--odoc-dir]. *)
val odoc_root : Context.t -> Path.Build.t

(** [_odoc/p/<pkg>/<version>] — directory target of a single pkg's rule. *)
val pkg_odoc_subtree
  :  Context.t
  -> pkg:string
  -> version:string
  -> Path.Build.t

(** Emit [-P pkg:<dir>] and [-L libname:<dir>] flags for [odoc link],
    one per [.odoc_pkg_marker] / [.odoc_lib_marker] file found inside
    each trigger pkg's voodoo subtree. For pkg markers the package
    name is read from the path; for lib markers the library name is
    the marker dir's basename (voodoo uses the dotted findlib name,
    e.g. [compiler-libs.bytecomp]).

    Reads the per-pkg sidecar files produced by [gen_rules]' marker
    scan, so this declares each trigger pkg's voodoo dir as a build
    dep transitively. *)
val lp_flags
  :  Context.t
  -> Command.Args.without_targets Command.Args.t Action_builder.t

(** Emit a single ["-R p/:https://ocaml.org/p/"] flag for [odoc
    html-generate]. odoc's [--remap] does a literal string-prefix
    match on each rendered URL path; voodoo's switch-pkg subtree
    always lives at [p/<pkg>/<version>/...], so one rule rewrites
    every cross-package reference into a switch dep to its
    canonical ocaml.org URL. *)
val remap_args
  :  Context.t
  -> Command.Args.without_targets Command.Args.t Action_builder.t

(** Generate rules for [_doc/_voodoo/_odoc/p/<pkg_name>/...].

    Looks up [pkg_name]'s version in the switch's
    [.opam-switch/packages/<pkg_name>.<v>/], parses its opam file to
    determine direct deps, and emits a rule that runs [odoc_driver_voodoo]
    with appropriate build dependencies. Returns no rules if the package
    isn't installed in the switch. *)
val gen_rules
  :  Super_context.t
  -> dir:Path.Build.t
  -> pkg_name:string
  -> Build_config.Gen_rules.result Memo.t
