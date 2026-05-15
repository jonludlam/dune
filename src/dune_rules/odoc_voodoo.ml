open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

(* Layout: a single shared --odoc-dir at _build/<ctx>/_doc/_voodoo/_odoc/.
   Each pkg's rule has directory_target at _odoc/p/<pkg>/<version>/.
   Voodoo's [extra_paths] scans the shared root for sibling marker files. *)

let root ctx = Path.Build.relative (Context.build_dir ctx) "_doc/_voodoo"
let odoc_root ctx = root ctx ++ "_odoc"

let pkg_odoc_subtree ctx ~pkg ~version =
  odoc_root ctx ++ "p" ++ pkg ++ version
;;

(* Sidecar file inside the pkg's voodoo subtree listing every
   [.odoc_pkg_marker] / [.odoc_lib_marker] under it (one path per
   line). Written as part of [compile_rule]'s action, so it always
   lives alongside the voodoo output. Read by [lp_flags] to discover
   what [-L] / [-P] arguments to emit. *)
let markers_file ctx ~pkg ~version =
  pkg_odoc_subtree ctx ~pkg ~version ++ ".markers"
;;

let program sctx ~dir =
  Super_context.resolve_program
    sctx
    ~dir
    ~where:Original_path
    ~loc:None
    ~hint:"opam install odoc-driver"
    "odoc_driver_voodoo"
;;

let opam_switch_prefix ctx =
  let+ env = Context.installed_env ctx in
  Env.get env Opam_switch.opam_switch_prefix_var_name
  |> Option.map ~f:(Path.External.parse_string_exn ~loc:Loc.none)
;;

let changes_file_path ~prefix pkg =
  Path.External.relative
    (Path.External.relative
       (Path.External.relative prefix ".opam-switch")
       "install")
    (pkg ^ ".changes")
;;

(* Parse <prefix>/.opam-switch/switch-state for the list of installed packages.
   Returns a map of pkg-name -> installed version. This is opam's
   authoritative source; the packages/<pkg>.<v>/ directory listing can
   contain stale entries from past installs (e.g. dune.3.21.0 alongside
   the current dune.3.21.1), so we don't rely on it. *)
let installed_versions =
  let entry_re = Re.compile (Re.Perl.re ~opts:[ `Multiline ] {|^\s*"([^."]+)\.([^"]+)"|}) in
  fun ~prefix ->
    let switch_state =
      Path.External.relative
        (Path.External.relative prefix ".opam-switch")
        "switch-state"
    in
    Fs_memo.file_contents (Path.Outside_build_dir.External switch_state)
    >>| fun contents ->
    Re.all entry_re contents
    |> List.fold_left ~init:String.Map.empty ~f:(fun acc m ->
      let name = Re.Group.get m 1 in
      let version = Re.Group.get m 2 in
      String.Map.set acc name version)
;;

let lookup_version ~prefix pkg =
  let+ map = installed_versions ~prefix in
  String.Map.find map pkg
;;

(* Find the opam package that provides the OCaml standard library, by
   scanning every [.changes] file in the switch for one whose [added:]
   list includes [lib/ocaml/stdlib.cmi]. The compiler distribution
   varies by switch (e.g. [ocaml-base-compiler] on a vanilla switch,
   [ocaml-variants] on an oxcaml switch), so we identify it by the
   stdlib it installs rather than by name. *)
let stdlib_provider =
  let stdlib_marker = Re.compile (Re.str {|"lib/ocaml/stdlib.cmi"|}) in
  fun ~prefix ->
    let install_dir =
      Path.External.relative
        (Path.External.relative prefix ".opam-switch")
        "install"
    in
    Fs_memo.dir_contents (Path.Outside_build_dir.External install_dir)
    >>= function
    | Error _ -> Memo.return None
    | Ok dir_contents ->
      let pkgs =
        Fs_memo.Dir_contents.to_list dir_contents
        |> List.filter_map ~f:(fun (name, _kind) ->
          String.drop_suffix name ~suffix:".changes")
      in
      Memo.parallel_map pkgs ~f:(fun pkg ->
        let changes_path = Path.External.relative install_dir (pkg ^ ".changes") in
        let+ contents =
          Fs_memo.file_contents (Path.Outside_build_dir.External changes_path)
        in
        if Re.execp stdlib_marker contents then Some pkg else None)
      >>| List.find_map ~f:Fun.id
;;

(* The set of opam packages we ask voodoo to document.

   Sources, unioned:
   - [(package (depends ...))] across every stanza in the workspace's
     [dune-project], minus workspace-local packages and deps tagged
     with [with-test] / [with-doc] / [with-dev-setup] / [post].
   - The opam package providing the OCaml standard library (so refs
     to [Stdlib] / [List] / etc. from local docs can resolve). We
     identify it by which [.changes] file installs the stdlib, since
     the name varies by switch.

   We do not chase transitive opam deps. If a package in the trigger
   set has refs to another switch package that isn't itself a
   dune-project dep, those refs will be unresolved in the voodoo
   compile artefacts (that's fine — they're only used to back the
   local lib's link, which has its own [-P] flag per trigger pkg). *)
let trigger_pkgs ctx =
  let* prefix_opt = opam_switch_prefix ctx in
  match prefix_opt with
  | None -> Memo.return []
  | Some prefix ->
    let* packages = Dune_load.packages () in
    let workspace_names = Package.Name.Map.keys packages |> Package.Name.Set.of_list in
    let is_excluded_by_filter (dep : Package_dependency.t) =
      Package_dependency.has_constraint_on Package_variable_name.with_test dep
      || Package_dependency.has_constraint_on Package_variable_name.with_doc dep
      || Package_dependency.has_constraint_on Package_variable_name.with_dev_setup dep
      || Package_dependency.has_constraint_on Package_variable_name.post dep
    in
    let dune_project_deps =
      Package.Name.Map.values packages
      |> List.concat_map ~f:(fun pkg ->
        Package.depends pkg
        |> List.filter_map ~f:(fun (dep : Package_dependency.t) ->
          if is_excluded_by_filter dep then None else Some dep.name))
      |> List.map ~f:Package.Name.to_string
    in
    let* stdlib_pkg = stdlib_provider ~prefix in
    let candidate_names =
      Option.to_list stdlib_pkg @ dune_project_deps
      |> List.filter ~f:(fun name ->
        not
          (Package.Name.Set.mem
             workspace_names
             (Package.Name.of_string name)))
      |> List.sort_uniq ~compare:String.compare
    in
    let lib_entry_re = Re.compile (Re.Perl.re ~opts:[ `Multiline ] {|^\s*"lib/|}) in
    Memo.parallel_map candidate_names ~f:(fun name ->
      let changes = changes_file_path ~prefix name in
      let* exists = Fs_memo.file_exists (Path.Outside_build_dir.External changes) in
      if not exists
      then Memo.return None
      else
        let* contents =
          Fs_memo.file_contents (Path.Outside_build_dir.External changes)
        in
        (* Skip meta-packages like [base-unix] (empty [.changes]) and
           [ocaml] (only installs [.opam-switch/config/...]). voodoo's
           [find_pkg] would error out with "No package found" on them. *)
        if not (Re.execp lib_entry_re contents)
        then Memo.return None
        else
          let+ v_opt = lookup_version ~prefix name in
          Option.map v_opt ~f:(fun v -> name, v))
    >>| List.filter_map ~f:Fun.id
;;

(* Tell odoc to rewrite refs into voodoo's switch-pkg tree to the
   canonical ocaml.org URLs. odoc's [-R <prefix>:<replacement>] does a
   literal string-prefix match on the rendered URL path; voodoo's pkg
   subtree always sits under [p/<pkg>/<version>/...], so a single rule
   covers every switch dep regardless of which packages are involved. *)
let remap_args _ctx : Command.Args.without_targets Command.Args.t Action_builder.t =
  Action_builder.return
    (Command.Args.S [ A "-R"; A "p/:https://ocaml.org/p/" ])
;;

(* Emit [-P pkg:<dir>] and [-L libname:<dir>] flags for [odoc link],
   one per marker discovered in the trigger pkgs' voodoo subtrees.

   Each marker's parent directory is the argument target. For [-L] the
   library name is that dir's basename (voodoo names lib subdirs after
   the dotted findlib name, e.g. [compiler-libs.bytecomp]). For [-P]
   the package name is two segments up (the [<pkg>] in
   [.../p/<pkg>/<ver>/doc/.odoc_pkg_marker]).

   Each marker dir is emitted as a [Command.Args.Path] so dune
   translates it relative to the [odoc link] invocation's cwd. *)
let lp_flags ctx : Command.Args.without_targets Command.Args.t Action_builder.t =
  let open Action_builder.O in
  let* pkg_versions = Action_builder.of_memo (trigger_pkgs ctx) in
  let+ per_pkg_args =
    Action_builder.all
      (List.map pkg_versions ~f:(fun (pkg, version) ->
         let markers = markers_file ctx ~pkg ~version in
         let+ lines = Action_builder.lines_of (Path.build markers) in
         List.concat_map lines ~f:(fun line ->
           let basename = Filename.basename line in
           (* [line] is a workspace-rooted path written by
              [Scan_markers] (i.e. starting [_build/<ctx>/...]).
              Parse with [Path.of_string] then narrow to a build
              path so dune translates it relative to the link's
              cwd. *)
           let dir =
             Filename.dirname line
             |> Path.of_string
             |> Path.as_in_build_dir
             |> Option.value_exn
           in
           match basename with
           | ".odoc_pkg_marker" ->
             let pkg_name =
               (* dir = .../p/<pkg>/<ver>/doc; we want <pkg>, which is
                  the basename two parents up. *)
               Path.Build.parent_exn dir
               |> Path.Build.parent_exn
               |> Path.Build.basename
             in
             [ Command.Args.A "-P"
             ; Concat (":", [ A pkg_name; Path (Path.build dir) ])
             ]
           | ".odoc_lib_marker" ->
             let libname = Path.Build.basename dir in
             [ Command.Args.A "-L"
             ; Concat (":", [ A libname; Path (Path.build dir) ])
             ]
           | _ -> [])))
  in
  Command.Args.S (List.concat per_pkg_args)
;;

(* Per-pkg working dir under which we build a voodoo-prep-shaped tree
   ([prep/universes/_/<pkg>/<version>/...]) by symlinking the switch's
   installed files for that pkg. voodoo's [find_pkg] walks [prep/]
   relative to cwd, so we [chdir] here when invoking [odoc_driver_voodoo]. *)
let prep_workdir ctx ~pkg ~version =
  root ctx ++ "_prep" ++ pkg ++ version
;;

(* Custom action that walks a built voodoo subtree and writes a
   list of [.odoc_pkg_marker] / [.odoc_lib_marker] paths (one per
   line, sorted) to a file. Used in place of [find | sort] so we
   don't shell out for marker discovery. *)
module Scan_markers = struct
  module Spec = struct
    type ('path, 'target) t = 'path * 'target

    let name = "odoc-voodoo-scan-markers"
    let version = 1
    let bimap (src, dst) f g = f src, g dst
    let is_useful_to ~memoize = memoize
    let encode (src, dst) path target : Sexp.t = List [ path src; target dst ]

    let action (src, dst) ~ectx:_ ~eenv:_ =
      let rec walk acc dir =
        match Sys.readdir dir with
        | exception Sys_error _ -> acc
        | entries ->
          Array.fold_left entries ~init:acc ~f:(fun acc name ->
            let p = Filename.concat dir name in
            if try Sys.is_directory p with Sys_error _ -> false
            then walk acc p
            else if
              String.equal name ".odoc_pkg_marker"
              || String.equal name ".odoc_lib_marker"
            then p :: acc
            else acc)
      in
      let paths =
        walk [] (Path.to_string src) |> List.sort ~compare:String.compare
      in
      Io.with_file_out (Path.build dst) ~f:(fun oc ->
        List.iter paths ~f:(fun p ->
          output_string oc p;
          output_char oc '\n'));
      Fiber.return ()
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action ~src ~dst : Action.t = A.action (src, dst)
end

(* Parse the [added:] entries of an opam [.changes] file. Skips
   directory entries (their digest is the literal string ["D"]) so
   we only symlink regular files and symlinks. The [opam-version]
   field is invalid in changes files (opam-format prints a warning);
   strip it before parsing. *)
let parse_changes_added contents =
  let contents =
    match String.lsplit2 contents ~on:'\n' with
    | Some (first, rest) when String.starts_with first ~prefix:"opam-version" ->
      rest
    | _ -> contents
  in
  let filename = OpamFile.make (OpamFilename.raw "<changes>") in
  let track = OpamFile.Changes.read_from_string ~filename contents in
  OpamStd.String.Map.fold
    (fun path change acc ->
       match change with
       | OpamDirTrack.Added digest
         when not (String.equal (OpamDirTrack.string_of_digest digest) "D") ->
         path :: acc
       | _ -> acc)
    track
    []
;;

let compile_rule sctx ~pkg_name ~version ~changes ~prefix =
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let shared_odoc = odoc_root ctx in
  let prog = program sctx ~dir:build_dir in
  let changes_path = Path.external_ changes in
  let workdir = prep_workdir ctx ~pkg:pkg_name ~version in
  let pkgroot_in_prep =
    workdir ++ "prep" ++ "universes" ++ "_" ++ pkg_name ++ version
  in
  let target = pkg_odoc_subtree ctx ~pkg:pkg_name ~version in
  let markers = markers_file ctx ~pkg:pkg_name ~version in
  let voodoo_html = root ctx ++ "_html" in
  (* Step 1: build a voodoo-prep-shaped symlink tree mirroring this
     pkg's installed files. voodoo's [find_pkg] walks the result of
     [Bos.OS.Dir.fold_contents] over [prep/]; that fold enumerates
     directories as well as files (with trailing slashes), which is
     what triggers the [non_meta_libraries] branch in [Voodoo.of_pkg]
     and ultimately produces e.g. [stdlib.odoc]. *)
  let* changes_contents =
    Fs_memo.file_contents (Path.Outside_build_dir.External changes)
  in
  let added_paths =
    parse_changes_added changes_contents
    (* Reject absolute paths (opam shouldn't emit them in [added:]). *)
    |> List.filter ~f:(fun p ->
      String.length p > 0 && not (Char.equal p.[0] '/'))
  in
  let symlink_actions, parent_dirs =
    List.fold_left
      added_paths
      ~init:([], Path.Build.Set.empty)
      ~f:(fun (links, parents) path ->
        let src = Path.external_ (Path.External.relative prefix path) in
        let dst = Path.Build.relative pkgroot_in_prep path in
        let parents =
          match Path.Build.parent dst with
          | Some p -> Path.Build.Set.add parents p
          | None -> parents
        in
        Action.symlink src dst :: links, parents)
  in
  let mkdir_actions =
    Path.Build.Set.to_list parent_dirs |> List.map ~f:Action.mkdir
  in
  let prep_action =
    Action.progn
      ((Action.remove_tree workdir :: mkdir_actions) @ symlink_actions)
  in
  (* Step 2: run [odoc_driver_voodoo] from [workdir] so it finds
     [prep/]. Use [--blessed] to land output at [p/<pkg>/<version>/]. *)
  let voodoo_args =
    [ Command.Args.A pkg_name
    ; A "--blessed"
    ; A "--actions"
    ; A "compile-only"
    ; A "--odoc-dir"
    ; Path (Path.build shared_odoc)
    ; A "--html-dir"
    ; Path (Path.build voodoo_html)
    ]
  in
  (* Step 3: scan the voodoo output for marker files and write the
     list into [.markers] (read by [lp_flags] to emit -P / -L). *)
  let scan_action = Scan_markers.action ~src:(Path.build target) ~dst:markers in
  let action =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.path changes_path)
    >>> Command.run_dyn_prog ~dir:(Path.build workdir) prog voodoo_args
    |> Action_builder.With_targets.map
         ~f:
           (Action.Full.map ~f:(fun voodoo_action ->
              Action.progn [ prep_action; voodoo_action; scan_action ]))
  in
  let action =
    Action_builder.With_targets.add_directories
      ~directory_targets:[ target ]
      action
  in
  Super_context.add_rule sctx ~dir:build_dir action
;;

let gen_rules sctx ~dir:_ ~pkg_name =
  let ctx = Super_context.context sctx in
  let* prefix_opt = opam_switch_prefix ctx in
  match prefix_opt with
  | None -> Memo.return Gen_rules.no_rules
  | Some prefix ->
    let changes = changes_file_path ~prefix pkg_name in
    let* exists = Fs_memo.file_exists (Path.Outside_build_dir.External changes) in
    if not exists
    then Memo.return Gen_rules.no_rules
    else
      let* version_opt = lookup_version ~prefix pkg_name in
      (match version_opt with
       | None -> Memo.return Gen_rules.no_rules
       | Some version ->
         let directory_targets =
           Path.Build.Map.singleton
             (pkg_odoc_subtree ctx ~pkg:pkg_name ~version)
             Loc.none
         in
         let rules =
           Rules.collect_unit (fun () ->
             compile_rule sctx ~pkg_name ~version ~changes ~prefix)
         in
         Memo.return (Gen_rules.make ~directory_targets rules))
;;
