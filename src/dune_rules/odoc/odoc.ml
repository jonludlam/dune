open Import
open Memo.O
open Odoc_scope
open Odoc_target
open Odoc_paths
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

let add_rule sctx =
  let dir = Super_context.context sctx |> Context.build_dir in
  Super_context.add_rule sctx ~dir
;;

module Dep : sig
  (** [format_alias output ctx target] returns the alias that depends on all
      targets produced by odoc for [target] in output format [output]. *)
  val format_alias : Output_format.t -> Context.t -> 'a Odoc_target.t -> Alias.t
end = struct
  let format_alias : type a. Output_format.t -> Context.t -> a Odoc_target.t -> Alias.t =
    fun f ctx m -> Output_format.alias f ~dir:(Paths.output ctx f m)
  ;;
end

(* The [(documentation (files ...))] stanza allows with the [as] keyword to
   distinguish the input file and the page name, so a page artifact's name may
   differ from its source file name. *)
let page_artifact pkg ~path ~name =
  Odoc_artifact.make ~source:path { Odoc_target.name } (Pkg pkg)
;;

module Flags = struct
  type warnings = Dune_env.Odoc.warnings =
    | Fatal
    | Nonfatal

  type t = { warnings : warnings }

  let default = { warnings = Nonfatal }

  let get ~dir =
    Env_stanza_db.value ~default ~dir ~f:(fun config ->
      match config.odoc.warnings with
      | None -> Memo.return None
      | Some warnings -> Memo.return (Some { warnings }))
    |> Action_builder.of_memo
  ;;
end

let odoc_base_flags quiet build_dir =
  let open Action_builder.O in
  let+ conf = Flags.get ~dir:build_dir in
  match conf.warnings with
  | Fatal ->
    (* if quiet has been passed, we're running odoc on an external
       artifact (e.g. stdlib.cmti) - so no point in warn-error *)
    if quiet then Command.Args.S [] else A "--warn-error"
  | Nonfatal -> S []
;;

let odoc_dev_tool_exe_path_building_if_necessary () =
  let open Action_builder.O in
  let path = Path.build (Pkg_dev_tool.exe_path Odoc) in
  let+ () = Action_builder.path path in
  Ok path
;;

let odoc_program sctx dir =
  let open Action_builder.O in
  let* lock_dir_exists =
    Action_builder.of_memo
      (match Config.get Compile_time.lock_dev_tools with
       | `Enabled -> Memo.return true
       | `Disabled ->
         (* even if lock_dev_tools is disabled, there might be a lock dir
            created by `dune tools install odoc` *)
         let path = Lock_dir.dev_tool_external_lock_dir Odoc in
         Fs_memo.dir_exists (Path.Outside_build_dir.External path))
  in
  match lock_dir_exists with
  | true -> odoc_dev_tool_exe_path_building_if_necessary ()
  | false ->
    Super_context.resolve_program
      sctx
      ~dir
      ~where:Original_path
      "odoc"
      ~loc:None
      ~hint:"opam install odoc"
;;

let run_odoc sctx ~dir command ~quiet ~flags_for args =
  let build_dir = Super_context.context sctx |> Context.build_dir in
  let program = odoc_program sctx build_dir in
  let base_flags =
    let open Action_builder.O in
    let* () = Action_builder.return () in
    match flags_for with
    | None -> Action_builder.return Command.Args.empty
    | Some path -> odoc_base_flags quiet path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets deps
  >>> Command.run_dyn_prog
        ~dir
        ~sandbox:Sandbox_config.needs_sandboxing
        program
        [ A command; Dyn base_flags; S args ]
;;

let parse_odoc_deps lines =
  List.filter_map lines ~f:(fun line ->
    match String.split ~on:' ' line with
    | [ m; _hash ] -> Some (Module_name.of_checked_string m)
    | _ -> None)
;;

(* The documentation target of a local library: public libraries live under
   their package, private libraries under their unique name. *)
let lib_target (lib : Lib.Local.t) : Odoc_target.mod_ Odoc_target.t =
  match Lib_info.package (Lib.Local.info lib) with
  | Some pkg -> Lib (pkg, lib)
  | None -> Private_lib (lib_unique_name (Lib.Local.to_lib lib), lib)
;;

let mode_of_lib (lib : Lib.Local.t) =
  Lib_info.modes (Lib.Local.info lib)
  |> Compilation_mode.Set.of_lib_mode_set
  |> Compilation_mode.Set.for_merlin
;;

let module_artifact target ~obj_dir ~mode (m : Module.t) =
  let cmti =
    Obj_dir.Module.cmti_file
      ~cm_kind:
        (match mode with
         | Compilation_mode.Ocaml -> Ocaml Cmi
         | Melange -> Melange Cmi)
      obj_dir
      m
  in
  Odoc_artifact.make
    ~source:cmti
    { Odoc_target.module_name =
        Module_name.Unique.to_name (Module.obj_name m) ~loc:Loc.none
    }
    target
;;

(* All module .odoc files of a local library. Declared as explicit file
   dependencies (rather than a directory glob) so that odoc can resolve
   references under sandboxing while never depending on the compiling library's
   own directory contents. *)
let lib_module_artifacts sctx lib =
  let target = lib_target lib in
  let obj_dir = Lib_info.obj_dir (Lib.Local.info lib) in
  let mode = mode_of_lib lib in
  let+ modules = Dir_contents.modules_of_local_lib sctx lib ~for_:mode in
  Modules.fold modules ~init:[] ~f:(fun m acc ->
    module_artifact target ~obj_dir ~mode m :: acc)
;;

(* [odoc_file_by_module] maps a module name to the path of that module's
   compiled .odoc file. It is used to translate the module names reported by
   [odoc compile-deps] into the .odoc files they refer to, so those files can
   be declared as build dependencies of the module being compiled. *)
let module_deps sctx artifact ~obj_dir ~odoc_file_by_module =
  let ctx = Super_context.context sctx in
  let self =
    match Odoc_artifact.get_kind artifact with
    | Module (mod_, _) -> mod_.Odoc_target.module_name
    | Page _ -> Code_error.raise "Odoc.module_deps: not a module artifact" []
  in
  let cmti = Odoc_artifact.source_file artifact in
  let deps_output =
    let open Action_builder.O in
    (let* odoc = odoc_program sctx (Context.build_dir ctx) in
     Command.run'
       odoc
       ~sandbox:Sandbox_config.needs_sandboxing
       ~dir:(Path.build (Obj_dir.odoc_dir obj_dir))
       [ A "compile-deps"; Dep (Path.build cmti) ])
    |> Super_context.execute_action_stdout
         sctx
         ~loc:Loc.none
         ~dir:(Obj_dir.odoc_dir obj_dir)
    |> Action_builder.of_memo
  in
  let open Action_builder.O in
  let* lines = deps_output >>| String.split_lines in
  let deps =
    parse_odoc_deps lines
    |> List.filter_map ~f:(fun dep_mod ->
      if Module_name.equal dep_mod self
      then None
      else Module_name.Map.find odoc_file_by_module dep_mod)
  in
  Dune_engine.Dep.Set.of_files deps |> Action_builder.deps
;;

let compile_module
      sctx
      ~obj_dir
      artifact
      ~includes:(file_deps, iflags)
      ~odoc_file_by_module
  =
  let ctx = Super_context.context sctx in
  let odoc_file = Odoc_artifact.odoc_file ctx artifact in
  let cmti = Odoc_artifact.source_file artifact in
  let odoc_dir = Odoc_artifact.odoc_dir ctx artifact in
  let odoc_root = Paths.odoc_root ctx in
  let parent_id = Path.reach (Path.build odoc_dir) ~from:(Path.build odoc_root) in
  let+ () =
    let action_with_targets =
      let run_odoc =
        run_odoc
          sctx
          ~dir:(Path.build odoc_root)
          "compile"
          ~quiet:false
          ~flags_for:(Some cmti)
          [ A "-I"
          ; Path (Path.build odoc_dir)
          ; iflags
          ; A "--output-dir"
          ; Path (Path.build odoc_root)
          ; A "--parent-id"
          ; A parent_id
          ; Hidden_targets [ odoc_file ]
          ; Dep (Path.build cmti)
          ]
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets
            (module_deps sctx artifact ~obj_dir ~odoc_file_by_module)
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  odoc_file
;;

let compile_page sctx artifact ~includes ~pkg =
  let ctx = Super_context.context sctx in
  let odoc_file = Odoc_artifact.odoc_file ctx artifact in
  let odoc_input = Odoc_artifact.source_file artifact in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build (Odoc_artifact.odoc_dir ctx artifact))
      "compile"
      ~quiet:false
      ~flags_for:(Some odoc_input)
      [ Command.Args.dyn includes
      ; As [ "--pkg"; Package.Name.to_string pkg ]
      ; A "-o"
      ; Target odoc_file
      ; Dep (Path.build odoc_input)
      ]
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file
;;

let odoc_include_flags ctx pkg requires =
  Resolve.args
    (let open Resolve.O in
     let+ paths =
       let+ libs = requires in
       let paths =
         List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
           match Lib.Local.of_lib lib with
           | None -> paths
           | Some lib ->
             Path.Set.add paths (Path.build (Paths.odocs ctx (lib_target lib))))
       in
       let paths =
         match pkg with
         | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
         | None -> paths
       in
       Path.Set.to_list paths
     in
     Command.Args.S
       (List.concat_map paths ~f:(fun dir -> [ Command.Args.A "-I"; Path dir ])))
;;

(* All module .odoc files of the given local libraries. Declared as explicit
   file dependencies (rather than a directory glob) so that odoc can resolve
   references under sandboxing while never depending on the compiling library's
   own directory contents. *)
let lib_odoc_files sctx libs =
  let ctx = Super_context.context sctx in
  Memo.List.concat_map libs ~f:(fun lib ->
    let+ artifacts = lib_module_artifacts sctx lib in
    List.map artifacts ~f:(fun a -> Path.build (Odoc_artifact.odoc_file ctx a)))
;;

let setup_library_odoc_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t

    let equal (sc1, l1) (sc2, l2) = Super_context.equal sc1 sc2 && Lib.Local.equal l1 l2
    let hash (sc, l) = Tuple.T2.hash Super_context.hash Lib.Local.hash (sc, l)
    let to_dyn _ = Dyn.Opaque
  end
  in
  let f (sctx, local_lib) =
    let ctx = Super_context.context sctx in
    let info = Lib.Local.info local_lib in
    let obj_dir = Lib_info.obj_dir info in
    let mode = mode_of_lib local_lib in
    let* module_artifacts = lib_module_artifacts sctx local_lib in
    let lib = Lib.Local.to_lib local_lib in
    let package = Lib_info.package info in
    let* includes =
      let* closure = Lib.closure [ lib ] ~linking:false ~for_:mode in
      let not_self l = not (Lib.equal l lib) in
      let include_libs = Resolve.map closure ~f:(List.filter ~f:not_self) in
      let* dep_libs =
        let+ libs = Resolve.read_memo closure in
        List.filter_map libs ~f:Lib.Local.of_lib
        |> List.filter ~f:(fun l -> not (Lib.Local.equal l local_lib))
      in
      (* Cross-library edges are resolved via -I plus explicit file deps on the
         dependency libraries' .odoc files. Intra-library edges come from
         [module_deps] (compile-deps) below. No directory glob is used, so this
         is correct under sandboxing and never depends on this library's own
         .odoc -- even now that libraries share the package's _odoc directory. *)
      let file_deps =
        let open Action_builder.O in
        let* dep_odocs = Action_builder.of_memo (lib_odoc_files sctx dep_libs) in
        Action_builder.deps (Dune_engine.Dep.Set.of_files dep_odocs)
      in
      Memo.return
        (file_deps, Command.Args.memo (odoc_include_flags ctx package include_libs))
    in
    let odoc_file_by_module =
      List.fold_left module_artifacts ~init:Module_name.Map.empty ~f:(fun acc a ->
        match Odoc_artifact.get_kind a with
        | Module (mod_, _) ->
          Module_name.Map.set
            acc
            mod_.Odoc_target.module_name
            (Path.build (Odoc_artifact.odoc_file ctx a))
        | Page _ -> acc)
    in
    module_artifacts
    |> List.map ~f:(fun artifact ->
      compile_module sctx ~includes ~obj_dir artifact ~odoc_file_by_module)
    |> Memo.all_concurrently
    >>| (ignore : Path.Build.t list -> unit)
  in
  Memo.With_implicit_output.create
    "setup_library_odoc_rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f
;;

let setup_library_odoc_rules sctx local_lib =
  Memo.With_implicit_output.exec setup_library_odoc_rules_def (sctx, local_lib)
;;

let odoc_output_targets sctx odoc_file (out : Output_format.t) ~output_dir =
  let action =
    let command =
      match out with
      | Html | Json -> "html-targets"
      | Markdown -> "markdown-targets"
    in
    let ctx = Super_context.context sctx in
    let open Action_builder.O in
    let* odoc = odoc_program sctx output_dir in
    Command.run'
      odoc
      ~sandbox:Sandbox_config.needs_sandboxing
      ~dir:(Path.build output_dir)
      [ Command.Args.A command
      ; A "-o"
      ; Path (Path.build output_dir)
      ; Dep (Path.build (Odoc_artifact.odocl_file ctx odoc_file))
      ; Output_format.args out
      ]
  in
  Super_context.execute_action_stdout sctx ~loc:Loc.none ~dir:output_dir action
  >>| String.split_lines
  >>| List.filter ~f:(fun s -> not (String.is_empty s))
  >>| List.map ~f:(Path.Build.relative output_dir)
;;

let html_generate_args sctx ~search_db ~html_root ~odoc_support_path odoc_file out =
  let ctx = Super_context.context sctx in
  let search_args =
    match search_db with
    | None -> Command.Args.empty
    | Some search_db ->
      Sherlodoc.odoc_args sctx ~search_db ~dir_sherlodoc_dot_js:html_root
  in
  [ search_args
  ; Command.Args.A "-o"
  ; Path (Path.build html_root)
  ; A "--support-uri"
  ; Path (Path.build odoc_support_path)
  ; A "--theme-uri"
  ; Path (Path.build odoc_support_path)
  ; Dep (Path.build (Odoc_artifact.odocl_file ctx odoc_file))
  ; Output_format.args out
  ]
;;

let setup_generate sctx ~search_db odoc_file out =
  let ctx = Super_context.context sctx in
  let odoc_support_path = Paths.odoc_support ctx in
  let command, output_dir, args =
    match out with
    | Output_format.Markdown ->
      ( "markdown-generate"
      , Paths.markdown_root ctx
      , [ Command.Args.A "-o"
        ; Path (Path.build (Paths.markdown_root ctx))
        ; Dep (Path.build (Odoc_artifact.odocl_file ctx odoc_file))
        ] )
    | Html | Json ->
      let html_root = Paths.html_root ctx in
      ( "html-generate"
      , html_root
      , html_generate_args sctx ~search_db ~html_root ~odoc_support_path odoc_file out )
  in
  let* targets =
    match out with
    | Markdown -> odoc_output_targets sctx odoc_file out ~output_dir
    | Html | Json -> Memo.return [ Odoc_artifact.output_file ctx out odoc_file ]
  in
  let run_odoc =
    run_odoc sctx ~dir:(Path.build output_dir) command ~quiet:false ~flags_for:None args
    |> Action_builder.With_targets.add ~file_targets:targets
  in
  add_rule sctx run_odoc
;;

let setup_generate_module_html_and_json sctx ~search_db odoc_file =
  let ctx = Super_context.context sctx in
  let odoc_support_path = Paths.odoc_support ctx in
  let html_root = Paths.html_root ctx in
  let run_odoc out =
    run_odoc
      sctx
      ~dir:(Path.build html_root)
      "html-generate"
      ~quiet:false
      ~flags_for:None
      (html_generate_args
         sctx
         ~search_db:(Some search_db)
         ~html_root
         ~odoc_support_path
         odoc_file
         out)
  in
  let dir = Odoc_artifact.output_file ctx Html odoc_file |> Path.Build.parent_exn in
  let rule =
    Action_builder.progn [ run_odoc Html; run_odoc Json ]
    |> Action_builder.With_targets.add_directories ~directory_targets:[ dir ]
  in
  add_rule sctx rule
;;

let setup_generate_html_and_json sctx ~search_db odoc_file =
  match Odoc_artifact.get_kind odoc_file with
  | Module _ -> setup_generate_module_html_and_json sctx ~search_db odoc_file
  | Page _ ->
    let* () = setup_generate sctx ~search_db:(Some search_db) odoc_file Html in
    setup_generate sctx ~search_db:(Some search_db) odoc_file Json
;;

let setup_generate_markdown sctx odoc_file =
  setup_generate sctx ~search_db:None odoc_file Markdown
;;

let setup_css_rule sctx =
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx in
  let run_odoc =
    let cmd =
      run_odoc
        sctx
        ~dir:(Path.build (Context.build_dir ctx))
        "support-files"
        ~quiet:false
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    Action_builder.With_targets.add_directories ~directory_targets:[ dir ] cmd
  in
  add_rule sctx run_odoc
;;

let setup_toplevel_index_rule sctx output =
  let* packages = Dune_load.packages () in
  let index = Odoc_discovery.Toplevel_index.of_packages packages output in
  let content = Odoc_discovery.Toplevel_index.content output index in
  let ctx = Super_context.context sctx in
  let path = Output_format.toplevel_index_path output ctx in
  add_rule sctx (Action_builder.write_file path content)
;;

let odoc_artefacts : type a. _ -> a Odoc_target.t -> _ =
  fun sctx target ->
  let ctx = Super_context.context sctx in
  let module_artifacts (target : Odoc_target.mod_ Odoc_target.t) lib =
    let obj_dir = Lib_info.obj_dir (Lib.Local.info lib) in
    let mode = mode_of_lib lib in
    let+ modules = Odoc_discovery.entry_modules_by_lib sctx lib in
    List.map modules ~f:(module_artifact target ~obj_dir ~mode)
  in
  match target with
  | Pkg pkg ->
    let+ mlds =
      let+ mlds, _ = Odoc_discovery.mlds sctx pkg in
      let mlds =
        Odoc_discovery.check_mlds_no_dupes ~pkg ~mlds ~path_to_string:(fun p ->
          Path.to_string_maybe_quoted (Path.build p))
      in
      String.Map.update mlds "index" ~f:(function
        | None -> Some (Paths.gen_mld_dir ctx pkg ++ "index.mld", "index")
        | Some _ as s -> s)
    in
    String.Map.to_list_map mlds ~f:(fun _ (path, name) -> page_artifact pkg ~path ~name)
  | Lib (_, lib) -> module_artifacts target lib
  | Private_lib (_, lib) -> module_artifacts target lib
;;

let link_odoc_rules sctx (odoc_file : Odoc_artifact.t) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  (* Link resolves all references, so depend on every dependency library's .odoc
     files (and the package pages) explicitly, so they are materialised for -I
     under sandboxing. *)
  let deps =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    let local_libs = List.filter_map libs ~f:Lib.Local.of_lib in
    let* lib_files = Action_builder.of_memo (lib_odoc_files sctx local_libs) in
    let* pkg_files =
      match pkg with
      | None -> Action_builder.return []
      | Some p ->
        let+ arts = Action_builder.of_memo (odoc_artefacts sctx (Pkg p)) in
        List.map arts ~f:(fun a -> Path.build (Odoc_artifact.odoc_file ctx a))
    in
    Action_builder.deps (Dune_engine.Dep.Set.of_files (lib_files @ pkg_files))
  in
  let dir = Path.build (Path.Build.parent_exn (Odoc_artifact.odocl_file ctx odoc_file)) in
  let run_odoc =
    run_odoc
      sctx
      ~dir
      "link"
      ~quiet:false
      ~flags_for:(Some (Odoc_artifact.odoc_file ctx odoc_file))
      [ odoc_include_flags ctx pkg requires
      ; A "-o"
      ; Target (Odoc_artifact.odocl_file ctx odoc_file)
      ; Dep (Path.build (Odoc_artifact.odoc_file ctx odoc_file))
      ]
  in
  add_rule
    sctx
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets deps >>> run_odoc)
;;

let setup_lib_odocl_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t * Lib.t list Resolve.t

    let equal (sc1, l1, r1) (sc2, l2, r2) =
      Super_context.equal sc1 sc2
      && Lib.Local.equal l1 l2
      && Resolve.equal (List.equal Lib.equal) r1 r2
    ;;

    let hash (sc, l, r) =
      Poly.hash
        (Super_context.hash sc, Lib.Local.hash l, Resolve.hash (List.hash Lib.hash) r)
    ;;

    let to_dyn _ = Dyn.Opaque
  end
  in
  let f (sctx, lib, requires) =
    let* odocs = odoc_artefacts sctx (lib_target lib) in
    let pkg = Lib_info.package (Lib.Local.info lib) in
    Memo.parallel_iter odocs ~f:(fun odoc -> link_odoc_rules sctx ~pkg ~requires odoc)
  in
  Memo.With_implicit_output.create
    "setup_library_odocls_rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f
;;

let setup_lib_odocl_rules sctx lib ~requires =
  Memo.With_implicit_output.exec setup_lib_odocl_rules_def (sctx, lib, requires)
;;

let setup_pkg_rules_def memo_name f =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.Name.t * Compilation_mode.t

    let equal (s1, p1, c1) (s2, p2, c2) =
      Package.Name.equal p1 p2
      && Super_context.equal s1 s2
      && Compilation_mode.equal c1 c2
    ;;

    let hash = Tuple.T3.hash Super_context.hash Package.Name.hash Poly.hash
    let to_dyn (_, package, _) = Package.Name.to_dyn package
  end
  in
  Memo.With_implicit_output.create
    memo_name
    ~input:(module Input)
    ~implicit_output:Rules.implicit_output
    f
;;

let setup_pkg_odocl_rules_def =
  let f (sctx, pkg, for_) =
    let* libs =
      Super_context.context sctx |> Context.name |> Odoc_discovery.libs_of_pkg ~pkg
    in
    let* requires =
      let libs = (libs :> Lib.t list) in
      Lib.closure libs ~linking:false ~for_
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_odocl_rules sctx ~requires)
    and* _ =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let pkg = Some pkg in
      let+ () =
        Memo.parallel_iter pkg_odocs ~f:(fun odoc ->
          link_odoc_rules sctx ~pkg ~requires odoc)
      in
      pkg_odocs
    and* _ =
      Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (lib_target lib))
    in
    Memo.return ()
  in
  setup_pkg_rules_def "setup-package-odocls-rules" f
;;

let setup_pkg_odocl_rules sctx ~pkg ~for_ : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_odocl_rules_def (sctx, pkg, for_)
;;

let out_file ctx (output : Output_format.t) odoc =
  Odoc_artifact.output_file ctx output odoc
;;

let out_files ctx (output : Output_format.t) odocs =
  let extra_files =
    match output with
    | Html -> [ Path.build (Paths.odoc_support ctx) ]
    | Json -> []
    | Markdown -> []
  in
  Path.build (Output_format.toplevel_index_path output ctx)
  :: List.rev_append
       extra_files
       (List.map odocs ~f:(fun odoc -> Path.build (out_file ctx output odoc)))
;;

let add_format_alias_deps ctx format target odocs =
  match (format : Output_format.t) with
  | Markdown ->
    (* skip alias deps for markdown since package directories are directory targets *)
    Memo.return ()
  | Html | Json ->
    let paths = out_files ctx format odocs in
    Rules.Produce.Alias.add_deps
      (Dep.format_alias format ctx target)
      (Action_builder.paths paths)
;;

let setup_lib_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t

    let equal (sc1, l1) (sc2, l2) = Super_context.equal sc1 sc2 && Lib.Local.equal l1 l2
    let hash = Tuple.T2.hash Super_context.hash Lib.Local.hash
    let to_dyn _ = Dyn.Opaque
  end
  in
  let f (sctx, lib) =
    let ctx = Super_context.context sctx in
    let target = lib_target lib in
    let* odocs = odoc_artefacts sctx target in
    let* () = add_format_alias_deps ctx Html target odocs in
    add_format_alias_deps ctx Json target odocs
  in
  Memo.With_implicit_output.create
    "setup-library-html-rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f
;;

let search_db_for_lib sctx lib =
  let target = lib_target lib in
  let ctx = Super_context.context sctx in
  let dir = Paths.html ctx target in
  let* odocs = odoc_artefacts sctx target in
  let odocls = List.map odocs ~f:(Odoc_artifact.odocl_file ctx) in
  Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
;;

let setup_lib_html_rules sctx ~search_db lib =
  let target = lib_target lib in
  let* odocs = odoc_artefacts sctx target in
  let* () =
    Memo.parallel_iter odocs ~f:(fun odoc ->
      setup_generate_html_and_json sctx ~search_db odoc)
  in
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, lib)
;;

let setup_pkg_html_rules_def =
  let f (sctx, pkg, _for_) =
    let ctx = Super_context.context sctx in
    let* libs = Context.name ctx |> Odoc_discovery.libs_of_pkg ~pkg in
    let dir = Paths.html ctx (Pkg pkg) in
    let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
    let* lib_odocs =
      Memo.List.concat_map libs ~f:(fun lib -> odoc_artefacts sctx (lib_target lib))
    in
    let all_odocs = pkg_odocs @ lib_odocs in
    let* search_db =
      let odocls = List.map all_odocs ~f:(Odoc_artifact.odocl_file ctx) in
      Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_html_rules sctx ~search_db) in
    let* () =
      Memo.parallel_iter pkg_odocs ~f:(setup_generate_html_and_json ~search_db sctx)
    in
    let* () = add_format_alias_deps ctx Html (Pkg pkg) all_odocs in
    add_format_alias_deps ctx Json (Pkg pkg) all_odocs
  in
  setup_pkg_rules_def "setup-package-html-rules" f
;;

let setup_pkg_html_rules sctx ~pkg ~for_ : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def (sctx, pkg, for_)
;;

let setup_lib_markdown_rules sctx lib =
  let target = lib_target lib in
  let* () =
    match Lib_info.package (Lib.Local.info lib) with
    | Some _ -> Memo.return ()
    | None ->
      odoc_artefacts sctx target
      >>= Memo.parallel_iter ~f:(fun odoc -> setup_generate_markdown sctx odoc)
  in
  let ctx = Super_context.context sctx in
  odoc_artefacts sctx (lib_target lib) >>= add_format_alias_deps ctx Markdown target
;;

let setup_pkg_markdown_rules sctx ~pkg =
  let ctx = Super_context.context sctx in
  let* libs = Context.name ctx |> Odoc_discovery.libs_of_pkg ~pkg in
  let* all_odocs =
    let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
    let+ lib_odocs =
      Memo.List.concat_map libs ~f:(fun lib -> odoc_artefacts sctx (lib_target lib))
    in
    pkg_odocs @ lib_odocs
  in
  let* () =
    if List.is_empty all_odocs
    then Memo.return ()
    else (
      let pkg_markdown_dir = Paths.markdown ctx (Pkg pkg) in
      let markdown_root = Paths.markdown_root ctx in
      let actions =
        List.map all_odocs ~f:(fun odoc ->
          run_odoc
            sctx
            ~dir:(Path.build markdown_root)
            "markdown-generate"
            ~quiet:false
            ~flags_for:None
            [ Command.Args.A "-o"
            ; Command.Args.Path (Path.build markdown_root)
            ; Command.Args.Dep (Path.build (Odoc_artifact.odocl_file ctx odoc))
            ])
      in
      let rule =
        Action_builder.progn actions
        |> Action_builder.With_targets.add_directories
             ~directory_targets:[ pkg_markdown_dir ]
      in
      add_rule sctx rule)
  in
  let* () = Memo.parallel_iter libs ~f:(setup_lib_markdown_rules sctx) in
  add_format_alias_deps ctx Markdown (Pkg pkg) all_odocs
;;

let setup_package_aliases_format sctx (pkg : Package.t) (output : Output_format.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source (Context.build_dir ctx) pkg_dir in
    Output_format.alias output ~dir
  in
  match (output : Output_format.t) with
  | Markdown ->
    let directory_target = Paths.markdown ctx (Pkg name) in
    let toplevel_index = Paths.markdown_index ctx in
    let deps =
      let open Action_builder.O in
      let+ () = Action_builder.path (Path.build directory_target)
      and+ () = Action_builder.path (Path.build toplevel_index) in
      ()
    in
    Rules.Produce.Alias.add_deps alias deps
  | Html | Json ->
    let* lib_aliases =
      Context.name ctx
      |> Odoc_discovery.libs_of_pkg ~pkg:name
      >>| List.map ~f:(fun lib -> Dep.format_alias output ctx (lib_target lib))
    in
    let deps =
      Dep.format_alias output ctx (Pkg name) :: lib_aliases
      |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)
      |> Action_builder.deps
    in
    Rules.Produce.Alias.add_deps alias deps
;;

let setup_package_aliases sctx (pkg : Package.t) =
  Output_format.iter ~f:(setup_package_aliases_format sctx pkg)
;;

let default_index ~pkg entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  Lib.Local.Map.to_list entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) ->
    let name lib = Lib.name (Lib.Local.to_lib lib) in
    Lib_name.compare (name x) (name y))
  |> List.iter ~f:(fun (lib, modules) ->
    let lib = Lib.Local.to_lib lib in
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string (Lib.name lib));
    Buffer.add_string
      b
      (match modules with
       | [ x ] ->
         sprintf
           "The entry point of this library is the module:\n{!module-%s}.\n"
           (Module_name.to_string (Module.name x))
       | _ ->
         sprintf
           "This library exposes the following toplevel modules:\n{!modules:%s}\n"
           (modules
            |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
            |> List.sort ~compare:(fun x y ->
              Module_name.compare (Module.name x) (Module.name y))
            |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
            |> String.concat ~sep:" ")));
  Buffer.contents b
;;

let package_mlds =
  let memo =
    Memo.create
      "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package_name)
      (fun (sctx, pkg) ->
         Rules.collect (fun () ->
           let* mlds, warnings = Odoc_discovery.mlds sctx pkg in
           Odoc_discovery.report_warnings warnings;
           let mlds =
             Odoc_discovery.check_mlds_no_dupes ~pkg ~mlds ~path_to_string:(fun p ->
               Path.to_string_maybe_quoted (Path.build p))
           in
           let ctx = Super_context.context sctx in
           if String.Map.mem mlds "index"
           then Memo.return mlds
           else (
             let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
             let* entry_modules = Odoc_discovery.entry_modules sctx ~pkg in
             let+ () =
               add_rule
                 sctx
                 (Action_builder.write_file gen_mld (default_index ~pkg entry_modules))
             in
             String.Map.set mlds "index" (gen_mld, "index"))))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)
;;

let setup_package_odoc_rules sctx ~pkg =
  let* mlds = package_mlds sctx ~pkg >>| fst in
  (* CR-someday jeremiedimino: it is weird that we drop the [Package.t] and go
     back to a package name here. Need to try and change that one day. *)
  let+ (_ : Path.Build.t list) =
    String.Map.values mlds
    |> Memo.parallel_map ~f:(fun (path, name) ->
      compile_page
        sctx
        (page_artifact pkg ~path ~name)
        ~pkg
        ~includes:(Action_builder.return []))
  in
  ()
;;

let gen_project_rules sctx project =
  Dune_project.packages project
  |> Dune_lang.Package_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (_, (pkg : Package.t)) ->
    (* setup @doc to build the correct html for the package *)
    setup_package_aliases sctx pkg)
;;

let setup_private_library_doc_alias sctx ~scope ~dir (l : Library.t) =
  match l.visibility with
  | Public _ -> Memo.return ()
  | Private _ ->
    let ctx = Super_context.context sctx in
    let* lib =
      let src_dir = Path.drop_optional_build_context_src_exn (Path.build dir) in
      Lib.DB.find_lib_id_even_when_hidden
        (Scope.libs scope)
        (Local (Library.to_lib_id ~src_dir l))
      >>| Option.value_exn
    in
    let target = lib_target (Lib.Local.of_lib_exn lib) in
    Rules.Produce.Alias.add_deps
      (Alias.make ~dir Alias0.private_doc)
      (target |> Dep.format_alias Html ctx |> Dune_engine.Dep.alias |> Action_builder.dep)
;;

let has_rules ?(directory_targets = Path.Build.Map.empty) m =
  let+ rules = Rules.collect_unit (fun () -> m) in
  let directory_targets =
    Path.Build.Map.union
      directory_targets
      (Rules.directory_targets rules)
      ~f:(fun _ loc _ -> Some loc)
  in
  Gen_rules.make ~directory_targets (Memo.return rules)
;;

let with_package pkg ~f =
  let pkg = Package.Name.of_string pkg in
  let* packages = Dune_load.packages () in
  match Package.Name.Map.find packages pkg with
  | Some pkg -> has_rules (f pkg)
  | None -> Memo.return Gen_rules.no_rules
;;

let gen_rules sctx ~dir rest =
  match rest with
  | [] ->
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ "_html" ] ->
    let ctx = Super_context.context sctx in
    let directory_targets = Path.Build.Map.singleton (Paths.odoc_support ctx) Loc.none in
    has_rules
      ~directory_targets
      (Sherlodoc.sherlodoc_dot_js sctx ~dir:(Paths.html_root ctx)
       >>> setup_css_rule sctx
       >>> setup_toplevel_index_rule sctx Html
       >>> setup_toplevel_index_rule sctx Json)
  | [ "_markdown" ] ->
    let* packages = Dune_load.packages () in
    let ctx = Super_context.context sctx in
    let all_package_dirs =
      Package.Name.Map.to_list packages
      |> List.map ~f:(fun (_, (pkg : Package.t)) ->
        let pkg_name = Package.name pkg in
        Paths.markdown ctx (Pkg pkg_name))
    in
    let directory_targets =
      List.fold_left all_package_dirs ~init:Path.Build.Map.empty ~f:(fun acc dir ->
        Path.Build.Map.set acc dir Loc.none)
    in
    has_rules
      ~directory_targets
      (let* () = setup_toplevel_index_rule sctx Markdown in
       Package.Name.Map.to_seq packages
       |> Memo.parallel_iter_seq ~f:(fun (_, (pkg : Package.t)) ->
         let pkg_name = Package.name pkg in
         setup_pkg_markdown_rules sctx ~pkg:pkg_name))
  | [ "_markdown"; _lib_unique_name_or_pkg ] ->
    (* package directories are directory targets *)
    Memo.return Gen_rules.no_rules
  | [ "_mlds"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
      let pkg = Package.name pkg in
      let* _mlds, rules = package_mlds sctx ~pkg in
      Rules.produce rules)
  | [ "_odoc"; "pkg"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
      let pkg = Package.name pkg in
      setup_package_odoc_rules sctx ~pkg)
  | [ "_odoc"; lib_unique_name_or_pkg ] ->
    has_rules
      (let ctx = Super_context.context sctx in
       let* scope_id = Scope_id.of_string lib_unique_name_or_pkg in
       match scope_id with
       | Scope_id.Package pkg_name ->
         let* packages = Dune_load.packages () in
         (match Package.Name.Map.find packages pkg_name with
          | Some pkg ->
            let* libs =
              Context.name ctx |> Odoc_discovery.libs_of_pkg ~pkg:(Package.name pkg)
            in
            Memo.parallel_iter libs ~f:(fun lib -> setup_library_odoc_rules sctx lib)
          | None -> Memo.return ())
       | Scope_id.Private_lib { lib_name; project; _ } ->
         let* lib_db =
           let+ scope = Scope.DB.find_by_project (Context.name ctx) project in
           Scope.libs scope
         in
         let* lib =
           let+ lib = Lib.DB.find lib_db lib_name in
           Option.bind ~f:Lib.Local.of_lib lib
         in
         (match lib with
          | None -> Memo.return ()
          | Some lib -> setup_library_odoc_rules sctx lib))
  | [ "_odocls"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string (Context.name ctx) lib_unique_name_or_pkg in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let for_ =
         match lib with
         | Some lib ->
           let modes =
             Lib_info.modes (Lib.Local.info lib) |> Compilation_mode.Set.of_lib_mode_set
           in
           Compilation_mode.Set.for_merlin modes
         | None -> Ocaml
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib ->
           (match Lib_info.package (Lib.Local.info lib) with
            | None ->
              let* requires = Lib.closure [ Lib.Local.to_lib lib ] ~linking:false ~for_ in
              setup_lib_odocl_rules sctx lib ~requires
            | Some pkg -> setup_pkg_odocl_rules sctx ~pkg ~for_)
       and+ () =
         let* packages = Dune_load.packages () in
         match
           Package.Name.Map.find packages (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_odocl_rules sctx ~pkg:name ~for_
       in
       ())
  | [ "_html"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string (Context.name ctx) lib_unique_name_or_pkg in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let for_ =
         match lib with
         | Some lib ->
           let modes =
             Lib_info.modes (Lib.Local.info lib) |> Compilation_mode.Set.of_lib_mode_set
           in
           Compilation_mode.Set.for_merlin modes
         | None -> Ocaml
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib ->
           (match Lib_info.package (Lib.Local.info lib) with
            | None ->
              (* lib with no package above it *)
              let* search_db = search_db_for_lib sctx lib in
              setup_lib_html_rules sctx ~search_db lib
            | Some pkg -> setup_pkg_html_rules sctx ~pkg ~for_)
       and+ () =
         let* packages = Dune_load.packages () in
         match
           Package.Name.Map.find packages (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_html_rules sctx ~pkg:name ~for_
       in
       ())
  | _ -> Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
;;
