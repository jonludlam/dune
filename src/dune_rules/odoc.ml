open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

module Scope_key : sig
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let file_key project =
    let name = Dune_project.name project in
    let root = Dune_project.root project in
    let digest = Digest.generic (name, root) |> Digest.to_string in
    String.take digest 12
  ;;

  let find_project_by_key =
    let memo =
      let make_map projects =
        String.Map.of_list_map_exn projects ~f:(fun project -> file_key project, project)
        |> Memo.return
      in
      let module Input = struct
        type t = Dune_project.t list

        let equal = List.equal Dune_project.equal
        let hash = List.hash Dune_project.hash
        let to_dyn = Dyn.list Dune_project.to_dyn
      end
      in
      Memo.create "project-by-keys" ~input:(module Input) make_map
    in
    fun key ->
      let* projects = Dune_load.projects () in
      let+ map = Memo.exec memo projects in
      String.Map.find_exn map key
  ;;

  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      Lib_name.parse_string_exn (Loc.none, s), public_libs
    | Some (lib, key) ->
      let+ scope = find_project_by_key key >>= Scope.DB.find_by_project context in
      Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope
  ;;

  let to_string lib project =
    let key = file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib) key
  ;;
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project
;;

let pkg_or_lnu lib =
  match Lib_info.package (Lib.info lib) with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib
;;

type target =
  | Lib of Lib.Local.t
  | Pkg of Package.Name.t

(* Artifact types - tracking documentation units through the pipeline *)

[@@@warning "-37-69-32"]  (* Suppress unused warnings during refactoring *)

type artifact_kind =
  | Module of { visible : bool; module_name : Module_name.t }
  | Page of { name : string }  (* mld files *)

type artifact_source =
  | Local_source of Path.Build.t  (* cmti, cmt, mld from local build *)
  | Installed_source of {
      src_path : Path.t;  (* From Lib_info.src_dir *)
      module_name : string;
      archive : string;  (* Which archive it belongs to *)
    }

type artifact = {
  kind : artifact_kind;
  source : artifact_source;

  (* Output paths *)
  odoc_file : Path.Build.t;
  odocl_file : Path.Build.t;
  html_file : Path.Build.t;
  json_file : Path.Build.t;
  output_dir : Path.Build.t;  (* Base output directory for this artifact *)

  (* Context for compilation/linking *)
  parent_id : string;  (* e.g., "dyn" or "lwt.unix" *)
  pkg : Package.Name.t option;
  lib_name : Lib_name.t;  (* Library name - needed for -I paths *)

  (* Target context - needed for looking up dependencies *)
  target : target;
}

(* Legacy type alias for backwards compatibility during refactoring *)
type odoc_artefact = artifact

(* Artifact accessor functions *)
let artifact_kind art = art.kind
let artifact_source art = art.source
let artifact_target art = art.target
let artifact_parent_id art = art.parent_id
let artifact_pkg art = art.pkg

let add_rule sctx =
  let dir = Super_context.context sctx |> Context.build_dir in
  Super_context.add_rule sctx ~dir
;;

module Paths = struct
  let odoc_support_dirname = "odoc.support"
  let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"

  let odocs ctx = function
    | Lib lib ->
      let info = Lib.Local.info lib in
      (match Lib_info.package info with
       | Some pkg ->
         (* Use v3 paths for libraries with packages - must match compile_module output *)
         let lib_name = Lib.name (Lib.Local.to_lib lib) in
         (* Path: _doc/_odoc/{package}/{library} *)
         root ctx ++ "_odoc" ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
       | None ->
         (* Fallback to v2 paths for libraries without packages *)
         (* Use _doc/_odoc/{lib_unique_name} instead of .objs directory *)
         root ctx ++ "_odoc" ++ pkg_or_lnu (Lib.Local.to_lib lib))
    | Pkg pkg -> root ctx ++ "_odoc" ++ (Package.Name.to_string pkg)
  ;;

  let html_root ctx = root ctx ++ "_html"
  let odocl_root ctx = root ctx ++ "_odocls"

  let add_pkg_lnu base m =
    base
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib lib -> pkg_or_lnu (Lib.Local.to_lib lib)
  ;;

  let html ctx = function
    | Lib lib ->
      let info = Lib.Local.info lib in
      (match Lib_info.package info with
       | Some pkg ->
         (* Use v3 paths for libraries with packages: _doc/_html/{package}/{library} *)
         let lib_name = Lib.name (Lib.Local.to_lib lib) in
         html_root ctx ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
       | None ->
         (* Fallback to v2 paths for libraries without packages *)
         add_pkg_lnu (html_root ctx) (Lib lib))
    | Pkg pkg -> html_root ctx ++ Package.Name.to_string pkg
  ;;

  let odocl ctx = function
    | Lib lib ->
      let info = Lib.Local.info lib in
      (match Lib_info.package info with
       | Some pkg ->
         (* Use v3 paths for libraries with packages: _doc/_odocls/{package}/{library} *)
         let lib_name = Lib.name (Lib.Local.to_lib lib) in
         odocl_root ctx ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
       | None ->
         (* Fallback to v2 paths for libraries without packages *)
         add_pkg_lnu (odocl_root ctx) (Lib lib))
    | Pkg pkg -> odocl_root ctx ++ Package.Name.to_string pkg
  ;;
  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg
  let odoc_support ctx = html_root ctx ++ odoc_support_dirname
  let toplevel_index ctx = html_root ctx ++ "index.html"
end

module Output_format = struct
  type t =
    | Html
    | Json

  let all = [ Html; Json ]
  let iter ~f = Memo.parallel_iter all ~f

  let extension = function
    | Html -> ".html"
    | Json -> ".html.json"
  ;;

  let args = function
    | Html -> Command.Args.empty
    | Json -> A "--as-json"
  ;;

  let target t odoc_file =
    match t with
    | Html -> odoc_file.html_file
    | Json -> odoc_file.json_file
  ;;

  let alias t ~dir =
    match t with
    | Html -> Alias.make Alias0.doc ~dir
    | Json -> Alias.make Alias0.doc_json ~dir
  ;;

  let toplevel_index_path format ctx =
    let base = Paths.toplevel_index ctx in
    match format with
    | Html -> base
    | Json -> Path.Build.extend_basename base ~suffix:".json"
  ;;
end

module Dep : sig
  (** [format_alias output ctx target] returns the alias that depends on all
      targets produced by odoc for [target] in output format [output]. *)
  val format_alias : Output_format.t -> Context.t -> target -> Alias.t

  (** [deps ctx pkg libraries] returns all odoc dependencies of [libraries]. If
      [libraries] are all part of a package [pkg], then the odoc dependencies of
      the package are also returned*)
  val deps
    :  Context.t
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx target odocs] Adds [odocs] as dependencies for [target].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> target -> Path.Set.t -> unit Memo.t
end = struct
  let format_alias f ctx m = Output_format.alias f ~dir:(Paths.html ctx m)
  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    (* We need Package_discovery to map installed libraries to their opam packages.
       Use Action_builder.of_memo to execute Memo code from Action_builder context. *)
    let* pkg_discovery = Action_builder.of_memo (Package_discovery.create ~context:ctx) in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p -> Dep.Set.singleton (Dep.alias (alias ~dir:(Paths.odocs ctx (Pkg p))))
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match Lib.Local.of_lib lib with
         | None ->
           (* Installed library - add dependency on its odocl files via .odoc-all alias *)
           (* Use Package_discovery to get the correct opam package name *)
           let lib_name = Lib.name lib in
           let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let dir =
                Paths.root ctx
                ++ "_odocls"
                ++ Package.Name.to_string lib_pkg
                ++ Lib_name.to_string lib_name
              in
              let alias_path = alias ~dir in
              Log.info [ Pp.textf "Dep.deps: Adding dependency on installed library %s (opam package=%s) odocls alias at %s"
                           (Lib_name.to_string lib_name)
                           (Package.Name.to_string lib_pkg)
                           (Path.Build.to_string dir) ];
              Dep.Set.add acc (Dep.alias alias_path)
            | None ->
              Log.info [ Pp.textf "Dep.deps: Installed library %s has no opam package, skipping"
                           (Lib_name.to_string lib_name) ];
              acc)
         | Some lib ->
           let dir = Paths.odocs ctx (Lib lib) in
           let alias = alias ~dir in
           Dep.Set.add acc (Dep.alias alias)))
  ;;

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  let setup_deps ctx m files =
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
  ;;
end

let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : Path.Build.t -> t
  val odoc_file : doc_dir:Path.Build.t -> t -> Path.Build.t
  val odoc_input : t -> Path.Build.t
end = struct
  type t = Path.Build.t

  let create p = p

  let odoc_file ~doc_dir t =
    let t = Filename.remove_extension (Path.Build.basename t) in
    Path.Build.relative doc_dir (sprintf "page-%s%s" t odoc_ext)
  ;;

  let odoc_input t = t
end

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

let odoc_dev_tool_lock_dir_exists () =
  let path = Dune_pkg.Lock_dir.dev_tool_lock_dir_path Odoc in
  Fs_memo.dir_exists (Path.Outside_build_dir.In_source_dir path)
;;

let odoc_dev_tool_exe_path_building_if_necessary () =
  let open Action_builder.O in
  let path = Path.build (Pkg_dev_tool.exe_path Odoc) in
  let+ () = Action_builder.path path in
  Ok path
;;

let odoc_program sctx dir =
  let open Action_builder.O in
  let* odoc_dev_tool_lock_dir_exists =
    Action_builder.of_memo (odoc_dev_tool_lock_dir_exists ())
  in
  match odoc_dev_tool_lock_dir_exists with
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
  >>> Command.run_dyn_prog ~dir program [ A command; Dyn base_flags; S args ]
;;

let _module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
     let+ deps =
       if Module.has m ~ml_kind:Intf
       then Dep_graph.deps_of dep_graphs.intf m
       else
         (* When a module has no .mli, use the dependencies for the .ml *)
         Dep_graph.deps_of dep_graphs.impl m
     in
     List.map deps ~f:(fun m -> Path.build (Obj_dir.Module.odoc obj_dir m)))
;;

(* ===== Package-centric path helpers for odoc v3 ===== *)

(* Package-centric path helpers *)
let doc_root_v3 ctx = Path.Build.relative (Context.build_dir ctx) "_doc"

let odoc_root_v3 ctx = Path.Build.relative (doc_root_v3 ctx) "_odoc"

let odocl_root_v3 ctx = Path.Build.relative (doc_root_v3 ctx) "_odocls"

let package_dir_v3 ctx pkg =
  Path.Build.relative (odoc_root_v3 ctx) (Package.Name.to_string pkg)

let library_dir_v3 ctx pkg lib =
  Path.Build.relative (package_dir_v3 ctx pkg) (Lib_name.to_string lib)

let odoc_file_v3 ctx pkg lib module_ =
  let parent_dir = package_dir_v3 ctx pkg in
  let parent_id_path = Path.Build.relative parent_dir (Lib_name.to_string lib) in
  (* Use obj_name to get the wrapped module name (e.g., stdune__User_message) *)
  let basename =
    Module.obj_name module_
    |> Module_name.Unique.artifact_filename ~ext:".odoc"
  in
  Path.Build.relative parent_id_path basename

(* Parent ID computation helpers *)
let parent_id_of_module pkg lib =
  Printf.sprintf "%s/%s" (Package.Name.to_string pkg) (Lib_name.to_string lib)

let parent_id_of_library pkg =
  Package.Name.to_string pkg

let parent_id_root = ""

(* Get the stdlib library, if available *)
let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

(* Helper to determine package ownership of a library *)
let determine_package_for_library_memo sctx lib_name =
  let ctx = Super_context.context sctx in
  let package_discovery = Package_discovery.create ~context:ctx in
  let* _package_discovery = package_discovery in
  match Lib_name.to_string lib_name with
  | lib_str when String.is_prefix lib_str ~prefix:"dune" ->
      (* Fallback: libraries starting with "dune" likely belong to dune package *)
      Memo.return (Package.Name.of_string "dune")
  | _ ->
      (* Default: assume library name matches package name for now *)
      (* TODO: This needs refinement to use actual package discovery *)
      Memo.return (Package.Name.of_string (Lib_name.to_string lib_name))

(* Synchronous version for immediate use *)
let determine_package_for_library sctx lib_name =
  Memo.run (determine_package_for_library_memo sctx lib_name)

(* Prevent unused value warnings until we integrate these functions *)
let () =
  ignore library_dir_v3;
  ignore odoc_file_v3;
  ignore parent_id_of_module;
  ignore parent_id_of_library;
  ignore parent_id_root;
  ignore determine_package_for_library

let compile_module
      sctx
      ~ctx
      ~obj_dir
      ~pkg
      ~lib_name
      ?(lib_deps = Action_builder.return ())
      (m : Module.t)
  =
  let parent_id = parent_id_of_module pkg lib_name in
  let odoc_v3_path = odoc_file_v3 ctx pkg lib_name m in
  let cmti_file = Obj_dir.Module.cmti_file obj_dir m ~cm_kind:(Ocaml Cmi) in

  (* The include path should point to the library's odoc directory where .odoc files are located *)
  let lib_odoc_dir =
    let pkg_name = Package.Name.to_string pkg in
    let lib_name_str = Lib_name.to_string lib_name in
    Paths.root ctx ++ "_odoc" ++ pkg_name ++ lib_name_str
  in

  let run_odoc =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets lib_deps
    >>> Action_builder.With_targets.add ~file_targets:[odoc_v3_path]
      (run_odoc
        sctx
        ~dir:(Path.build (Paths.odocs ctx (Pkg pkg)))
        "compile"
        ~quiet:false
        ~flags_for:(Some odoc_v3_path)
        [ A "-I"
        ; Path (Path.build lib_odoc_dir)
        ; A "--output-dir"
        ; Path (Path.build (odoc_root_v3 ctx))
        ; A "--parent-id"
        ; A parent_id
        ; Dep (Path.build cmti_file)
        ])
  in
  let+ () = add_rule sctx run_odoc in
  odoc_v3_path
;;

let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let odoc_file = Mld.odoc_file m ~doc_dir in
  let odoc_input = Mld.odoc_input m in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build doc_dir)
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

(* Compile an mld file using v3 --parent-id and --output-dir
   Note: odoc creates output in {output_dir}/{parent_id}/page-{name}.odoc
   so output_file should account for this structure *)
let compile_mld_v3 sctx ~mld_path ~output_file ~output_dir ~parent_id =
  let run_odoc =
    Action_builder.With_targets.add ~file_targets:[output_file]
      (run_odoc
        sctx
        ~dir:(Path.build output_dir)
        "compile"
        ~quiet:false
        ~flags_for:(Some output_file)
        [ Dep mld_path
        ; A "--output-dir"
        ; Path (Path.build output_dir)
        ; A "--parent-id"
        ; A parent_id
        ])
  in
  add_rule sctx run_odoc
;;

(* Compile an installed library module using an artifact *)
let compile_installed_module_artifact sctx ~artifact ~src_dir ~lib_deps ~module_names ~pkg_name ~lib_name =
  let ctx = Super_context.context sctx in

  (* Extract module name from artifact *)
  let module_name =
    match artifact.kind with
    | Module { module_name; _ } -> Module_name.to_string module_name
    | Page _ -> assert false  (* Should never be called with a Page artifact *)
  in

  let module_name_lower = String.uncapitalize_ascii module_name in

  (* Find the source file (.cmti, .cmt, or .cmi) *)
  let* cmt_file =
    let cmti = Path.relative src_dir (module_name_lower ^ ".cmti") in
    let* cmti_exists = Fs_memo.file_exists (Path.as_outside_build_dir_exn cmti) in
    if cmti_exists then Memo.return cmti
    else
      let cmt = Path.relative src_dir (module_name_lower ^ ".cmt") in
      let* cmt_exists = Fs_memo.file_exists (Path.as_outside_build_dir_exn cmt) in
      if cmt_exists then Memo.return cmt
      else Memo.return (Path.relative src_dir (module_name_lower ^ ".cmi"))
  in

  (* Generate compile-deps file *)
  let deps_file =
    Paths.root ctx ++ "_odoc" ++ pkg_name ++ lib_name ++ (module_name ^ ".deps")
  in

  let program = odoc_program sctx (Context.build_dir ctx) in

  (* Generate compile-deps rule *)
  let* () =
    let run_compile_deps =
      Command.run_dyn_prog
        program
        ~dir:(Path.build (Context.build_dir ctx))
        ~stdout_to:deps_file
        [ A "compile-deps"; Dep cmt_file ]
    in
    add_rule sctx run_compile_deps
  in

  (* Parse deps file to find dependencies within the same library *)
  let module_deps =
    let open Action_builder.O in
    let* lines = Action_builder.lines_of (Path.build deps_file) in
    let dep_modules =
      List.filter_map lines ~f:(fun line ->
        match String.split ~on:' ' line with
        | [ m; _hash ] -> Some (Module_name.of_string m)
        | _ -> None)
    in
    (* Find .odoc files for dependencies in the same library *)
    let dep_odoc_files =
      List.filter_map dep_modules ~f:(fun dep_module ->
        let dep_module_str = Module_name.to_string dep_module in
        if String.equal dep_module_str module_name then
          None  (* Skip self-dependencies *)
        else if List.mem module_names dep_module_str ~equal:String.equal then
          (* Module is in the same library *)
          let dep_module_lower = String.uncapitalize_ascii dep_module_str in
          let dep_odoc =
            Paths.root ctx ++ "_odoc" ++ pkg_name ++ lib_name ++ (dep_module_lower ^ ".odoc")
          in
          Some (Path.build dep_odoc)
        else
          None  (* Dependencies from other libraries handled via lib_deps *))
    in
    Dune_engine.Dep.Set.of_files dep_odoc_files |> Action_builder.deps
  in

  (* Generate odoc compile rule *)
  (* odoc_output_dir should be the root _odoc directory, since odoc will create
     subdirectories based on the parent_id (e.g., pkg/lib) *)
  let odoc_output_dir = Paths.root ctx ++ "_odoc" in
  let lib_dir = odoc_output_dir ++ pkg_name ++ lib_name in
  let run_odoc =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets lib_deps
    >>> Action_builder.with_no_targets module_deps
    >>> Action_builder.With_targets.add ~file_targets:[artifact.odoc_file]
          (run_odoc
             sctx
             ~dir:(Path.build lib_dir)
             "compile"
             ~quiet:false
             ~flags_for:(Some artifact.odoc_file)
             [ Dep cmt_file
             ; A "-I"
             ; Path (Path.build lib_dir)
             ; A "--output-dir"
             ; Path (Path.build odoc_output_dir)
             ; A "--parent-id"
             ; A artifact.parent_id
             ])
  in
  add_rule sctx run_odoc
;;

let odoc_include_flags ctx pkg ~stdlib_opt requires pkg_discovery =
  (* Debug: inspect what's in requires at the start *)
  let () =
    match Resolve.peek requires with
    | Ok libs_list ->
      let lib_names = List.map libs_list ~f:(fun lib -> Lib_name.to_string (Lib.name lib)) in
      Log.info [ Pp.textf "odoc_include_flags: Called with %d libs: %s"
                   (List.length libs_list)
                   (String.concat ~sep:", " lib_names) ]
    | Error _ ->
      Log.info [ Pp.textf "odoc_include_flags: Called with Error requires" ]
  in
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     (* Add stdlib to the list of libraries if provided *)
     let libs = match stdlib_opt with
       | Some stdlib -> stdlib :: libs
       | None -> libs
     in
     let paths =
       List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
         match Lib.Local.of_lib lib with
         | None ->
           (* Installed library - add v3 path: _odoc/{package}/{library} *)
           (* Use Package_discovery to get the correct opam package name *)
           let lib_name = Lib.name lib in
           let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
           Log.info [ Pp.textf "odoc_include_flags: Processing installed library %s"
                        (Lib_name.to_string lib_name) ];
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let installed_odoc_path =
                Paths.root ctx
                ++ "_odoc"
                ++ Package.Name.to_string lib_pkg
                ++ Lib_name.to_string lib_name
              in
              Log.info [ Pp.textf "odoc_include_flags: Adding include path for %s (opam pkg=%s): %s"
                           (Lib_name.to_string lib_name)
                           (Package.Name.to_string lib_pkg)
                           (Path.Build.to_string installed_odoc_path) ];
              Path.Set.add paths (Path.build installed_odoc_path)
            | None ->
              Log.info [ Pp.textf "odoc_include_flags: Library %s has no opam package, skipping"
                           (Lib_name.to_string lib_name) ];
              paths)
         | Some lib -> Path.Set.add paths (Path.build (Paths.odocs ctx (Lib lib))))
     in
     let paths =
       match pkg with
       | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
       | None -> paths
     in
     Command.Args.S
       (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
          [ Command.Args.A "-I"; Path dir ])))
;;

(* Generate -L library:path flags for odoc link
   These tell odoc where to find .odocl files for library dependencies *)
let odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery =
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     (* Add stdlib to the list of libraries if provided *)
     let libs = match stdlib_opt with
       | Some stdlib -> stdlib :: libs
       | None -> libs
     in
     let lib_args =
       List.concat_map libs ~f:(fun lib ->
         let lib_name = Lib.name lib in
         let lib_name_str = Lib_name.to_string lib_name in
         match Lib.Local.of_lib lib with
         | None ->
           (* Installed library - use _odoc/{package}/{library} path *)
           let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let pkg_name_str = Package.Name.to_string lib_pkg in
              let odoc_path =
                Paths.root ctx
                ++ "_odoc"
                ++ pkg_name_str
                ++ lib_name_str
              in
              let lib_path_arg = lib_name_str ^ ":" ^ Path.Build.to_string odoc_path in
              Log.info [ Pp.textf "odoc_lib_flags: Adding -L %s" lib_path_arg ];
              [ Command.Args.A "-L"; A lib_path_arg ]
            | None ->
              Log.info [ Pp.textf "odoc_lib_flags: Library %s has no package, skipping" lib_name_str ];
              [])
         | Some local_lib ->
           (* Local library - use library's odoc path *)
           let lib_pkg = Lib_info.package (Lib.Local.info local_lib) in
           (match lib_pkg with
            | Some pkg ->
              let pkg_name_str = Package.Name.to_string pkg in
              let odoc_path =
                Paths.root ctx
                ++ "_odoc"
                ++ pkg_name_str
                ++ lib_name_str
              in
              let lib_path_arg = lib_name_str ^ ":" ^ Path.Build.to_string odoc_path in
              Log.info [ Pp.textf "odoc_lib_flags: Adding -L %s (local)" lib_path_arg ];
              [ Command.Args.A "-L"; A lib_path_arg ]
            | None ->
              (* Library without package - use old v2 path *)
              Log.info [ Pp.textf "odoc_lib_flags: Local library %s has no package, skipping" lib_name_str ];
              []))
     in
     Command.Args.S lib_args)
;;

(* Get package dependencies from odoc-config.sexp for a given package *)
let get_config_package_deps pkg_discovery pkg_opt =
  match pkg_opt with
  | None -> []
  | Some pkg ->
    let config = Package_discovery.config_of_package pkg_discovery pkg in
    config.Odoc_config.deps.packages
;;

(* Generate -P package:path flags for odoc link
   These tell odoc where to find .odoc files for package dependencies *)
let odoc_pkg_flags ctx requires pkg_discovery ~current_pkg =
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     (* Collect unique packages from library dependencies *)
     let lib_pkg_paths =
       List.fold_left libs ~init:Package.Name.Map.empty ~f:(fun acc lib ->
         let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
         match lib_pkg_opt with
         | Some pkg ->
           let pkg_name_str = Package.Name.to_string pkg in
           let odoc_path = Paths.root ctx ++ "_odoc" ++ pkg_name_str in
           Package.Name.Map.set acc pkg odoc_path
         | None -> acc)
     in

     (* Also add package dependencies from odoc-config.sexp *)
     let config_pkg_deps = get_config_package_deps pkg_discovery current_pkg in
     let all_pkg_paths =
       List.fold_left config_pkg_deps ~init:lib_pkg_paths ~f:(fun acc pkg ->
         let pkg_name_str = Package.Name.to_string pkg in
         let odoc_path = Paths.root ctx ++ "_odoc" ++ pkg_name_str in
         Package.Name.Map.set acc pkg odoc_path)
     in

     let pkg_args =
       Package.Name.Map.to_list_map all_pkg_paths ~f:(fun pkg path ->
         let pkg_name_str = Package.Name.to_string pkg in
         let pkg_path_arg = pkg_name_str ^ ":" ^ Path.Build.to_string path in
         Log.info [ Pp.textf "odoc_pkg_flags: Adding -P %s" pkg_path_arg ];
         [ Command.Args.A "-P"; A pkg_path_arg ])
       |> List.concat
     in
     Command.Args.S pkg_args)
;;

let link_odoc_rules sctx (odoc_file : odoc_artefact) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  let deps = Dep.deps ctx pkg requires in
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "link"
      ~quiet:false
      ~flags_for:(Some odoc_file.odoc_file)
      [ odoc_include_flags ctx pkg ~stdlib_opt requires pkg_discovery
      ; odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery
      ; odoc_pkg_flags ctx requires pkg_discovery ~current_pkg:odoc_file.pkg
      ; A "-o"
      ; Target odoc_file.odocl_file
      ; Dep (Path.build odoc_file.odoc_file)
      ]
  in
  add_rule
    sctx
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets deps >>> run_odoc)
;;

(* Unified compilation function that works for all artifact types.
   This follows the driver's pattern where all information needed to compile
   is contained in the artifact itself. *)
let compile_artifact sctx ~artifact ~lib_deps =
  let ctx = Super_context.context sctx in

  (* Determine the compilation directory based on the target *)
  let compile_dir = match artifact.target with
    | Lib _ -> artifact.output_dir  (* Library artifacts compile in their own directory *)
    | Pkg pkg -> Paths.odocs ctx (Pkg pkg)  (* Package artifacts compile in package directory *)
  in

  let run_odoc =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets lib_deps
    >>> Action_builder.With_targets.add ~file_targets:[artifact.odoc_file]
      (run_odoc
        sctx
        ~dir:(Path.build compile_dir)
        "compile"
        ~quiet:false
        ~flags_for:(Some artifact.odoc_file)
        [ Command.Args.A "-I"
        ; Command.Args.Path (Path.build artifact.output_dir)
        ; Command.Args.A "--output-dir"
        ; Command.Args.Path (Path.build (odoc_root_v3 ctx))
        ; Command.Args.A "--parent-id"
        ; Command.Args.A artifact.parent_id
        ; (match artifact.source with
           | Local_source path -> Command.Args.Dep (Path.build path)
           | Installed_source { src_path; _ } -> Command.Args.Dep src_path)
        ])
  in
  add_rule sctx run_odoc
;;

(* Unified linking function that works for all artifact types.
   This follows the driver's pattern where all information needed to link
   is derived from the artifact itself. *)
let link_artifact sctx ~artifact =
  (* Compute requires from the artifact's target *)
  let* requires = match artifact.target with
    | Lib lib -> Lib.requires (Lib.Local.to_lib lib)
    | Pkg _ -> Memo.return (Resolve.return [])  (* Package-level artifacts have no library dependencies *)
  in

  (* Call the existing link_odoc_rules with computed requires *)
  link_odoc_rules sctx artifact ~pkg:artifact.pkg ~requires
;;

let setup_generate sctx ~search_db odoc_file out =
  let ctx = Super_context.context sctx in
  let odoc_support_path = Paths.odoc_support ctx in
  let search_args =
    Sherlodoc.odoc_args sctx ~search_db ~dir_sherlodoc_dot_js:(Paths.html_root ctx)
  in
  let html_file = Output_format.target out odoc_file in
  Log.info [ Pp.textf "odoc v3: setup_generate for html_file=%s" (Path.Build.to_string html_file) ];
  (* Check if the HTML file is in a subdirectory (v3 path for modules) or not (v2 path or package mlds) *)
  let html_dir_opt =
    let html_dir = Path.Build.parent_exn html_file in
    let html_root = Paths.html_root ctx in
    if Path.Build.equal html_dir html_root then None else Some html_dir
  in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "html-generate"
      ~quiet:false
      ~flags_for:None
      [ search_args
      ; A "-o"
      ; Path (Path.build (Paths.html_root ctx))
      ; A "--support-uri"
      ; Path (Path.build odoc_support_path)
      ; A "--theme-uri"
      ; Path (Path.build odoc_support_path)
      ; Dep (Path.build odoc_file.odocl_file)
      ; Output_format.args out
      ; (match html_dir_opt with
         | None -> Hidden_targets [ html_file ]
         | Some _ -> Command.Args.empty)
      ]
  in
  (* Add explicit dependency on CSS/support files *)
  let rule =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.path (Path.build odoc_support_path))
    >>> Action_builder.With_targets.add ~file_targets:[html_file] run_odoc
  in
  Log.info [ Pp.textf "odoc v3: calling add_rule for html_file=%s" (Path.Build.to_string html_file) ];
  let+ () = add_rule sctx rule in
  Log.info [ Pp.textf "odoc v3: add_rule completed for html_file=%s" (Path.Build.to_string html_file) ];
  None  (* No directory targets, using file targets instead *)
;;

let setup_generate_all sctx ~search_db odoc_file =
  Memo.List.concat_map Output_format.all ~f:(fun out ->
    let+ dir_opt = setup_generate sctx ~search_db odoc_file out in
    Option.to_list dir_opt)
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

let sp = Printf.sprintf

module Toplevel_index = struct
  type item =
    { name : string
    ; version : Package_version.t option
    ; link : string
    }

  let of_packages packages =
    Package.Name.Map.to_list_map packages ~f:(fun name package ->
      let name = Package.Name.to_string name in
      { name; version = Package.version package; link = sp "%s/index.html" name })
  ;;

  let html_list_items t =
    List.map t ~f:(fun { name; version; link } ->
      let link = sp {|<a href="%s">%s</a>|} link name in
      let version_suffix =
        match version with
        | None -> ""
        | Some v -> sp {| <span class="version">%s</span>|} (Package_version.to_string v)
      in
      sp "<li>%s%s</li>" link version_suffix)
    |> String.concat ~sep:"\n      "
  ;;

  let html t =
    sp
      {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./%s/odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <main class="content">
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      %s
      </ol>
      </div>
    </main>
  </body>
</html>|}
      Paths.odoc_support_dirname
      (html_list_items t)
  ;;

  let string_to_json s = `String s
  let list_to_json ~f l = `List (List.map ~f l)

  let option_to_json ~f = function
    | None -> `Null
    | Some x -> f x
  ;;

  let item_to_json { name; version; link } =
    `Assoc
      [ "name", string_to_json name
      ; ( "version"
        , Option.map ~f:Package_version.to_string version
          |> option_to_json ~f:string_to_json )
      ; "link", string_to_json link
      ]
  ;;

  (** This format is public API. *)
  let to_json items = `Assoc [ "packages", list_to_json items ~f:item_to_json ]

  let json t = Dune_stats.Json.to_string (to_json t)

  let content (output : Output_format.t) t =
    match output with
    | Html -> html t
    | Json -> json t
  ;;
end

let setup_toplevel_index_rule sctx output =
  let* packages = Dune_load.packages () in
  let index = Toplevel_index.of_packages packages in
  let content = Toplevel_index.content output index in
  let ctx = Super_context.context sctx in
  let path = Output_format.toplevel_index_path output ctx in
  add_rule sctx (Action_builder.write_file path content)
;;

let setup_toplevel_index_rules sctx =
  Output_format.iter ~f:(setup_toplevel_index_rule sctx)
;;

let libs_of_pkg ctx ~pkg =
  let+ { Scope.DB.Lib_entry.Set.libraries; _ } =
    Scope.DB.lib_entries_of_package ctx pkg
  in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map libraries ~f:(fun lib ->
    match Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements with
    | None -> Some lib
    | Some _ -> None)
;;

let entry_modules_by_lib sctx lib =
  Dir_contents.modules_of_local_lib sctx lib >>| Modules.entry_modules
;;

let entry_modules sctx ~pkg =
  let* l =
    Super_context.context sctx
    |> Context.name
    |> libs_of_pkg ~pkg
    >>| List.filter ~f:(fun lib ->
      Lib.Local.info lib |> Lib_info.status |> Lib_info.Status.is_private |> not)
  in
  let+ l =
    Memo.parallel_map l ~f:(fun l ->
      let+ m = entry_modules_by_lib sctx l in
      l, m)
  in
  Lib.Local.Map.of_list_exn l
;;

(* Create an artifact for a local module or page *)
let create_artifact_local ctx ~target ~source ~kind =
  let html_base = Paths.html ctx target in
  let odocl_base = Paths.odocl ctx target in
  let odoc_file = source in  (* For local sources, the source path is the odoc file path *)
  let basename = Path.Build.basename odoc_file |> Filename.remove_extension in
  let odocl_file = odocl_base ++ (basename ^ ".odocl") in

  let (html_file, json_file, parent_id, lib_name, output_dir) = match target with
  | Lib lib ->
    let lib_t = Lib.Local.to_lib lib in
    let lib_name = Lib.name lib_t in
    let html_dir = html_base ++ Stdune.String.capitalize basename in
    let file output =
      html_dir ++ "index"
      |> Path.Build.extend_basename ~suffix:(Output_format.extension output)
    in
    let parent_id = Lib_name.to_string lib_name in
    let output_dir = Paths.odocs ctx (Lib lib) in
    (file Html, file Json, parent_id, lib_name, output_dir)
  | Pkg pkg ->
    let file output =
      html_base ++ (basename |> String.drop_prefix ~prefix:"page-" |> Option.value_exn)
      |> Path.Build.extend_basename ~suffix:(Output_format.extension output)
    in
    let parent_id = Package.Name.to_string pkg in
    (* For package-level artifacts, use package name as lib name *)
    let lib_name = Lib_name.of_string parent_id in
    let output_dir = Paths.odocs ctx (Pkg pkg) in
    (file Html, file Json, parent_id, lib_name, output_dir)
  in

  let pkg = match target with
    | Lib lib -> Lib.Local.info lib |> Lib_info.package
    | Pkg pkg -> Some pkg
  in

  { kind
  ; source = Local_source source
  ; odoc_file
  ; odocl_file
  ; html_file
  ; json_file
  ; output_dir
  ; parent_id
  ; pkg
  ; lib_name
  ; target
  }
;;

(* Create an artifact for an installed library module *)
let create_artifact_installed ctx ~pkg ~lib_name ~module_name ~archive ~visible =
  let pkg_name_str = Package.Name.to_string pkg in
  let lib_name_str = Lib_name.to_string lib_name in
  let module_name_lower = String.uncapitalize_ascii module_name in

  (* Paths for installed libraries follow the v3 structure *)
  let odoc_file =
    Paths.root ctx ++ "_odoc" ++ pkg_name_str ++ lib_name_str ++ (module_name_lower ^ ".odoc")
  in
  let odocl_file =
    Paths.root ctx ++ "_odocls" ++ pkg_name_str ++ lib_name_str ++ (module_name_lower ^ ".odocl")
  in

  let html_base = Paths.html_root ctx ++ pkg_name_str ++ lib_name_str in
  let html_dir = html_base ++ module_name in
  let html_file = html_dir ++ "index.html" in
  let json_file = html_dir ++ "index.html.json" in

  let parent_id = pkg_name_str ^ "/" ^ lib_name_str in
  let kind = Module { visible; module_name = Module_name.of_string module_name } in

  (* For installed libraries, we create a dummy target - we don't have Lib.Local.t *)
  (* The target will be used to look up dependencies when needed *)
  let target = Pkg pkg in  (* Use package as target for installed libs *)

  let output_dir = Paths.root ctx ++ "_odoc" ++ pkg_name_str ++ lib_name_str in

  { kind
  ; source = Installed_source { src_path = Path.external_ (Path.External.of_string "/dev/null"); module_name; archive }
  ; odoc_file
  ; odocl_file
  ; html_file
  ; json_file
  ; output_dir
  ; parent_id
  ; pkg = Some pkg
  ; lib_name
  ; target
  }
;;

(* Create an artifact for an installed package mld file *)
let create_artifact_installed_mld ctx ~pkg ~mld_path ~page_name =
  let pkg_name_str = Package.Name.to_string pkg in

  (* Paths for installed package mld files follow the v3 structure *)
  let odoc_name = "page-" ^ page_name ^ ".odoc" in
  let odocl_name = "page-" ^ page_name ^ ".odocl" in
  let odoc_root = odoc_root_v3 ctx in
  let odocl_root = odocl_root_v3 ctx in
  let html_root = Paths.html_root ctx in

  let odoc_file = odoc_root ++ pkg_name_str ++ odoc_name in
  let odocl_file = odocl_root ++ pkg_name_str ++ odocl_name in
  let html_file = html_root ++ pkg_name_str ++ (page_name ^ ".html") in
  let json_file = html_root ++ pkg_name_str ++ (page_name ^ ".json") in

  let parent_id = pkg_name_str in
  let kind = Page { name = page_name } in
  let target = Pkg pkg in
  let lib_name = Lib_name.of_string pkg_name_str in  (* Use package name as lib name for pages *)
  let output_dir = odoc_root ++ pkg_name_str in

  { kind
  ; source = Installed_source { src_path = mld_path; module_name = page_name; archive = pkg_name_str }
  ; odoc_file
  ; odocl_file
  ; html_file
  ; json_file
  ; output_dir
  ; parent_id
  ; pkg = Some pkg
  ; lib_name
  ; target
  }
;;

(* Discover mld files for an installed package and create artifacts *)
let discover_installed_pkg_mld_artifacts ctx ~pkg : artifact list Memo.t =
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let mld_files = Package_discovery.mlds_of_package pkg_discovery pkg in

  Memo.List.filter_map mld_files ~f:(fun mld_path ->
    let mld_basename = Path.basename mld_path in
    let page_name =
      match String.drop_suffix mld_basename ~suffix:".mld" with
      | Some n -> n
      | None -> mld_basename
    in

    (* Skip index.mld - it will be handled by the default package index generation *)
    if String.equal page_name "index" then
      Memo.return None
    else
      Memo.return (Some (create_artifact_installed_mld ctx ~pkg ~mld_path ~page_name))
  )
;;

(* Discover modules for an installed library and create artifacts *)
let discover_installed_lib_artifacts ctx ~pkg ~lib_name ~lib : artifact list Memo.t =
  let pkg_name_str = Package.Name.to_string pkg in
  let lib_name_str = Lib_name.to_string lib_name in

  (* Get archive information *)
  let info = Lib.info lib in
  let archives = Lib_info.archives info in
  let archive_names =
    let byte_archives = Mode.Dict.get archives Mode.Byte in
    match byte_archives with
    | [] ->
      if Lib_name.equal lib_name (Lib_name.of_string "stdlib")
      then [ "stdlib" ]
      else []
    | archives ->
      List.map archives ~f:(fun p -> Path.basename p |> Filename.remove_extension)
  in

  if List.is_empty archive_names then (
    Log.info [ Pp.textf "odoc v3: Installed library %s/%s has no archives, no artifacts"
                 pkg_name_str lib_name_str ];
    Memo.return []
  ) else (
    (* Read classify file *)
    let classify_file =
      Paths.root ctx ++ "classify" ++ pkg_name_str ++ lib_name_str ++ "odoc.classify"
    in

    let* classify_content = Build_system.read_file (Path.build classify_file) in
    let classify_lines = String.split_lines classify_content in

    (* Parse classify file to get modules *)
    let module_names =
      List.concat_map classify_lines ~f:(fun line ->
        match String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) with
        | [] -> []
        | archive :: modules ->
          if List.mem archive_names archive ~equal:String.equal
          then modules
          else []
      )
    in

    Log.info [ Pp.textf "odoc v3: Found %d modules for installed library %s/%s"
                 (List.length module_names) pkg_name_str lib_name_str ];

    (* Create artifacts for each module - assume all are visible for now *)
    (* TODO: We could potentially parse odoc files to check visibility *)
    let default_archive = match archive_names with
      | [] -> "unknown"
      | archive :: _ -> archive
    in
    let artifacts = List.map module_names ~f:(fun module_name ->
      create_artifact_installed ctx ~pkg ~lib_name ~module_name
        ~archive:default_archive
        ~visible:true
    ) in

    Memo.return artifacts
  )
;;

(* Create an artifact for a local library module *)
let create_artifact_local_module ctx ~pkg ~lib_name ~local_lib ~module_ =
  let pkg_name_str = Package.Name.to_string pkg in
  let lib_name_str = Lib_name.to_string lib_name in
  let module_name = Module.name module_ |> Module_name.to_string in
  let module_name_lower = String.uncapitalize_ascii module_name in

  (* Paths for local libraries follow the same v3 structure as installed *)
  let odoc_file =
    Paths.root ctx ++ "_odoc" ++ pkg_name_str ++ lib_name_str ++ (module_name_lower ^ ".odoc")
  in
  let odocl_file =
    Paths.root ctx ++ "_odocls" ++ pkg_name_str ++ lib_name_str ++ (module_name_lower ^ ".odocl")
  in

  let html_base = Paths.html_root ctx ++ pkg_name_str ++ lib_name_str in
  let html_dir = html_base ++ module_name in
  let html_file = html_dir ++ "index.html" in
  let json_file = html_dir ++ "index.html.json" in

  let parent_id = pkg_name_str ^ "/" ^ lib_name_str in
  let kind = Module { visible = Module.visibility module_ = Visibility.Public;
                      module_name = Module.name module_ } in

  let target = Lib local_lib in

  (* Get the source cmti/cmt file *)
  let obj_dir = Lib.Local.obj_dir local_lib in
  let source_file = Obj_dir.Module.cmti_file obj_dir module_ ~cm_kind:(Ocaml Cmi) in

  let output_dir = Paths.root ctx ++ "_odoc" ++ pkg_name_str ++ lib_name_str in

  { kind
  ; source = Local_source source_file
  ; odoc_file
  ; odocl_file
  ; html_file
  ; json_file
  ; output_dir
  ; parent_id
  ; pkg = Some pkg
  ; lib_name
  ; target
  }
;;

(* Create an artifact for a v2 library module (library without package) *)
let create_artifact_v2_module ctx ~lib_unique_name ~local_lib ~module_ =
  let lib_name = Lib.name (Lib.Local.to_lib local_lib) in
  let module_name = Module.name module_ |> Module_name.to_string in
  let module_name_lower = String.uncapitalize_ascii module_name in

  (* v2 paths use the unique library name *)
  let odoc_dir = Paths.root ctx ++ "_odoc" ++ lib_unique_name in
  let odoc_basename = Module.obj_name module_ |> Module_name.Unique.artifact_filename ~ext:".odoc" in
  let odoc_file = Path.Build.relative odoc_dir odoc_basename in

  (* v2 doesn't have odocl/html generation in the same way, but we create paths for consistency *)
  let odocl_dir = Paths.root ctx ++ "_odocls" ++ lib_unique_name in
  let odocl_file = Path.Build.relative odocl_dir (module_name_lower ^ ".odocl") in

  let html_base = Paths.html_root ctx ++ lib_unique_name in
  let html_dir = html_base ++ module_name in
  let html_file = html_dir ++ "index.html" in
  let json_file = html_dir ++ "index.html.json" in

  (* v2 uses --pkg instead of --parent-id, but we store it as parent_id for uniformity *)
  let parent_id = lib_unique_name in
  let kind = Module { visible = Module.visibility module_ = Visibility.Public;
                      module_name = Module.name module_ } in

  let target = Lib local_lib in

  (* Get the source cmti/cmt file *)
  let obj_dir = Lib.Local.obj_dir local_lib in
  let source_file = Obj_dir.Module.cmti_file obj_dir module_ ~cm_kind:(Ocaml Cmi) in

  let output_dir = odoc_dir in

  { kind
  ; source = Local_source source_file
  ; odoc_file
  ; odocl_file
  ; html_file
  ; json_file
  ; output_dir
  ; parent_id
  ; pkg = None  (* v2 libraries have no package *)
  ; lib_name
  ; target
  }
;;

(* Discover modules for a local library and create artifacts.
   Handles both v3 libraries (with packages) and v2 libraries (without packages). *)
let discover_local_lib_artifacts sctx ctx ~pkg ~lib_name ~local_lib : artifact list Memo.t =
  let* all_modules = Dir_contents.modules_of_local_lib sctx local_lib in
  let modules = Modules.fold all_modules ~init:[] ~f:(fun m acc -> m :: acc) in

  (* Check if this is a v2 library (no package) *)
  let info = Lib.Local.info local_lib in
  let actual_pkg = Lib_info.package info in

  let artifacts = match actual_pkg with
  | None ->
    (* v2 library - use lib_unique_name for directory structure *)
    let status = Lib_info.status info in
    let lib_unique_name = match status with
      | Lib_info.Status.Private (project, _) -> Scope_key.to_string lib_name project
      | _ -> Lib_name.to_string lib_name  (* Fallback, shouldn't happen for private libs *)
    in
    List.map modules ~f:(fun module_ ->
      create_artifact_v2_module ctx ~lib_unique_name ~local_lib ~module_
    )
  | Some _ ->
    (* v3 library - use pkg/lib directory structure *)
    List.map modules ~f:(fun module_ ->
      create_artifact_local_module ctx ~pkg ~lib_name ~local_lib ~module_
    )
  in
  Memo.return artifacts
;;

(* Unified artifact discovery for both local and installed libraries *)
let discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib : artifact list Memo.t =
  match Lib.Local.of_lib lib with
  | Some local_lib ->
      (* Local library *)
      discover_local_lib_artifacts sctx ctx ~pkg ~lib_name ~local_lib
  | None ->
      (* Installed library *)
      discover_installed_lib_artifacts ctx ~pkg ~lib_name ~lib
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.rev_map mlds ~f:(fun mld ->
      Filename.remove_extension (Path.Build.basename mld), mld)
    |> Filename.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]
;;

let odoc_artefacts sctx target =
  let ctx = Super_context.context sctx in
  let dir = Paths.odocs ctx target in
  match target with
  | Pkg pkg ->
    let+ mlds =
      let+ mlds = Packages.mlds sctx pkg in
      let mlds = check_mlds_no_dupes ~pkg ~mlds in
      Filename.Map.update mlds "index" ~f:(function
        | None -> Some (Paths.gen_mld_dir ctx pkg ++ "index.mld")
        | Some _ as s -> s)
    in
    Filename.Map.to_list_map mlds ~f:(fun name mld ->
      let odoc_file = Mld.create mld |> Mld.odoc_file ~doc_dir:dir in
      let kind = Page { name } in
      create_artifact_local ctx ~target ~source:odoc_file ~kind)
  | Lib lib ->
    let info = Lib.Local.info lib in
    let+ modules = entry_modules_by_lib sctx lib in
    (* Determine the package this library belongs to *)
    let pkg = Lib_info.package info in

    List.map modules ~f:(fun m ->
      let visible = Module.visibility m = Visibility.Public in
      let module_name = Module.name m in
      let kind = Module { visible; module_name } in

      match pkg with
      | Some pkg ->
        (* Use v3 paths for libraries with packages *)
        let lib_t = Lib.Local.to_lib lib in
        let lib_name = Lib.name lib_t in
        let odoc_file = odoc_file_v3 ctx pkg lib_name m in
        create_artifact_local ctx ~target ~source:odoc_file ~kind
      | None ->
        (* Fallback to v2 paths for libraries without packages *)
        (* Use v2 path pattern: _doc/_odoc/{lib_unique_name} *)
        let lib_t = Lib.Local.to_lib lib in
        let lib_unique = lib_unique_name lib_t in
        let odoc_dir = Paths.root ctx ++ "_odoc" ++ lib_unique in
        let basename = Module.obj_name m |> Module_name.Unique.artifact_filename ~ext:".odoc" in
        let odoc_file = Path.Build.relative odoc_dir basename in
        create_artifact_local ctx ~target ~source:odoc_file ~kind)
;;

(* Helper to group artifacts by library name *)
let group_artifacts_by_lib artifacts =
  List.fold_left artifacts ~init:Lib_name.Map.empty ~f:(fun acc artifact ->
    let lib_name = artifact.lib_name in
    let existing = Lib_name.Map.find acc lib_name |> Option.value ~default:[] in
    Lib_name.Map.set acc lib_name (artifact :: existing)
  )
;;

(* Helper function to compile artifacts for a single library with proper dependencies *)
let compile_library_artifacts sctx ctx ~pkg_name ~lib_name ~lib_artifacts : Path.Build.t Memo.t =
  (* Get the library object for dependency computation *)
  let* lib_db = Scope.DB.public_libs (Context.name ctx) in
  let* lib_opt = Lib.DB.find lib_db lib_name in

  match lib_opt with
  | None ->
    Log.info [ Pp.textf "odoc v3: Library %s not found, skipping" (Lib_name.to_string lib_name) ];
    (* Return a dummy output_dir *)
    Memo.return (Paths.root ctx ++ "_odoc" ++ pkg_name ++ Lib_name.to_string lib_name)
  | Some lib ->
    (* Set up library dependencies *)
    let* pkg_discovery = Package_discovery.create ~context:ctx in
    let* stdlib_opt =
      if Lib_name.equal lib_name (Lib_name.of_string "stdlib")
      then Memo.return None
      else Lib.DB.find lib_db (Lib_name.of_string "stdlib")
    in

    let lib_deps =
      let open Action_builder.O in
      let* requires = Resolve.Memo.read (Lib.requires lib) in
      let requires =
        match stdlib_opt with
        | Some stdlib_lib -> stdlib_lib :: requires
        | None -> requires
      in
      let dep_set =
        List.fold_left requires ~init:Dune_engine.Dep.Set.empty ~f:(fun acc dep_lib ->
          let dep_lib_name = Lib.name dep_lib in
          match Lib.Local.of_lib dep_lib with
          | Some local_dep ->
            let dep_dir = Paths.odocs ctx (Lib local_dep) in
            let dep_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:dep_dir in
            Dune_engine.Dep.Set.add acc (Dune_engine.Dep.alias dep_alias)
          | None ->
            let dep_pkg_opt = Package_discovery.package_of_library pkg_discovery dep_lib in
            (match dep_pkg_opt with
             | Some dep_pkg ->
               let dep_pkg_name = Package.Name.to_string dep_pkg in
               let dep_lib_name_str = Lib_name.to_string dep_lib_name in
               let dep_dir = Paths.root ctx ++ "_odoc" ++ dep_pkg_name ++ dep_lib_name_str in
               let dep_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:dep_dir in
               Dune_engine.Dep.Set.add acc (Dune_engine.Dep.alias dep_alias)
             | None -> acc))
      in
      Action_builder.deps dep_set
    in

    (* Compile each artifact using unified compilation function *)
    let* () =
      Memo.parallel_iter lib_artifacts ~f:(fun artifact ->
        compile_artifact sctx ~artifact ~lib_deps)
    in

    (* Set up .odoc-all alias for this library *)
    let odoc_files = List.map lib_artifacts ~f:(fun artifact -> Path.build artifact.odoc_file) in
    let odoc_path_set = Path.Set.of_list odoc_files in
    let lib_odoc_dir = (List.hd lib_artifacts).output_dir in
    let alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_odoc_dir in
    let* () = Rules.Produce.Alias.add_deps alias (Action_builder.path_set odoc_path_set) in

    (* Return the library's output directory *)
    Memo.return lib_odoc_dir
;;

(* Discover ALL artifacts for a package identifier (either a v3 package name or v2 lib_unique_name).
   For v3 packages: returns artifacts for all libraries in the package + package-level mld files
   For v2 libraries: returns artifacts for all modules in the library (no mld files since v2 has no packages)

   This is the unified entry point that all handlers (odoc, odocls, html) should use. *)
let discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name : (artifact list * string list) Memo.t =
  (* Check if this is a v2 library (contains '@') or a v3 package *)
  if String.contains pkg_or_lib_unique_name '@' then (
    (* v2 library: lib_unique_name format like "dune_pkg@e6ee5b2bc981" *)
    let* lib_name, lib_db = Scope_key.of_string (Context.name ctx) pkg_or_lib_unique_name in
    let* lib_opt =
      let+ lib = Lib.DB.find lib_db lib_name in
      Option.bind ~f:Lib.Local.of_lib lib
    in
    match lib_opt with
    | None -> Memo.return ([], [])
    | Some local_lib ->
      (* Discover artifacts for this v2 library - discover_local_lib_artifacts will detect no package *)
      let dummy_pkg = Package.Name.of_string pkg_or_lib_unique_name in
      let+ artifacts = discover_local_lib_artifacts sctx ctx ~pkg:dummy_pkg ~lib_name ~local_lib in
      (* v2 libraries don't have subdirectories in the same sense as v3 packages *)
      (artifacts, [])
  ) else (
    (* v3 package: regular package name like "dyn" *)
    let pkg = Package.Name.of_string pkg_or_lib_unique_name in

    (* Check if this is a local project package or an installed package *)
    let* is_project_pkg =
      let* packages = Dune_load.packages () in
      Memo.return (Package.Name.Map.mem packages pkg)
    in

    if is_project_pkg then (
      (* Local package - discover mld artifacts + all library artifacts *)
      let* local_libs = Context.name ctx |> libs_of_pkg ~pkg in

      (* Get library subdirectory names *)
      let lib_subdirs = List.map local_libs ~f:(fun local_lib ->
        Lib.name (Lib.Local.to_lib local_lib) |> Lib_name.to_string
      ) in

      (* Get package-level mld artifacts *)
      let* pkg_artifacts = odoc_artefacts sctx (Pkg pkg) in

      (* Get artifacts for all libraries in this package *)
      let* lib_artifacts_list =
        Memo.List.map local_libs ~f:(fun local_lib ->
          let lib = Lib.Local.to_lib local_lib in
          let lib_name = Lib.name lib in
          discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib
        )
      in

      (* Flatten and combine all artifacts *)
      let all_artifacts = pkg_artifacts @ List.concat lib_artifacts_list in
      Memo.return (all_artifacts, lib_subdirs)
    ) else (
      (* Installed package - discover mld artifacts + library artifacts *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let installed_libs = Package_discovery.libraries_of_package pkg_discovery pkg in

      (* Get library subdirectory names for build_dir_only_sub_dirs *)
      let lib_subdirs =
        List.filter_map installed_libs ~f:(fun lib ->
          match Lib.Local.of_lib lib with
          | Some _ -> None  (* Skip local libs *)
          | None ->
            let lib_name = Lib.name lib in
            Some (Lib_name.to_string lib_name))
      in

      (* Get package-level mld artifacts *)
      let* mld_artifacts = discover_installed_pkg_mld_artifacts ctx ~pkg in

      (* Get artifacts for all installed libraries in this package *)
      let* lib_artifacts_list =
        Memo.List.map installed_libs ~f:(fun lib ->
          let lib_name = Lib.name lib in
          discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib
        )
      in

      (* Flatten and combine all artifacts *)
      let all_artifacts = mld_artifacts @ List.concat lib_artifacts_list in
      Memo.return (all_artifacts, lib_subdirs)
    )
  )
;;

(* Unified handler for _odoc and _odocls package/library directories.
   Both handlers follow the same pattern:
   1. Discover artifacts for the package/library
   2. Group artifacts by library
   3. Process each library (either compile for _odoc or link for _odocls)
   4. Return Build_config with rules *)
let handle_package_artifacts sctx ~dir ~path_prefix pkg_or_lib_name =
  let ctx = Super_context.context sctx in

  (* Use unified artifact discovery *)
  let* all_artifacts, lib_subdirs = discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_or_lib_name in

  (* Group artifacts by library *)
  let artifacts_by_lib = group_artifacts_by_lib all_artifacts in

  (* Determine which operation to perform based on path_prefix *)
  let rules = match path_prefix with
    | "_odoc" ->
      (* Compilation: use compile_library_artifacts helper *)
      Rules.collect_unit (fun () ->
        let* lib_alias_dirs =
          Lib_name.Map.to_list artifacts_by_lib
          |> Memo.List.map ~f:(fun (lib_name, lib_artifacts) ->
            if List.is_empty lib_artifacts then
              Memo.return (Paths.root ctx ++ path_prefix ++ pkg_or_lib_name ++ Lib_name.to_string lib_name)
            else
              compile_library_artifacts sctx ctx ~pkg_name:pkg_or_lib_name ~lib_name ~lib_artifacts)
        in

        (* Create package-level .odoc-all alias *)
        let pkg_dir = Paths.root ctx ++ path_prefix ++ pkg_or_lib_name in
        let pkg_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:pkg_dir in
        let lib_alias_deps =
          List.map lib_alias_dirs ~f:(fun lib_dir ->
            Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir
            |> Dune_engine.Dep.alias)
          |> Dune_engine.Dep.Set.of_list
        in
        Rules.Produce.Alias.add_deps pkg_alias (Action_builder.deps lib_alias_deps)
      )
    | "_odocls" ->
      (* Linking: link each artifact and create library-level aliases *)
      Rules.collect_unit (fun () ->
        Lib_name.Map.to_list artifacts_by_lib
        |> Memo.parallel_iter ~f:(fun (lib_name, lib_artifacts) ->
          if List.is_empty lib_artifacts then
            Memo.return ()
          else (
            (* Separate Page artifacts from Module artifacts *)
            let page_artifacts, module_artifacts =
              List.partition_map lib_artifacts ~f:(fun artifact ->
                match artifact.kind with
                | Page _ -> Either.Left artifact
                | Module _ -> Either.Right artifact)
            in

            (* Link Page artifacts (MLD files) without library dependencies *)
            let* () =
              Memo.parallel_iter page_artifacts ~f:(fun artifact ->
                (* Pages don't have library dependencies, use empty requires *)
                link_odoc_rules sctx artifact ~pkg:artifact.pkg ~requires:(Resolve.return [])
              )
            in

            (* Link Module artifacts with library dependencies *)
            let* () =
              if List.is_empty module_artifacts then
                Memo.return ()
              else (
                (* Get the library object for dependency resolution *)
                let* lib_db = Scope.DB.public_libs (Context.name ctx) in
                let* lib_opt = Lib.DB.find lib_db lib_name in

                match lib_opt with
                | None ->
                  Log.info [ Pp.textf "odoc v3: Library %s not found in lib_db, skipping module artifacts"
                              (Lib_name.to_string lib_name) ];
                  Memo.return ()
                | Some lib ->
                  (* Get library dependencies for linking *)
                  let* requires = Lib.requires lib in

                  (* Link each module artifact *)
                  Memo.parallel_iter module_artifacts ~f:(fun artifact ->
                    link_odoc_rules sctx artifact ~pkg:artifact.pkg ~requires
                  )
              )
            in

            (* Set up .odoc-all alias for this library's odocl files (both pages and modules) *)
            let odocl_files = List.map lib_artifacts ~f:(fun artifact -> Path.build artifact.odocl_file) in
            let lib_dir = Paths.root ctx ++ path_prefix ++ pkg_or_lib_name ++ Lib_name.to_string lib_name in
            let lib_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir in
            Rules.Produce.Alias.add_deps lib_alias (Action_builder.paths odocl_files)
          ))
      )
    | _ -> failwith ("Unexpected path_prefix: " ^ path_prefix)
  in

  Memo.return
    (Build_config.Gen_rules.make
       ~build_dir_only_sub_dirs:
         (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir
            (Subdir_set.of_list lib_subdirs))
       rules)
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
    let* odocs = odoc_artefacts sctx (Lib lib) in
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

    type t = Super_context.t * Package.Name.t

    let equal (s1, p1) (s2, p2) = Package.Name.equal p1 p2 && Super_context.equal s1 s2
    let hash = Tuple.T2.hash Super_context.hash Package.Name.hash
    let to_dyn (_, package) = Package.Name.to_dyn package
  end
  in
  Memo.With_implicit_output.create
    memo_name
    ~input:(module Input)
    ~implicit_output:Rules.implicit_output
    f
;;

let setup_pkg_odocl_rules_def =
  let f (sctx, pkg) =
    let* libs = Super_context.context sctx |> Context.name |> libs_of_pkg ~pkg in
    let ctx = Super_context.context sctx in
    let* requires =
      let libs = (libs :> Lib.t list) in
      let* closure = Lib.closure libs ~linking:false in
      (* Add stdlib explicitly to requires unless we ARE stdlib *)
      let pkg_name = Package.Name.to_string pkg in
      let is_stdlib_pkg = String.equal pkg_name "ocaml-compiler" in
      if is_stdlib_pkg then (
        Log.info [ Pp.textf "setup_pkg_odocl_rules: Package %s is stdlib, not adding stdlib to requires" pkg_name ];
        Memo.return closure
      ) else (
        let* public_libs = Scope.DB.public_libs (Context.name ctx) in
        let+ stdlib_opt = Lib.DB.find public_libs (Lib_name.of_string "stdlib") in
        match stdlib_opt with
        | Some stdlib_lib ->
          Log.info [ Pp.textf "setup_pkg_odocl_rules: Adding stdlib to requires for package %s" pkg_name ];
          Resolve.map closure ~f:(fun libs ->
            Log.info [ Pp.textf "setup_pkg_odocl_rules: Closure has %d libs before adding stdlib" (List.length libs) ];
            let result = stdlib_lib :: libs in
            Log.info [ Pp.textf "setup_pkg_odocl_rules: Closure has %d libs after adding stdlib" (List.length result) ];
            result)
        | None ->
          Log.info [ Pp.textf "setup_pkg_odocl_rules: stdlib not found in public_libs for package %s" pkg_name ];
          closure
      )
    in
    (* Debug: inspect what's in requires before passing to setup_lib_odocl_rules *)
    let () =
      match Resolve.peek requires with
      | Ok libs_list ->
        let lib_names = List.map libs_list ~f:(fun lib -> Lib_name.to_string (Lib.name lib)) in
        Log.info [ Pp.textf "setup_pkg_odocl_rules: requires contains %d libs: %s"
                     (List.length libs_list)
                     (String.concat ~sep:", " lib_names) ]
      | Error _ ->
        Log.info [ Pp.textf "setup_pkg_odocl_rules: requires is Error" ]
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
    and* _ = Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib)) in
    Memo.return ()
  in
  setup_pkg_rules_def "setup-package-odocls-rules" f
;;

let setup_pkg_odocl_rules sctx ~pkg : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_odocl_rules_def (sctx, pkg)
;;

let out_file (output : Output_format.t) odoc =
  match output with
  | Html -> odoc.html_file
  | Json -> odoc.json_file
;;

let out_files ctx (output : Output_format.t) odocs =
  let extra_files =
    match output with
    | Html -> [ Path.build (Paths.odoc_support ctx) ]
    | Json -> []
  in
  Path.build (Output_format.toplevel_index_path output ctx)
  :: List.rev_append
       extra_files
       (List.map odocs ~f:(fun odoc -> Path.build (out_file output odoc)))
;;

let search_db_for_lib sctx lib =
  let target = Lib lib in
  let ctx = Super_context.context sctx in
  let dir = Paths.html ctx target in
  Log.info [ Pp.textf "odoc v3: search_db_for_lib getting odocs" ];
  let* odocs = odoc_artefacts sctx target in
  let odocls = List.map odocs ~f:(fun odoc -> odoc.odocl_file) in
  Log.info [ Pp.textf "odoc v3: search_db_for_lib calling Sherlodoc.search_db" ];
  let* result = Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls in
  Log.info [ Pp.textf "odoc v3: search_db_for_lib done" ];
  Memo.return result
;;

let setup_lib_html_rules sctx ~search_db lib =
  let ctx = Super_context.context sctx in
  let lib_name = Lib.name (Lib.Local.to_lib lib) |> Lib_name.to_string in
  Log.info [ Pp.textf "odoc v3: setup_lib_html_rules for lib=%s" lib_name ];
  let target = Lib lib in
  let* odocs = odoc_artefacts sctx target in
  Log.info [ Pp.textf "odoc v3: got %d odocs for lib=%s" (List.length odocs) lib_name ];
  let* _dirs =
    Memo.List.concat_map odocs ~f:(fun odoc ->
      let odoc_path = Path.Build.to_string odoc.odoc_file in
      Log.info [ Pp.textf "odoc v3: calling setup_generate_all for odoc=%s" odoc_path ];
      setup_generate_all sctx ~search_db odoc)
  in
  Log.info [ Pp.textf "odoc v3: finished setup_generate_all for lib=%s" lib_name ];

  (* Add alias dependencies for this library's HTML outputs *)
  let* () =
    Output_format.iter ~f:(fun output ->
      let paths = out_files ctx output odocs in
      Log.info [ Pp.textf "odoc v3: adding %d paths to alias for lib=%s" (List.length paths) lib_name ];
      Rules.Produce.Alias.add_deps
        (Dep.format_alias output ctx target)
        (Action_builder.paths paths))
  in

  (* Also add dependencies on the HTML aliases of all required libraries and packages *)
  let lib_t = Lib.Local.to_lib lib in
  let* deps_result = Lib.requires lib_t in
  let* pkg_discovery = Package_discovery.create ~context:ctx in

  (* Get package dependencies from odoc-config.sexp *)
  let lib_info = Lib.Local.info lib in
  let lib_pkg_opt = Lib_info.package lib_info in
  let config_pkg_deps = get_config_package_deps pkg_discovery lib_pkg_opt in

  match Resolve.peek deps_result with
  | Error _ -> Memo.return ()
  | Ok deps ->
    Log.info [ Pp.textf "odoc v3: Processing %d library deps + %d config pkg deps for HTML aliases for lib=%s"
                 (List.length deps) (List.length config_pkg_deps) lib_name ];
    Output_format.iter ~f:(fun output ->
      (* Library dependencies *)
      let lib_dep_aliases =
        List.filter_map deps ~f:(fun dep_lib ->
          let dep_lib_name = Lib.name dep_lib |> Lib_name.to_string in
          match Lib.Local.of_lib dep_lib with
          | Some local_dep ->
            (* Local library - use local HTML alias *)
            Log.info [ Pp.textf "odoc v3: Adding HTML alias dependency for LOCAL library %s" dep_lib_name ];
            Some (Dep.format_alias output ctx (Lib local_dep))
          | None ->
            (* Installed library - use package-level HTML alias *)
            let dep_pkg_opt = Package_discovery.package_of_library pkg_discovery dep_lib in
            (match dep_pkg_opt with
             | Some dep_pkg ->
               Log.info [ Pp.textf "odoc v3: Adding HTML alias dependency for INSTALLED library %s (pkg=%s)"
                            dep_lib_name (Package.Name.to_string dep_pkg) ];
               Some (Dep.format_alias output ctx (Pkg dep_pkg))
             | None ->
               Log.info [ Pp.textf "odoc v3: Skipping HTML alias dependency for library %s (no package found)" dep_lib_name ];
               None))
      in

      (* Package dependencies from odoc-config.sexp *)
      let config_pkg_aliases =
        List.map config_pkg_deps ~f:(fun dep_pkg ->
          Log.info [ Pp.textf "odoc v3: Adding HTML alias dependency for CONFIG package %s"
                       (Package.Name.to_string dep_pkg) ];
          Dep.format_alias output ctx (Pkg dep_pkg))
      in

      let all_aliases = lib_dep_aliases @ config_pkg_aliases in
      let dep_set =
        Dune_engine.Dep.Set.of_list_map all_aliases ~f:(fun alias ->
          Dune_engine.Dep.alias alias)
      in
      Rules.Produce.Alias.add_deps
        (Dep.format_alias output ctx target)
        (Action_builder.deps dep_set))
;;

(* Unused for now - was planned for dependency HTML generation but not yet implemented properly *)
(*
let setup_installed_lib_html sctx lib =
  (* For installed libraries, we need to be in an implicit output context to add rules *)
  (* This is a memoized version that will only set up HTML once per library *)
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.t

    let equal (sc1, l1) (sc2, l2) = Super_context.equal sc1 sc2 && Lib.equal l1 l2
    let hash = Tuple.T2.hash Super_context.hash Lib.hash
    let to_dyn _ = Dyn.Opaque
  end in

  let f (sctx, lib) =
    let ctx = Super_context.context sctx in
    let lib_name = Lib.name lib in
    let pkg_opt = Lib_info.package (Lib.info lib) in
    match pkg_opt with
    | None -> Memo.return ()
    | Some pkg_name ->
      let pkg_name_str = Package.Name.to_string pkg_name in
      let lib_name_str = Lib_name.to_string lib_name in

      (* Read the classify file to find modules *)
      let classify_path = Paths.root ctx ++ "classify" ++ pkg_name_str ++ lib_name_str ++ "odoc.classify" in
      let* classify_content = Build_system.read_file (Path.build classify_path) in
      let classify_lines = String.split_lines classify_content in
      let archives = Lib_info.archives (Lib.info lib) in
      let archive_names =
        let byte_archives = Mode.Dict.get archives Mode.Byte in
        match byte_archives with
        | [] ->
          if Lib_name.equal lib_name (Lib_name.of_string "stdlib")
          then [ "stdlib" ]
          else []
        | archives ->
          List.map archives ~f:(fun p -> Path.basename p |> Filename.remove_extension)
      in
      let module_names =
        List.concat_map classify_lines ~f:(fun line ->
          match String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) with
          | [] -> []
          | archive :: mods ->
            if List.mem archive_names archive ~equal:String.equal
            then mods
            else []
        )
      in

      (* Generate HTML for each module *)
      Memo.parallel_iter module_names ~f:(fun module_name ->
        let module_name_lower = String.uncapitalize_ascii module_name in
        let odocl_file =
          Paths.root ctx ++ "_odocls" ++ pkg_name_str ++ lib_name_str ++ (module_name_lower ^ ".odocl")
        in
        let html_file =
          Paths.html_root ctx ++ pkg_name_str ++ lib_name_str ++ module_name ++ "index.html"
        in

        let odoc_support_path = Paths.odoc_support ctx in
        let run_odoc =
          run_odoc
            sctx
            ~dir:(Path.build (Paths.html_root ctx))
            "html-generate"
            ~quiet:false
            ~flags_for:None
            [ A "--search-uri"
            ; A "_odoc-theme"
            ; A "-o"
            ; Path (Path.build (Paths.html_root ctx))
            ; A "--support-uri"
            ; Path (Path.build odoc_support_path)
            ; A "--theme-uri"
            ; Path (Path.build odoc_support_path)
            ; Dep (Path.build odocl_file)
            ]
        in
        let rule = Action_builder.With_targets.add ~file_targets:[html_file] run_odoc in
        add_rule sctx rule
      )
  in

  let setup_installed_lib_html_def =
    Memo.With_implicit_output.create
      "setup-installed-library-html-rules"
      ~implicit_output:Rules.implicit_output
      ~input:(module Input)
      f
  in

  Memo.With_implicit_output.exec setup_installed_lib_html_def (sctx, lib)
;;
*)

(* Generate a default index for an installed package *)
let default_index_installed ~pkg lib_names =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  Printf.bprintf b "\nThis package provides the following libraries:\n\n";
  lib_names
  |> List.sort ~compare:Lib_name.compare
  |> List.iter ~f:(fun lib_name ->
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib_name);
    Printf.bprintf b "\nDocumentation for library {!%s}.\n\n" (Lib_name.to_string lib_name));
  Buffer.contents b
;;

(* Generate HTML for installed libraries in a package using artifacts *)
let setup_installed_pkg_html_rules sctx ~pkg : unit Memo.t =
  let ctx = Super_context.context sctx in
  let pkg_name_str = Package.Name.to_string pkg in
  Log.info [ Pp.textf "odoc v3: setup_installed_pkg_html_rules for pkg=%s" pkg_name_str ];

  (* Check if this is a local project package - if so, skip installed lib processing *)
  let* packages = Dune_load.packages () in
  let is_local_pkg = Package.Name.Map.mem packages pkg in

  if is_local_pkg then (
    Log.info [ Pp.textf "odoc v3: Package %s is local, skipping installed library HTML" pkg_name_str ];
    Memo.return ()
  ) else (
    Log.info [ Pp.textf "odoc v3: Package %s is external, processing installed libraries" pkg_name_str ];

    (* Find all installed libraries for this package *)
    let* pkg_discovery = Package_discovery.create ~context:ctx in
    let installed_libs = Package_discovery.libraries_of_package pkg_discovery pkg in

    (* Filter out local libraries - only process truly external installed libraries *)
    let truly_installed_libs =
      List.filter installed_libs ~f:(fun lib ->
        match Lib.Local.of_lib lib with
        | Some _ -> false  (* Local library, skip *)
        | None -> true     (* Installed library, process *)
      )
    in

    Log.info [ Pp.textf "odoc v3: found %d truly installed libraries for pkg=%s"
                 (List.length truly_installed_libs) pkg_name_str ];

    (* Process installed libraries and mld files *)
    let* () =
      if List.is_empty truly_installed_libs then
        Memo.return ()
      else
        (* For each installed library, discover its modules and generate HTML *)
        Memo.parallel_iter truly_installed_libs ~f:(fun lib ->
    let lib_name = Lib.name lib in
    Log.info [ Pp.textf "odoc v3: Processing installed library %s/%s"
                 pkg_name_str (Lib_name.to_string lib_name) ];

    (* Discover artifacts for this library *)
    let* artifacts = discover_installed_lib_artifacts ctx ~pkg ~lib_name ~lib in

    Log.info [ Pp.textf "odoc v3: Found %d artifacts for %s/%s"
                 (List.length artifacts) pkg_name_str (Lib_name.to_string lib_name) ];

    (* Get library dependencies to build HTML alias deps *)
    let* deps_result = Lib.requires lib in
    let* pkg_discovery = Package_discovery.create ~context:ctx in

    (* Collect unique package dependencies from library deps, excluding self-references *)
    let lib_dep_pkgs =
      match Resolve.peek deps_result with
      | Error _ -> []
      | Ok deps ->
        List.filter_map deps ~f:(fun dep_lib ->
          let dep_pkg_opt = Package_discovery.package_of_library pkg_discovery dep_lib in
          match dep_pkg_opt with
          | Some dep_pkg ->
            (* Skip dependencies within the same package to avoid cycles *)
            if Package.Name.equal dep_pkg pkg then
              None
            else
              Some dep_pkg
          | None -> None
        )
    in

    (* Also get package dependencies from odoc-config.sexp *)
    let config_pkg_deps = get_config_package_deps pkg_discovery (Some pkg) in

    (* Combine and deduplicate all package dependencies *)
    let dep_pkgs =
      (lib_dep_pkgs @ config_pkg_deps)
      |> List.sort_uniq ~compare:Package.Name.compare
    in

    (* Generate HTML for each artifact (no cross-package deps at file level) *)
    let* () = Memo.parallel_iter artifacts ~f:(fun artifact ->
      (* Generate HTML from odocl file *)
      let odoc_support_path = Paths.odoc_support ctx in

      (* Individual HTML files only depend on CSS/support, not other packages *)
      let html_deps = Action_builder.path (Path.build odoc_support_path) in

      let run_odoc =
        run_odoc
          sctx
          ~dir:(Path.build (Paths.html_root ctx))
          "html-generate"
          ~quiet:false
          ~flags_for:None
          [ Dep (Path.build artifact.odocl_file)
          ; A "--search-uri"
          ; A "_odoc-theme"
          ; A "-o"
          ; Path (Path.build (Paths.html_root ctx))
          ; A "--support-uri"
          ; Path (Path.build odoc_support_path)
          ; A "--theme-uri"
          ; Path (Path.build odoc_support_path)
          ]
      in
      let rule =
        let open Action_builder.With_targets.O in
        Action_builder.with_no_targets html_deps
        >>> Action_builder.With_targets.add ~file_targets:[artifact.html_file] run_odoc
      in
      add_rule sctx rule
    ) in

    (* Add HTML files to the package HTML alias so they get built when the alias is requested *)
    let html_files = List.map artifacts ~f:(fun artifact -> Path.build artifact.html_file) in

    (* Create @doc-no-deps alias with just this package's HTML files *)
    let doc_no_deps_alias = Alias.make (Alias.Name.of_string "doc-no-deps") ~dir:(Paths.html ctx (Pkg pkg)) in
    let* () = Rules.Produce.Alias.add_deps doc_no_deps_alias (Action_builder.paths html_files) in

    (* Filter dep_pkgs to avoid cycles and missing packages *)
    let filtered_dep_pkgs =
      dep_pkgs
      |> List.filter ~f:(fun dep_pkg ->
           (* Skip self-references to avoid cycles *)
           not (Package.Name.equal dep_pkg pkg))
      |> List.filter ~f:(fun dep_pkg ->
           (* Skip packages with no libraries - they won't have HTML anyway *)
           let libs = Package_discovery.libraries_of_package pkg_discovery dep_pkg in
           not (List.is_empty libs))
    in

    (* Create @doc alias that depends on @doc-no-deps plus filtered config deps *)
    let* () =
      Rules.Produce.Alias.add_deps
        (Dep.format_alias Html ctx (Pkg pkg))
        (Action_builder.dep (Dune_engine.Dep.alias doc_no_deps_alias))
    in

    (* Add dependencies on other packages' @doc-no-deps (not @doc to avoid cycles) *)
    let* () =
      if List.is_empty filtered_dep_pkgs then
        Memo.return ()
      else
        let dep_set =
          Dune_engine.Dep.Set.of_list_map filtered_dep_pkgs ~f:(fun dep_pkg ->
            let dep_no_deps_alias = Alias.make (Alias.Name.of_string "doc-no-deps") ~dir:(Paths.html ctx (Pkg dep_pkg)) in
            Dune_engine.Dep.alias dep_no_deps_alias)
        in
        Rules.Produce.Alias.add_deps
          (Dep.format_alias Html ctx (Pkg pkg))
          (Action_builder.deps dep_set)
    in

    Memo.return ()
  )
    in

    (* Generate HTML for package-level mld files using artifacts *)
    let* artifacts = discover_installed_pkg_mld_artifacts ctx ~pkg in

    Memo.parallel_iter artifacts ~f:(fun artifact ->
      (* Generate HTML directly without Sherlodoc, similar to installed libraries *)
      let odoc_support_path = Paths.odoc_support ctx in
      let html_deps =
        Action_builder.path (Path.build odoc_support_path)
      in

      let run_odoc =
        run_odoc
          sctx
          ~dir:(Path.build (Paths.html_root ctx))
          "html-generate"
          ~quiet:false
          ~flags_for:None
          [ Dep (Path.build artifact.odocl_file)
          ; A "-o"
          ; Path (Path.build (Paths.html_root ctx))
          ; A "--support-uri"
          ; A "_odoc-theme"
          ; A "--theme-uri"
          ; A "_odoc-theme"
          ]
      in

      let rule =
        let open Action_builder.With_targets.O in
        Action_builder.with_no_targets html_deps
        >>> Action_builder.With_targets.add ~file_targets:[artifact.html_file] run_odoc
      in

      add_rule sctx rule
    )
  )
;;

(*
let setup_installed_pkg_html_rules_DISABLED sctx ~pkg : unit Memo.t =
  let ctx = Super_context.context sctx in
  let pkg_name_str = Package.Name.to_string pkg in
  Log.info [ Pp.textf "odoc v3: setup_installed_pkg_html_rules for pkg=%s" pkg_name_str ];

  (* Find all installed libraries for this package *)
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let installed_libs = Package_discovery.libraries_of_package pkg_discovery pkg in

  Log.info [ Pp.textf "odoc v3: found %d installed libraries for pkg=%s"
               (List.length installed_libs) pkg_name_str ];

  (* Filter out local libraries - only process truly external installed libraries *)
  let truly_installed_libs =
    List.filter installed_libs ~f:(fun lib ->
      match Lib.Local.of_lib lib with
      | Some _ -> false  (* Local library, skip *)
      | None -> true     (* Installed library, process *)
    )
  in

  Log.info [ Pp.textf "odoc v3: found %d truly installed (non-local) libraries for pkg=%s"
               (List.length truly_installed_libs) pkg_name_str ];

  Memo.parallel_iter truly_installed_libs ~f:(fun lib ->
    let lib_name = Lib.name lib in
    let lib_name_str = Lib_name.to_string lib_name in
    Log.info [ Pp.textf "odoc v3: Generating HTML for installed library %s/%s" pkg_name_str lib_name_str ];

    (* Check if the library has archives to determine module names *)
    let info = Lib.info lib in
    let archives = Lib_info.archives info in
    let archive_names =
      let byte_archives = Mode.Dict.get archives Mode.Byte in
      match byte_archives with
      | [] ->
        if Lib_name.equal lib_name (Lib_name.of_string "stdlib")
        then [ "stdlib" ]
        else []
      | archives ->
        List.map archives ~f:(fun p -> Path.basename p |> Filename.remove_extension)
    in

    if List.is_empty archive_names then (
      Log.info [ Pp.textf "odoc v3: Library %s/%s has no archives, skipping HTML" pkg_name_str lib_name_str ];
      Memo.return ()
    ) else (
      (* Get the classify file path *)
      let classify_file =
        Paths.root ctx ++ "classify" ++ pkg_name_str ++ lib_name_str ++ "odoc.classify"
      in

      (* Read the classify file to get module names *)
      let* classify_content = Build_system.read_file (Path.build classify_file) in
        let classify_lines = String.split_lines classify_content in

        let module_names =
          List.concat_map classify_lines ~f:(fun line ->
            match String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) with
            | [] -> []
            | archive :: modules ->
              if List.mem archive_names archive ~equal:String.equal
              then modules
              else []
          )
        in

        Log.info [ Pp.textf "odoc v3: Library %s/%s has %d modules for HTML generation"
                     pkg_name_str lib_name_str (List.length module_names) ];

        (* Generate HTML for each module *)
        Memo.parallel_iter module_names ~f:(fun module_name ->
          let module_name_lower = String.uncapitalize_ascii module_name in
          let odocl_file =
            Paths.root ctx ++ "_odocls" ++ pkg_name_str ++ lib_name_str ++ (module_name_lower ^ ".odocl")
          in
          let html_file =
            Paths.html_root ctx ++ pkg_name_str ++ lib_name_str ++ module_name ++ "index.html"
          in

          let odoc_support_path = Paths.odoc_support ctx in
          let run_odoc =
            run_odoc
              sctx
              ~dir:(Path.build (Paths.html_root ctx))
              "html-generate"
              ~quiet:false
              ~flags_for:None
              [ A "--search-uri"
              ; A "_odoc-theme"
              ; A "-o"
              ; Path (Path.build (Paths.html_root ctx))
              ; A "--support-uri"
              ; Path (Path.build odoc_support_path)
              ; A "--theme-uri"
              ; Path (Path.build odoc_support_path)
              ; Dep (Path.build odocl_file)
              ]
          in
          let rule = Action_builder.With_targets.add ~file_targets:[html_file] run_odoc in
          add_rule sctx rule
        )
      )
    )
;;
*)

let setup_pkg_html_rules sctx ~pkg : unit Memo.t =
  let ctx = Super_context.context sctx in
  Log.info [ Pp.textf "odoc v3: setup_pkg_html_rules for pkg=%s" (Package.Name.to_string pkg) ];
  let* libs = Context.name ctx |> libs_of_pkg ~pkg in
  Log.info [ Pp.textf "odoc v3: found %d libs" (List.length libs) ];
  let dir = Paths.html ctx (Pkg pkg) in
  let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
  let* lib_odocs =
    Memo.List.concat_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
  in
  let all_odocs = pkg_odocs @ lib_odocs in
  Log.info [ Pp.textf "odoc v3: setting up sherlodoc rule with %d odocs" (List.length all_odocs) ];
  let* search_db =
    let odocls = List.map all_odocs ~f:(fun artefact -> artefact.odocl_file) in
    Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
  in
  Log.info [ Pp.textf "odoc v3: calling setup_lib_html_rules for %d libs" (List.length libs) ];
  let* () = Memo.parallel_iter libs ~f:(fun lib ->
    let lib_name = Lib.name (Lib.Local.to_lib lib) |> Lib_name.to_string in
    Log.info [ Pp.textf "odoc v3: calling setup_lib_html_rules for lib=%s" lib_name ];
    setup_lib_html_rules sctx ~search_db lib) in
  let* _pkg_dirs = Memo.List.concat_map pkg_odocs ~f:(setup_generate_all ~search_db sctx) in
  Output_format.iter ~f:(fun output ->
    let paths = out_files ctx output all_odocs in
    Rules.Produce.Alias.add_deps
      (Dep.format_alias output ctx (Pkg pkg))
      (Action_builder.paths paths))
;;

let setup_package_aliases_format sctx (pkg : Package.t) (output : Output_format.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source (Context.build_dir ctx) pkg_dir in
    Output_format.alias output ~dir
  in
  let* libs =
    Context.name ctx |> libs_of_pkg ~pkg:name >>| List.map ~f:(fun lib -> Lib lib)
  in
  Pkg name :: libs
  |> List.map ~f:(Dep.format_alias output ctx)
  |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)
  |> Action_builder.deps
  |> Rules.Produce.Alias.add_deps alias
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
           let* mlds = Packages.mlds sctx pkg in
           let mlds = check_mlds_no_dupes ~pkg ~mlds in
           let ctx = Super_context.context sctx in
           if Filename.Map.mem mlds "index"
           then Memo.return mlds
           else (
             let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
             let* entry_modules = entry_modules sctx ~pkg in
             let+ () =
               add_rule
                 sctx
                 (Action_builder.write_file gen_mld (default_index ~pkg entry_modules))
             in
             Filename.Map.set mlds "index" gen_mld)))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)
;;

let setup_package_odoc_rules sctx ~pkg =
  Rules.collect (fun () ->
    let* mlds = package_mlds sctx ~pkg >>| fst in
    let ctx = Super_context.context sctx in

    (* Get all libraries and filter by package *)
    let* libraries_in_package =
      Scope.DB.with_all ctx ~f:(fun find_scope ->
        let* projects = Dune_load.dune_files (Context.name ctx) in
        Memo.List.concat_map projects ~f:(fun dune_file ->
          let* stanzas = Dune_file.stanzas dune_file in
          Memo.List.filter_map stanzas ~f:(fun stanza ->
            match Stanza.repr stanza with
            | Library.T lib ->
              let scope = find_scope (Dune_file.project dune_file) in
              let lib_db = Scope.libs scope in
              let* resolved_lib = Lib.DB.find lib_db (Library.best_name lib) in
              (match resolved_lib with
              | None -> Memo.return None
              | Some resolved_lib ->
                let info = Lib.info resolved_lib in
                (match Lib_info.package info with
                | Some p when Package.Name.equal p pkg -> Memo.return (Some resolved_lib)
                | _ -> Memo.return None))
            | _ -> Memo.return None)))
    in

    (* Compile MLDs *)
    let* mld_odocs =
      Filename.Map.values mlds
      |> Memo.parallel_map ~f:(fun mld ->
        compile_mld
          sctx
          (Mld.create mld)
          ~pkg
          ~doc_dir:(Paths.odocs ctx (Pkg pkg))
          ~includes:(Action_builder.return []))
    in

    (* Also set up deps for MLDs and library aliases at package level *)
    let all_mld_odocs = Path.set_of_build_paths_list mld_odocs in
    let* () = Dep.setup_deps ctx (Pkg pkg) all_mld_odocs in

    (* Add dependencies on library .odoc-all aliases *)
    let* libs = libraries_in_package in
    let pkg_alias_path = Paths.odocs ctx (Pkg pkg) in
    let pkg_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:pkg_alias_path in
    Memo.parallel_iter libs ~f:(fun lib ->
      match Lib.Local.of_lib lib with
      | None -> Memo.return ()
      | Some local_lib ->
        (* Add dependency from package alias to library alias *)
        let lib_alias_path = Paths.odocs ctx (Lib local_lib) in
        let lib_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_alias_path in
        Rules.Produce.Alias.add_deps pkg_alias (Action_builder.dep (Dune_engine.Dep.alias lib_alias))))
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
    let lib = Lib (Lib.Local.of_lib_exn lib) in
    Rules.Produce.Alias.add_deps
      (Alias.make ~dir Alias0.private_doc)
      (lib |> Dep.format_alias Html ctx |> Dune_engine.Dep.alias |> Action_builder.dep)
;;

let has_rules ?(directory_targets = Path.Build.Map.empty) f =
  let rules = Rules.collect_unit f in
  Memo.return (Gen_rules.make ~directory_targets rules)
;;

let with_package pkg ~f =
  let pkg = Package.Name.of_string pkg in
  let* packages = Dune_load.packages () in
  match Package.Name.Map.find packages pkg with
  | Some pkg -> has_rules (fun () -> f pkg)
  | None -> Memo.return Gen_rules.no_rules
;;

(* Helper to find a library by package and name.
   For local libraries, uses Lib_info.package to check package membership.
   For installed libraries, uses Package_discovery. *)
let find_lib_for_package sctx ~pkg ~lib_name =
  let ctx = Super_context.context sctx in

  (* Look up the library - try package's local libs first to avoid finding installed versions of the same library *)
  let* lib_opt =
    (* First search package libraries including private ones *)
    let* pkg_libs = libs_of_pkg (Context.name ctx) ~pkg in
    let* local_lib_opt = Memo.List.find_map pkg_libs ~f:(fun lib ->
      if Lib_name.equal (Lib.name (lib :> Lib.t)) lib_name
      then Memo.return (Some (lib :> Lib.t))
      else Memo.return None)
    in
    match local_lib_opt with
    | Some lib -> Memo.return (Some lib)
    | None ->
      (* Not in package's local libs - try public libs (for installed libraries) *)
      let* public_libs = Scope.DB.public_libs (Context.name ctx) in
      Lib.DB.find public_libs lib_name
  in

  match lib_opt with
  | None ->
    Log.info [ Pp.textf "odoc v3: Library %s not found" (Lib_name.to_string lib_name) ];
    Memo.return None
  | Some lib ->
    (* Check if this is a local or installed library *)
    (match Lib.Local.of_lib lib with
    | Some _ ->
      (* Local library - check package via Lib_info.package *)
      let info = Lib.info lib in
      let lib_pkg = Lib_info.package info in
      if Option.is_some lib_pkg && Package.Name.equal (Option.value_exn lib_pkg) pkg then (
        Log.info [ Pp.textf "odoc v3: Found local library %s in package %s"
                     (Lib_name.to_string lib_name)
                     (Package.Name.to_string pkg) ];
        Memo.return (Some lib)
      ) else (
        Log.info [ Pp.textf "odoc v3: Local library %s not in package %s (has package %s)"
                     (Lib_name.to_string lib_name)
                     (Package.Name.to_string pkg)
                     (match lib_pkg with Some p -> Package.Name.to_string p | None -> "none") ];
        Memo.return None
      )
    | None ->
      (* Installed library - use Package_discovery *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let discovered_pkg = Package_discovery.package_of_library pkg_discovery lib in
      (match discovered_pkg with
      | Some discovered_pkg when Package.Name.equal discovered_pkg pkg ->
        Log.info [ Pp.textf "odoc v3: Found installed library %s in package %s"
                     (Lib_name.to_string lib_name)
                     (Package.Name.to_string pkg) ];
        Memo.return (Some lib)
      | Some other_pkg ->
        Log.info [ Pp.textf "odoc v3: Installed library %s is in package %s, not %s"
                     (Lib_name.to_string lib_name)
                     (Package.Name.to_string other_pkg)
                     (Package.Name.to_string pkg) ];
        Memo.return None
      | None ->
        Log.info [ Pp.textf "odoc v3: Installed library %s not in package %s (not found in Package_discovery)"
                     (Lib_name.to_string lib_name)
                     (Package.Name.to_string pkg) ];
        Memo.return None))
;;

let handle_classify_dir sctx ~pkg_name ~lib_name =
  (* classify library directory: _doc/classify/{package}/{library} *)
  Log.info [ Pp.textf "odoc v3: Handling classify dir for pkg=%s lib=%s" pkg_name lib_name ];
  let pkg = Package.Name.of_string pkg_name in
  let lib_name = Lib_name.of_string lib_name in
  let ctx = Super_context.context sctx in

  Log.info [ Pp.textf "odoc v3: (classify handler) calling find_lib_for_package" ];
  let* lib_opt = find_lib_for_package sctx ~pkg ~lib_name in

  match lib_opt with
  | None ->
    Log.info [ Pp.textf "odoc v3: Library %s not found for classify" (Lib_name.to_string lib_name) ];
    Memo.return ()
  | Some lib ->
    (* Only generate classify for installed libraries, not local ones *)
    match Lib.Local.of_lib lib with
    | Some _local_lib ->
      Log.info [ Pp.textf "odoc v3: Library %s is local, skipping classify" (Lib_name.to_string lib_name) ];
      Memo.return () (* Local library - no classify needed *)
    | None ->
      (* Library is installed - generate odoc classify *)
      Log.info [ Pp.textf "odoc v3: Library %s is installed, generating classify" (Lib_name.to_string lib_name) ];
      let info = Lib.info lib in
      let src_dir = Lib_info.src_dir info in
      let classify_output =
        Paths.root ctx ++ "classify" ++ pkg_name ++ Lib_name.to_string lib_name ++ "odoc.classify"
      in
      let run_classify =
        let program = odoc_program sctx (Context.build_dir ctx) in
        let deps = Action_builder.env_var "ODOC_SYNTAX" in
        let open Action_builder.With_targets.O in
        Action_builder.with_no_targets deps
        >>> Command.run_dyn_prog
              ~dir:(Path.build (Context.build_dir ctx))
              ~stdout_to:classify_output
              program
              [ A "classify"; A (Path.to_string src_dir) ]
      in
      add_rule sctx run_classify
;;

let handle_mlds_dir sctx ~pkg_name =
  (* _mlds/{pkg} - Generate mld files for package *)
  let pkg = Package.Name.of_string pkg_name in
  let* packages = Dune_load.packages () in
  match Package.Name.Map.find packages pkg with
  | Some local_pkg ->
    (* Local package *)
    let pkg = Package.name local_pkg in
    let* _mlds, rules = package_mlds sctx ~pkg in
    Rules.produce rules
  | None ->
    (* Not a local package - check if it's an installed package *)
    let ctx = Super_context.context sctx in
    Log.info [ Pp.textf "odoc v3: Generating mld for installed package %s" pkg_name ];
    let* pkg_discovery = Package_discovery.create ~context:ctx in
    let installed_libs = Package_discovery.libraries_of_package pkg_discovery pkg in
    let truly_installed_libs =
      List.filter installed_libs ~f:(fun lib ->
        match Lib.Local.of_lib lib with
        | Some _ -> false
        | None -> true
      )
    in
    if List.is_empty truly_installed_libs then
      Memo.return ()
    else (
      (* Generate index.mld for installed package *)
      let lib_names = List.map truly_installed_libs ~f:Lib.name in
      let index_content = default_index_installed ~pkg lib_names in
      let index_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
      add_rule sctx (Action_builder.write_file index_mld index_content)
    )
;;

let handle_html_dir sctx ~lib_unique_name_or_pkg =
  (* Unified HTML handler using package identifier detection *)
  Log.info [ Pp.textf "odoc v3: Handling HTML dir for %s (unified)" lib_unique_name_or_pkg ];

  (* Detect if this is v3 package or v2 library by checking for '@' *)
  let is_v3_package = not (String.contains lib_unique_name_or_pkg '@') in

  if is_v3_package then (
    (* v3 package (local or installed) *)
    let pkg = Package.Name.of_string lib_unique_name_or_pkg in
    let* packages = Dune_load.packages () in
    let is_local = Package.Name.Map.mem packages pkg in

    Log.info [ Pp.textf "odoc v3: Package %s is %s - calling appropriate setup function"
                 lib_unique_name_or_pkg
                 (if is_local then "LOCAL" else "INSTALLED") ];

    if is_local then
      setup_pkg_html_rules sctx ~pkg
    else
      setup_installed_pkg_html_rules sctx ~pkg
  ) else (
    (* v2 library (contains '@') *)
    let ctx = Super_context.context sctx in
    let* lib_name, lib_db = Scope_key.of_string (Context.name ctx) lib_unique_name_or_pkg in
    let* lib_opt =
      let+ lib = Lib.DB.find lib_db lib_name in
      Option.bind ~f:Lib.Local.of_lib lib
    in

    match lib_opt with
    | None ->
      Log.info [ Pp.textf "odoc v3: v2 library %s not found" lib_unique_name_or_pkg ];
      Memo.return ()
    | Some lib ->
      (* v2 library - generate HTML directly *)
      let* search_db = search_db_for_lib sctx lib in
      setup_lib_html_rules sctx ~search_db lib
  )
;;

(* NOTE: handle_odoc_v2_lib_dir was deleted - it's no longer needed.
   The unified _odoc handler now handles both v3 packages and v2 libraries
   through discover_package_artifacts, which internally detects '@' in the name. *)

(* NOTE: handle_odoc_lib_dir was deleted because it's never called.
   The pattern [ "_odoc"; pkg_name; lib_name ] just redirects to parent,
   and the package-level handler generates rules for all libraries. *)


(* NOTE: handle_odocls_lib_dir was deleted because it's never called.
   The pattern [ "_odocls"; pkg_name; lib_name ] just redirects to parent,
   and the package-level handler generates linking rules for all libraries. *)

let gen_rules sctx ~dir rest =
  let rest_str = String.concat ~sep:"/" rest in
  let rest_len = List.length rest in
  Log.info [ Pp.textf "odoc v3: gen_rules ENTRY for dir %s with rest '%s' (length=%d)"
      (Path.Build.to_string dir)
      rest_str
      rest_len ];
  (match rest with
  | [ "_html"; a; b ] ->
    Log.info [ Pp.textf "odoc v3: PRE-MATCH: 3-element _html pattern [_html; %s; %s]" a b ];
  | _ -> ());
  let result = (match rest with
  | [] ->
    Log.info [ Pp.textf "odoc v3: Matched rest=[]" ];
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ "_html" ] ->
    (* Root HTML directory - allow package subdirectories and set up sherlodoc and index files *)
    let ctx = Super_context.context sctx in
    let* packages = Dune_load.packages () in
    let pkg_subdirs =
      Package.Name.Map.keys packages
      |> List.map ~f:Package.Name.to_string
    in
    let directory_targets = Path.Build.Map.singleton (Paths.odoc_support ctx) Loc.none in
    let rules = Rules.collect_unit (fun () ->
      Sherlodoc.sherlodoc_dot_js sctx ~dir:(Paths.html_root ctx)
      >>> setup_css_rule sctx
      >>> setup_toplevel_index_rules sctx
    ) in
    Memo.return
      (Build_config.Gen_rules.make
         ~directory_targets
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir
              (Subdir_set.of_list pkg_subdirs))
         rules)
  | [ "_html"; pkg_name; lib_name ] ->
    (* Library directory: _doc/_html/{package}/{library} *)
    (* Redirect to parent - the package level handler will generate HTML for all libraries *)
    Log.info [ Pp.textf "odoc v3: Library directory handler for pkg=%s lib=%s - redirecting to parent" pkg_name lib_name ];
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_html"; pkg_name; lib_name; module_name ] ->
    Log.info [ Pp.textf "odoc v3: Module directory handler for pkg=%s lib=%s module=%s - redirecting to parent" pkg_name lib_name module_name ];
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_mlds"; pkg_name ] ->
    has_rules (fun () -> handle_mlds_dir sctx ~pkg_name)
  | [ "_odoc"; pkg_or_lib_name ] ->
    (* Compilation: use unified handler *)
    handle_package_artifacts sctx ~dir ~path_prefix:"_odoc" pkg_or_lib_name
  | [ "_odoc"; pkg_name; lib_name ] ->
    (* Library directory: _doc/_odoc/{package}/{library} *)
    (* Redirect to parent - the package level handler will generate rules for all libraries *)
    Log.info [ Pp.textf "odoc v3: Library directory handler for pkg=%s lib=%s - redirecting to parent" pkg_name lib_name ];
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_odocls" ] ->
    (* Root odocls directory - allow subdirs *)
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ "_odocls"; pkg_or_lib_name ] ->
    (* Linking: use unified handler *)
    handle_package_artifacts sctx ~dir ~path_prefix:"_odocls" pkg_or_lib_name
  | [ "_odocls"; pkg_name; lib_name ] ->
    (* Library directory: _doc/_odocls/{package}/{library} *)
    (* Redirect to parent - the package level handler will generate rules for all libraries *)
    Log.info [ Pp.textf "odoc v3: Library directory handler for pkg=%s lib=%s - redirecting to parent" pkg_name lib_name ];
    Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
  | [ "_html"; lib_unique_name_or_pkg ] ->
    has_rules (fun () -> handle_html_dir sctx ~lib_unique_name_or_pkg)
  | [ "classify"; pkg_name; lib_name ] ->
    has_rules (fun () -> handle_classify_dir sctx ~pkg_name ~lib_name)
  | other ->
    Log.info [ Pp.textf "odoc v3: No handler matched for rest=%s (pattern=%s)"
                 (String.concat ~sep:"/" rest)
                 (match other with
                  | [] -> "[]"
                  | [a] -> sprintf "[%s]" a
                  | [a; b] -> sprintf "[%s; %s]" a b
                  | [a; b; c] -> sprintf "[%s; %s; %s]" a b c
                  | [a; b; c; d] -> sprintf "[%s; %s; %s; %s]" a b c d
                  | _ -> sprintf "[%d elements]" (List.length other)) ];
    (* For unmatched paths, return empty rules with no subdirectories allowed.
       Subdirectories should be explicitly listed in parent handlers. *)
    Memo.return
      (Build_config.Gen_rules.make
         (Memo.return Rules.empty))
  ) in
  Log.info [ Pp.textf "odoc v3: gen_rules EXIT for dir %s with result from pattern: %s"
               (Path.Build.to_string dir)
               (match rest with
                | [] -> "empty"
                | [a] -> sprintf "[%s]" a
                | [a; b] -> sprintf "[%s; %s]" a b
                | [a; b; c] -> sprintf "[%s; %s; %s]" a b c
                | _ -> sprintf "[%d elements]" (List.length rest)) ];
  result
;;
