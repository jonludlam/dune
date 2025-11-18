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

let lib_unique_name (local_lib : Lib.Local.t) =
  let lib = Lib.Local.to_lib local_lib in
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project
;;

let pkg_or_lnu (local_lib : Lib.Local.t) =
  let lib = Lib.Local.to_lib local_lib in
  match Lib_info.package (Lib.info lib) with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name local_lib
;;

(* Get the unique ID for a library - this is the package name if available,
   otherwise the library's unique name (which includes project scope for private libs).
   This is used for odoc's --unique-id flag. *)
let lib_unique_id_string (lib : Lib.t) =
  match Lib.Local.of_lib lib with
  | Some local_lib -> pkg_or_lnu local_lib
  | None ->
    (* For installed libraries, use the package name from Lib_info.
       This may not be correct (e.g., compiler-libs), but this function is only used
       for the --unique-id flag which might not matter for installed libs. *)
    (match Lib_info.package (Lib.info lib) with
     | Some p -> Package.Name.to_string p
     | None -> Lib_name.to_string (Lib.name lib))
;;

type target =
  | Lib of Package.Name.t * Lib.t
    (* Library with a real package - package overrides Lib_info.package for installed libs *)
  | Private_lib of string * Lib.t
    (* Library without a real package - uses lib_unique_name as identifier *)
  | Pkg of Package.Name.t

(* Artifact types - tracking documentation units through the pipeline *)

[@@@warning "-37-69-32"] (* Suppress unused warnings during refactoring *)

type artifact_kind =
  | Module of
      { visible : bool
      ; module_name : Module_name.t
      }
  | Page of
      { name : string
      ; pkg_libs : Lib.t list
      }
(* mld files - pkg_libs are the libraries to link with *)

type artifact_source =
  | Local_source of Path.Build.t (* cmti, cmt, mld from local build *)
  | Installed_source of
      { src_path : Path.t (* From Lib_info.src_dir *)
      ; module_name : string
      ; archive : string (* Which archive it belongs to *)
      }

(* Check if a library is vendored using dune's vendored_dirs mechanism *)
let is_lib_vendored lib =
  let lib_info = Lib.info lib in
  match Lib_info.status lib_info with
  | Installed_private | Installed -> Memo.return false
  | Public (proj, _) | Private (proj, _) ->
    Source_tree.is_vendored (Dune_project.root proj)
;;

module Artifact : sig
  type t

  val kind : t -> artifact_kind
  val source : t -> artifact_source
  val odoc_file : t -> Path.Build.t
  val odocl_file : t -> Path.Build.t
  val html_file : Context.t -> string -> t -> Path.Build.t
  val json_file : Context.t -> string -> t -> Path.Build.t
  val output_dir : t -> Path.Build.t
  val parent_id : t -> string
  val pkg : t -> Package.Name.t option
  val lib_name : t -> Lib_name.t
  val target : t -> target
  val odoc_config : t -> Odoc_config.t
  val hidden : t -> bool
  val lib_modules : t -> Module_name.Set.t

  val create
    :  doc_root:Path.Build.t
    -> kind:artifact_kind
    -> source:artifact_source
    -> target:target
    -> odoc_config:Odoc_config.t
    -> lib_modules:Module_name.Set.t
    -> t
end = struct
  type t =
    { doc_root : Path.Build.t
    ; kind : artifact_kind
    ; source : artifact_source
    ; target : target
    ; odoc_config : Odoc_config.t
    ; lib_modules : Module_name.Set.t
    ; parent_id : string (* Cached - computed once in constructor *)
    }

  (* Accessors for stored fields *)
  let kind t = t.kind
  let source t = t.source
  let target t = t.target
  let odoc_config t = t.odoc_config
  let lib_modules t = t.lib_modules
  let parent_id t = t.parent_id

  (* Derived accessors - computed from target *)
  let pkg t =
    match t.target with
    | Lib (pkg, _) -> Some pkg
    | Private_lib _ -> None
    | Pkg pkg -> Some pkg
  ;;

  let lib_name t =
    match t.target with
    | Lib (_, lib) | Private_lib (_, lib) -> Lib.name lib
    | Pkg pkg -> Lib_name.of_string (Package.Name.to_string pkg)
  ;;

  (* Path computation helpers - use parent_id as the base directory *)
  let odocs_dir t =
    (* parent_id is "pkg/lib" for modules in libs, or "pkg" for package artifacts *)
    t.doc_root ++ "_odoc" ++ t.parent_id
  ;;

  let html_dir t =
    (* parent_id is "pkg/lib" for modules in libs, or "pkg" for package artifacts *)
    t.doc_root ++ "_html" ++ t.parent_id
  ;;

  let html_dir_for_mode ctx output_subdir t =
    (* Use mode-specific html root instead of hardcoded _html *)
    let doc_root = Context.build_dir ctx ++ "_doc" in
    let mode_root = doc_root ++ output_subdir in
    mode_root ++ t.parent_id
  ;;

  let odocl_dir t =
    (* parent_id is "pkg/lib" for modules in libs, or "pkg" for package artifacts *)
    t.doc_root ++ "_odocls" ++ t.parent_id
  ;;

  (* Extract the in_doc name from kind (for pages) or source (for modules) *)
  let get_basename_info t =
    match t.kind, t.source with
    | Page { name; _ }, _ -> name, "page-" ^ name
    | Module _, Local_source src_path ->
      let basename = Path.Build.basename src_path |> Filename.remove_extension in
      basename, basename
    | Module _, Installed_source { module_name = mod_str; _ } ->
      let basename = String.uncapitalize_ascii mod_str in
      basename, basename
  ;;

  (* Computed path accessors *)

  (* Generic function for odoc/odocl files - they follow the same structure *)
  let doc_file t ~base_dir ~extension =
    let basename, prefixed_basename = get_basename_info t in
    match t.kind with
    | Page _ ->
      (* For hierarchical pages, parent_id already includes the subdirectory,
         so we only need the leaf filename here, not the full path *)
      (match String.rsplit2 basename ~on:'/' with
       | Some (_parent_path, page_name) ->
         (* parent_id already has the subdirectory, just add the filename *)
         base_dir ++ ("page-" ^ page_name ^ extension)
       | None -> base_dir ++ (prefixed_basename ^ extension))
    | Module _ -> base_dir ++ (basename ^ extension)
  ;;

  let odoc_file t = doc_file t ~base_dir:(odocs_dir t) ~extension:".odoc"
  let odocl_file t = doc_file t ~base_dir:(odocl_dir t) ~extension:".odocl"

  (* Generic function for html/json files - they follow the same structure *)
  let html_output_file t ~html_base ~suffix =
    let basename, _ = get_basename_info t in
    match t.kind, t.target with
    | Module _, (Lib _ | Private_lib _) ->
      let html_dir = html_base ++ Stdune.String.capitalize basename in
      html_dir ++ ("index" ^ suffix)
    | Page _, Pkg _ ->
      (* For hierarchical pages, parent_id already includes the subdirectory,
         so we only need the leaf filename here *)
      (match String.rsplit2 basename ~on:'/' with
       | Some (_parent_path, page_name) ->
         (* parent_id already has the subdirectory, just add the filename *)
         let html_path = html_base ++ page_name in
         Path.Build.extend_basename html_path ~suffix
       | None ->
         let html_path = html_base ++ basename in
         Path.Build.extend_basename html_path ~suffix)
    | Module _, Pkg _ -> assert false (* Modules should have Lib or Private_lib targets, not Pkg *)
    | Page _, (Lib _ | Private_lib _) -> assert false (* Pages should have Pkg targets, not Lib *)
  ;;

  let html_file ctx output_subdir t =
    html_output_file t ~html_base:(html_dir_for_mode ctx output_subdir t) ~suffix:".html"
  ;;

  let json_file ctx output_subdir t =
    html_output_file t ~html_base:(html_dir_for_mode ctx output_subdir t) ~suffix:".html.json"
  ;;

  let output_dir t = odocs_dir t

  let hidden t =
    match t.kind with
    | Page _ -> false
    | Module _ ->
      let odocl_path = Path.Build.to_string (odocl_file t) in
      String.contains_double_underscore odocl_path
  ;;

  (* Helper to compute parent_id from kind and target *)
  let compute_parent_id ~kind ~target =
    match kind, target with
    | Module _, Lib (pkg, lib) ->
      (* Library with real package: parent_id is "pkg/lib" *)
      Package.Name.to_string pkg ^ "/" ^ Lib_name.to_string (Lib.name lib)
    | Module _, Private_lib (lib_unique_name, _) ->
      (* Private library: parent_id is just the lib_unique_name *)
      lib_unique_name
    | Page { name = in_doc_name; _ }, Pkg pkg ->
      (* For hierarchical pages, parent_id includes the subdirectory.
         For example, "deprecated/index.mld" has parent_id "odoc/deprecated" *)
      (match String.rsplit2 in_doc_name ~on:'/' with
       | Some (parent_path, _) -> Package.Name.to_string pkg ^ "/" ^ parent_path
       | None -> Package.Name.to_string pkg)
    | Module _, Pkg _ -> assert false (* Modules should have Lib or Private_lib targets, not Pkg *)
    | Page _, (Lib _ | Private_lib _) -> assert false (* Pages should have Pkg targets, not Lib *)
  ;;

  let create ~doc_root ~kind ~source ~target ~odoc_config ~lib_modules =
    let parent_id = compute_parent_id ~kind ~target in
    { doc_root; kind; source; target; odoc_config; lib_modules; parent_id }
  ;;
end

type artifact = Artifact.t

(* Legacy type alias for backwards compatibility during refactoring *)
type odoc_artefact = artifact

let add_rule sctx =
  let dir = Super_context.context sctx |> Context.build_dir in
  Super_context.add_rule sctx ~dir
;;

(* Doc_mode type and helpers - defined early to avoid circular dependencies *)
module Doc_mode = struct
  type t =
    | Local_only (* @doc - only local packages, with remapping *)
    | Full (* @doc-full - all packages, no remapping *)

  let output_subdir = function
    | Local_only -> "_html"
    | Full -> "_html_full"
  ;;

  let all = [ Local_only; Full ]
end

module Paths = struct
  let odoc_support_dirname = "odoc.support"
  let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"

  let odocs ctx = function
    | Lib (pkg, lib) ->
      (* Library with real package: _doc/_odoc/{package}/{library} *)
      let lib_name = Lib.name lib in
      root ctx ++ "_odoc" ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
    | Private_lib (lib_unique_name, _) ->
      (* Private library: _doc/_odoc/{lib_unique_name} *)
      root ctx ++ "_odoc" ++ lib_unique_name
    | Pkg pkg -> root ctx ++ "_odoc" ++ Package.Name.to_string pkg
  ;;

  let html_root ctx mode = root ctx ++ Doc_mode.output_subdir mode
  let odocl_root ctx = root ctx ++ "_odocls"

  let add_pkg_lnu base m =
    base
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib (pkg, _lib) -> Package.Name.to_string pkg
    | Private_lib (lib_unique_name, _) -> lib_unique_name
  ;;

  let html ctx mode target =
    match target with
    | Lib (pkg, lib) ->
      let lib_name = Lib.name lib in
      html_root ctx mode ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
    | Private_lib (lib_unique_name, _) ->
      (* Private library: use lib_unique_name as the complete identifier *)
      html_root ctx mode ++ lib_unique_name
    | Pkg pkg -> html_root ctx mode ++ Package.Name.to_string pkg
  ;;

  let odocl ctx = function
    | Lib (pkg, lib) ->
      let lib_name = Lib.name lib in
      odocl_root ctx ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
    | Private_lib (lib_unique_name, _) ->
      (* Private library: use lib_unique_name as the complete identifier *)
      odocl_root ctx ++ lib_unique_name
    | Pkg pkg -> odocl_root ctx ++ Package.Name.to_string pkg
  ;;

  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg
  let odoc_support ctx mode = html_root ctx mode ++ odoc_support_dirname
  let toplevel_index ctx mode = html_root ctx mode ++ "index.html"

  (* Sidebar root directory - separate from _odocls for cleaner organization *)
  let sidebar_root ctx = root ctx ++ "_sidebar"

  (* Index file for a package - generated after linking, input to sidebar generation *)
  let index_file ctx pkg =
    sidebar_root ctx ++ Package.Name.to_string pkg ++ "index.odoc-index"
  ;;

  (* Binary sidebar file for a package - generated after indexing *)
  let sidebar_file ctx pkg =
    sidebar_root ctx ++ Package.Name.to_string pkg ++ "sidebar.odoc-sidebar"
  ;;

  (* JSON sidebar file for web consumption - goes in HTML output *)
  let sidebar_json ctx mode pkg = html_root ctx mode ++ Package.Name.to_string pkg ++ "sidebar.json"

  (* Single remap file for all external dependencies *)
  let remap_file ctx = root ctx ++ "_remap" ++ "remap.txt"
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

  let target ctx output_subdir t odoc_file =
    match t with
    | Html -> Artifact.html_file ctx output_subdir odoc_file
    | Json -> Artifact.json_file ctx output_subdir odoc_file
  ;;

  let alias t ~dir =
    match t with
    | Html -> Alias.make Alias0.doc ~dir
    | Json -> Alias.make Alias0.doc_json ~dir
  ;;

  let toplevel_index_path format ctx mode =
    let base = Paths.toplevel_index ctx mode in
    match format with
    | Html -> base
    | Json -> Path.Build.extend_basename base ~suffix:".json"
  ;;
end

module Dep : sig
  (** [format_alias output mode ctx target] returns the alias that depends on all
      targets produced by odoc for [target] in output format [output] and doc mode. *)
  val format_alias
    :  Output_format.t
    -> Doc_mode.t
    -> Context.t
    -> target
    -> Alias.t

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
  let format_alias f mode ctx m =
    Output_format.alias f ~dir:(Paths.html ctx mode m)
  ;;

  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
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
           (* Installed library - add dependency on its .odoc-all alias *)
           let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let dir =
                Paths.root ctx
                ++ "_odoc"
                ++ Package.Name.to_string lib_pkg
                ++ Lib_name.to_string (Lib.name lib)
              in
              Dep.Set.add acc (Dep.alias (alias ~dir))
            | None -> acc)
         | Some local_lib ->
           (* Local library - add dependency on its .odoc-all alias *)
           let lib_t = Lib.Local.to_lib local_lib in
           let info = Lib.info lib_t in
           let pkg =
             match Lib_info.package info with
             | Some p -> p
             | None ->
               (* v2 library - create synthetic package from lib_unique_name *)
               Package.Name.of_string (pkg_or_lnu local_lib)
           in
           let dir = Paths.odocs ctx (Lib (pkg, lib_t)) in
           Dep.Set.add acc (Dep.alias (alias ~dir))))
  ;;

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  let setup_deps ctx m files =
    let target_name =
      match m with
      | Lib (pkg, lib) ->
        "lib:" ^ Package.Name.to_string pkg ^ "/" ^ Lib_name.to_string (Lib.name lib)
      | Private_lib (lib_unique_name, lib) ->
        "private_lib:" ^ lib_unique_name ^ "/" ^ Lib_name.to_string (Lib.name lib)
      | Pkg pkg -> "pkg:" ^ Package.Name.to_string pkg
    in
    Log.info
      [ Pp.textf
          "Dep.setup_deps: Adding %d files to .odoc-all for %s"
          (Path.Set.cardinal files)
          target_name
      ];
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
  ;;
end

(* Remap generation helpers *)

(* Get list of local workspace packages *)
let get_workspace_packages () =
  let* packages = Dune_load.packages () in
  Memo.return (Package.Name.Map.keys packages)
;;

(* Get package version using Package_discovery *)
let get_package_version pkg_discovery pkg_name =
  Package_discovery.version_of_package pkg_discovery pkg_name
;;

(* Generate remap mappings for external (non-local) packages *)
let generate_remap_mappings pkg_discovery ~local_packages ~all_deps =
  let local_pkg_set = Package.Name.Set.of_list local_packages in
  (* Filter to non-local (external/installed) packages *)
  let external_deps =
    List.filter all_deps ~f:(fun target ->
      match target with
      | Pkg pkg_name -> not (Package.Name.Set.mem local_pkg_set pkg_name)
      | Lib (pkg_name, _lib) -> not (Package.Name.Set.mem local_pkg_set pkg_name)
      | Private_lib _ -> false (* Private libs have no package - they are LOCAL *))
  in
  (* Generate mappings: local_path:remote_url *)
  let* mappings =
    Memo.List.map external_deps ~f:(fun target ->
      match target with
      | Private_lib _ ->
        (* Private libs are local, should never appear in external_deps *)
        Memo.return []
      | Pkg pkg_name | Lib (pkg_name, _) ->
        (* Get package version and construct package URL *)
        let* version_opt = get_package_version pkg_discovery pkg_name in
        let version = Option.value version_opt ~default:"latest" in
        let pkg_path = Package.Name.to_string pkg_name in
        let pkg_url =
          Printf.sprintf "https://ocaml.org/p/%s/%s/doc/" pkg_path version
        in
        (* Both package and library entries point to the same package URL.
           Odoc appends library paths automatically when resolving links. *)
        let pkg_mapping = pkg_path, pkg_url in
        (match target with
         | Lib (_, lib) ->
           let lib_path = pkg_path ^ "/" ^ Lib_name.to_string (Lib.name lib) in
           (* Library also maps to package URL, matching odoc_driver behavior *)
           Memo.return [ pkg_mapping; lib_path, pkg_url ]
         | _ -> Memo.return [ pkg_mapping ]))
  in
  Memo.return (List.concat mappings)
;;

(* Write remap file with given mappings *)
let write_remap_file sctx ~remap_file ~mappings =
  let contents =
    String.concat
      ~sep:"\n"
      (List.map mappings ~f:(fun (local, remote) -> Printf.sprintf "%s:%s" local remote))
  in
  add_rule sctx (Action_builder.write_file remap_file contents)
;;

let odoc_ext = ".odoc"

module Mld : sig
  type t

  val create : path:Path.Build.t -> name:string -> t
  val odoc_file : doc_dir:Path.Build.t -> t -> Path.Build.t
  val odoc_input : t -> Path.Build.t
end = struct
  (** The [(documentation (files ...))] stanza allows with the [as] keyword to
      distinguish the input file and the path in the documentation. Here we do
      not support layered hierarchy, but we do support changing the name (hence
      the two fields) *)
  type t =
    { path : Path.Build.t
    ; name : string (** The name of the mld compilation unit (without extension) *)
    }

  let create ~path ~name = { path; name }

  let odoc_file ~doc_dir { name; _ } =
    Path.Build.relative doc_dir (sprintf "page-%s%s" name odoc_ext)
  ;;

  let odoc_input { path; _ } = path
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

let odoc_dev_tool_exe_path_building_if_necessary () =
  let open Action_builder.O in
  let path = Path.build (Pkg_dev_tool.exe_path Odoc) in
  let+ () = Action_builder.path path in
  Ok path
;;

let odoc_program sctx dir =
  let odoc_dev_tool_lock_dir_exists =
    match Config.get Compile_time.lock_dev_tools with
    | `Enabled -> true
    | `Disabled -> false
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
;;

let library_dir_v3 ctx pkg lib =
  Path.Build.relative (package_dir_v3 ctx pkg) (Lib_name.to_string lib)
;;

let odoc_file_v3 ctx pkg lib module_ =
  let parent_dir = package_dir_v3 ctx pkg in
  let parent_id_path = Path.Build.relative parent_dir (Lib_name.to_string lib) in
  (* Use obj_name to get the wrapped module name (e.g., stdune__User_message) *)
  let basename =
    Module.obj_name module_ |> Module_name.Unique.artifact_filename ~ext:".odoc"
  in
  Path.Build.relative parent_id_path basename
;;

(* Parent ID computation helpers *)
let parent_id_of_module pkg lib =
  Printf.sprintf "%s/%s" (Package.Name.to_string pkg) (Lib_name.to_string lib)
;;

let parent_id_of_library pkg = Package.Name.to_string pkg
let parent_id_root = ""

(* Get the stdlib library, if available *)
let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

(* Memoized helper to get the odoc directory for a library name.
   This handles both local and installed libraries, and both v2 and v3 libraries. *)
let odoc_dir_of_lib_name =
  let module Input = struct
    type t = Context.t * Lib_name.t

    let equal (ctx1, lib1) (ctx2, lib2) =
      Context.equal ctx1 ctx2 && Lib_name.equal lib1 lib2
    ;;

    let hash (ctx, lib) = Tuple.T2.hash Context.hash Lib_name.hash (ctx, lib)

    let to_dyn (ctx, lib) =
      Dyn.record
        [ "context", Context.name ctx |> Context_name.to_dyn; "lib", Lib_name.to_dyn lib ]
    ;;
  end
  in
  let f (ctx, lib_name) =
    (* First check if it's a local library *)
    let* lib_db = Scope.DB.public_libs (Context.name ctx) in
    let* lib_opt = Lib.DB.find lib_db lib_name in
    match lib_opt with
    | Some lib ->
      (* Found as local library - check if it has a package *)
      let info = Lib.info lib in
      let pkg_opt = Lib_info.package info in
      (match pkg_opt with
       | Some pkg ->
         (* v3 library with package: _odoc/pkg_name/lib_name *)
         let pkg_str = Package.Name.to_string pkg in
         let lib_str = Lib_name.to_string lib_name in
         Memo.return (Paths.root ctx ++ "_odoc" ++ pkg_str ++ lib_str)
       | None ->
         (* v2 library without package: _odoc/lib_unique_name *)
         let status = Lib_info.status info in
         let lib_unique_name =
           match status with
           | Lib_info.Status.Private (project, _) -> Scope_key.to_string lib_name project
           | _ -> Lib_name.to_string lib_name
         in
         Memo.return (Paths.root ctx ++ "_odoc" ++ lib_unique_name))
    | None ->
      (* Not a local library - check if it's installed *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let pkg_opt = Package_discovery.package_of_lib_name pkg_discovery lib_name in
      (match pkg_opt with
       | Some pkg ->
         (* Installed library: _odoc/pkg_name/lib_name *)
         let pkg_str = Package.Name.to_string pkg in
         let lib_str = Lib_name.to_string lib_name in
         Memo.return (Paths.root ctx ++ "_odoc" ++ pkg_str ++ lib_str)
       | None ->
         (* Not found - use lib name as fallback *)
         let lib_str = Lib_name.to_string lib_name in
         Memo.return (Paths.root ctx ++ "_odoc" ++ lib_str))
  in
  Memo.create "odoc-dir-of-lib-name" ~input:(module Input) f
;;

(* Prevent unused value warnings until we integrate these functions *)
let () =
  ignore library_dir_v3;
  ignore odoc_file_v3;
  ignore parent_id_of_module;
  ignore parent_id_of_library;
  ignore parent_id_root
;;

(* Old unused compile functions removed - now using unified compile_artifact *)

let odoc_include_flags ctx pkg ~stdlib_opt requires pkg_discovery =
  (* Debug: inspect what's in requires at the start *)
  let () =
    match Resolve.peek requires with
    | Ok libs_list ->
      let lib_names =
        List.map libs_list ~f:(fun lib -> Lib_name.to_string (Lib.name lib))
      in
      Log.info
        [ Pp.textf
            "odoc_include_flags: Called with %d libs: %s"
            (List.length libs_list)
            (String.concat ~sep:", " lib_names)
        ]
    | Error _ -> Log.info [ Pp.textf "odoc_include_flags: Called with Error requires" ]
  in
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     (* Add stdlib to the list of libraries if provided and not already present *)
     let libs =
       match stdlib_opt with
       | Some stdlib ->
         if
           List.exists libs ~f:(fun lib ->
             Lib_name.equal (Lib.name lib) (Lib.name stdlib))
         then libs (* stdlib already in list, don't add it again *)
         else stdlib :: libs
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
           Log.info
             [ Pp.textf
                 "odoc_include_flags: Processing installed library %s"
                 (Lib_name.to_string lib_name)
             ];
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let installed_odoc_path =
                Paths.root ctx
                ++ "_odoc"
                ++ Package.Name.to_string lib_pkg
                ++ Lib_name.to_string lib_name
              in
              Log.info
                [ Pp.textf
                    "odoc_include_flags: Adding include path for %s (opam pkg=%s): %s"
                    (Lib_name.to_string lib_name)
                    (Package.Name.to_string lib_pkg)
                    (Path.Build.to_string installed_odoc_path)
                ];
              Path.Set.add paths (Path.build installed_odoc_path)
            | None ->
              Log.info
                [ Pp.textf
                    "odoc_include_flags: Library %s has no opam package, skipping"
                    (Lib_name.to_string lib_name)
                ];
              paths)
         | Some local_lib ->
           let lib_t = Lib.Local.to_lib local_lib in
           let info = Lib.info lib_t in
           let pkg =
             match Lib_info.package info with
             | Some p -> p
             | None ->
               (* v2 library - create synthetic package from lib_unique_name *)
               Package.Name.of_string (pkg_or_lnu local_lib)
           in
           Path.Set.add paths (Path.build (Paths.odocs ctx (Lib (pkg, lib_t)))))
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
let odoc_lib_flags _ctx ~stdlib_opt requires pkg_discovery =
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     (* Add stdlib to the list of libraries if provided and not already present *)
     let libs =
       match stdlib_opt with
       | Some stdlib ->
         if
           List.exists libs ~f:(fun lib ->
             Lib_name.equal (Lib.name lib) (Lib.name stdlib))
         then libs (* stdlib already in list, don't add it again *)
         else stdlib :: libs
       | None -> libs
     in
     (* Build a map to deduplicate by library name *)
     let lib_paths_map =
       List.fold_left libs ~init:Lib_name.Map.empty ~f:(fun acc lib ->
         let lib_name = Lib.name lib in
         let lib_name_str = Lib_name.to_string lib_name in
         (* Skip if already in map *)
         if Lib_name.Map.mem acc lib_name
         then acc
         else (
           match Lib.Local.of_lib lib with
           | None ->
             (* Installed library - use _odoc/{package}/{library} path *)
             let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
             (match lib_pkg_opt with
              | Some lib_pkg ->
                (* Get the library's odoc directory path using Paths.odocs *)
                let target = Lib (lib_pkg, lib) in
                let odoc_dir = Paths.odocs _ctx target in
                (* Make path relative to html_root (_doc/_html)
                   Paths.odocs returns _build/default/_doc/_odoc/pkg/lib
                   We need ../_odoc/pkg/lib relative to _build/default/_doc/_html *)
                let odoc_path = Path.Build.to_string odoc_dir in
                let odoc_path_rel =
                  match String.drop_prefix odoc_path ~prefix:"_build/default/_doc/" with
                  | Some suffix -> "../" ^ suffix
                  | None -> odoc_path (* Fallback to absolute if prefix doesn't match *)
                in
                let lib_path_arg = lib_name_str ^ ":" ^ odoc_path_rel in
                Lib_name.Map.set acc lib_name lib_path_arg
              | None -> acc)
           | Some local_lib ->
             (* Local library - use library's odoc path *)
             let lib_pkg = Lib_info.package (Lib.Local.info local_lib) in
             (match lib_pkg with
              | Some pkg ->
                (* Get the library's odoc directory path using Paths.odocs *)
                let target = Lib (pkg, lib) in
                let odoc_dir = Paths.odocs _ctx target in
                (* Make path relative to html_root (_doc/_html)
                   Paths.odocs returns _build/default/_doc/_odoc/... *)
                let odoc_path = Path.Build.to_string odoc_dir in
                let odoc_path_rel =
                  match String.drop_prefix odoc_path ~prefix:"_build/default/_doc/" with
                  | Some suffix -> "../" ^ suffix
                  | None -> odoc_path (* Fallback to absolute if prefix doesn't match *)
                in
                let lib_path_arg = lib_name_str ^ ":" ^ odoc_path_rel in
                Lib_name.Map.set acc lib_name lib_path_arg
              | None -> acc)))
     in
     (* Convert map to args *)
     let lib_args =
       Lib_name.Map.values lib_paths_map
       |> List.concat_map ~f:(fun lib_path_arg -> [ Command.Args.A "-L"; A lib_path_arg ])
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
let odoc_pkg_flags _ctx requires pkg_discovery ~current_pkg =
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
           (* Path relative to html_root (_doc/_html): ../_odoc/pkg *)
           let odoc_path = "../_odoc/" ^ pkg_name_str in
           Package.Name.Map.set acc pkg odoc_path
         | None -> acc)
     in
     (* Also add package dependencies from odoc-config.sexp *)
     let config_pkg_deps = get_config_package_deps pkg_discovery current_pkg in
     let all_pkg_paths =
       List.fold_left config_pkg_deps ~init:lib_pkg_paths ~f:(fun acc pkg ->
         let pkg_name_str = Package.Name.to_string pkg in
         (* Path relative to html_root (_doc/_html): ../_odoc/pkg *)
         let odoc_path = "../_odoc/" ^ pkg_name_str in
         Package.Name.Map.set acc pkg odoc_path)
     in
     (* Also add the current package itself if present - odoc requires --current-package
        to have a corresponding -P flag *)
     let all_pkg_paths =
       match current_pkg with
       | None -> all_pkg_paths
       | Some pkg ->
         let pkg_name_str = Package.Name.to_string pkg in
         (* Path relative to html_root (_doc/_html): ../_odoc/pkg *)
         let odoc_path = "../_odoc/" ^ pkg_name_str in
         Package.Name.Map.set all_pkg_paths pkg odoc_path
     in
     let pkg_args =
       Package.Name.Map.to_list_map all_pkg_paths ~f:(fun pkg path ->
         let pkg_name_str = Package.Name.to_string pkg in
         let pkg_path_arg = pkg_name_str ^ ":" ^ path in
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
  (* Get all packages in the workspace to pass as --warnings-tags *)
  let* packages = Dune_load.packages () in
  let all_pkg_names =
    Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
  in
  (* Build --warnings-tags arguments for all packages *)
  let warnings_tags_args =
    Command.Args.S
      (List.concat_map all_pkg_names ~f:(fun pkg_name ->
         [ Command.Args.A "--warnings-tags"; Command.Args.A pkg_name ]))
  in
  (* Suppress output for installed packages and vendored libraries *)
  let* quiet =
    match Artifact.source odoc_file with
    | Installed_source _ -> Memo.return true
    | Local_source _ ->
      (* Check if this is a vendored library *)
      (match Artifact.target odoc_file with
       | Lib (_, lib) | Private_lib (_, lib) -> is_lib_vendored lib
       | Pkg _ -> Memo.return false)
  in
  (* Add -L flag for the library itself so modules can reference each other,
     but only if the library isn't already in the requires list or stdlib.
     odoc_lib_flags already handles libraries in requires + stdlib. *)
  let* self_lib_flag =
    match Artifact.target odoc_file with
    | Lib (_, lib) | Private_lib (_, lib) ->
      let* libs_with_flags = Resolve.read_memo requires in
      (* Check if this library is already in requires or is stdlib *)
      let is_already_included =
        List.exists libs_with_flags ~f:(fun req_lib ->
          Lib_name.equal (Lib.name req_lib) (Lib.name lib))
        ||
        match stdlib_opt with
        | Some stdlib -> Lib_name.equal (Lib.name stdlib) (Lib.name lib)
        | None -> false
      in
      if is_already_included
      then Memo.return (Command.Args.S [])
      else (
        let lib_name_str = Lib_name.to_string (Artifact.lib_name odoc_file) in
        (* Get the library's odoc directory path using Paths.odocs *)
        let odoc_dir = Paths.odocs ctx (Artifact.target odoc_file) in
        (* Make path relative to html_root (_doc/_html)
           Paths.odocs returns _build/default/_doc/_odoc/... *)
        let odoc_path = Path.Build.to_string odoc_dir in
        let odoc_path_rel =
          match String.drop_prefix odoc_path ~prefix:"_build/default/_doc/" with
          | Some suffix -> "../" ^ suffix
          | None -> odoc_path (* Fallback to absolute if prefix doesn't match *)
        in
        let lib_path_arg = lib_name_str ^ ":" ^ odoc_path_rel in
        Memo.return (Command.Args.S [ Command.Args.A "-L"; A lib_path_arg ]))
    | Pkg _ -> Memo.return (Command.Args.S [])
  in
  let run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build (Paths.html_root ctx Doc_mode.Local_only))
      "link"
      ~quiet
      ~flags_for:(Some (Artifact.odoc_file odoc_file))
      [ odoc_include_flags ctx pkg ~stdlib_opt requires pkg_discovery
      ; odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery
      ; self_lib_flag (* Add -L for the library being linked *)
      ; odoc_pkg_flags ctx requires pkg_discovery ~current_pkg:(Artifact.pkg odoc_file)
      ; (* Add --current-package flag when we have a package *)
        (match Artifact.pkg odoc_file with
         | Some pkg_name ->
           Command.Args.As [ "--current-package"; Package.Name.to_string pkg_name ]
         | None -> Command.Args.S [])
      ; A "--enable-missing-root-warning"
      ; warnings_tags_args
      ; A "-o"
      ; Target (Artifact.odocl_file odoc_file)
      ; Dep (Path.build (Artifact.odoc_file odoc_file))
      ]
  in
  (* For installed packages or vendored libraries, suppress output (both stdout and stderr) *)
  let* should_suppress =
    match Artifact.source odoc_file with
    | Installed_source _ -> Memo.return true
    | Local_source _ ->
      (* Check if this is a vendored library *)
      (match Artifact.target odoc_file with
       | Lib (_, lib) | Private_lib (_, lib) -> is_lib_vendored lib
       | Pkg _ -> Memo.return false)
  in
  let run_odoc =
    if should_suppress
    then
      Action_builder.With_targets.map run_odoc ~f:(fun action ->
        Action.Full.map action ~f:Action.ignore_outputs)
    else run_odoc
  in
  add_rule
    sctx
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets deps >>> run_odoc)
;;

(* Unified compilation function that computes all dependencies from the artifact.
   This replaces the separate compile_module, compile_mld, compile_installed_module_artifact functions.
   All information needed for compilation is now in the artifact itself.

   For Module artifacts, we use `odoc compile-deps` to determine intra-library module dependencies.
   For Page artifacts (mld files), we compile directly without module dependencies. *)
let compile_artifact sctx ~artifact ~lib_artifacts =
  let ctx = Super_context.context sctx in
  let compile_dir = Context.build_dir ctx in
  let source_file =
    match Artifact.source artifact with
    | Local_source path -> Path.build path
    | Installed_source { src_path; _ } -> src_path
  in
  (* For Module artifacts, run compile-deps to find intra-library module dependencies *)
  let* module_deps =
    match Artifact.kind artifact with
    | Page _ ->
      (* Pages don't have module dependencies *)
      Memo.return (Action_builder.return ())
    | Module { module_name; _ } ->
      (* Generate deps file using odoc compile-deps *)
      let module_name_str = Module_name.to_string module_name in
      let deps_file =
        Path.Build.relative (Artifact.output_dir artifact) (module_name_str ^ ".deps")
      in
      let program = odoc_program sctx (Context.build_dir ctx) in
      (* Generate compile-deps rule *)
      let* () =
        let run_compile_deps =
          Command.run_dyn_prog
            program
            ~dir:(Path.build (Context.build_dir ctx))
            ~stdout_to:deps_file
            [ A "compile-deps"; Dep source_file ]
        in
        add_rule sctx run_compile_deps
      in
      (* Parse deps file and create dependencies on .odoc files.
         NOTE: odoc compile-deps returns ALL module dependencies, including from other libraries.
         However, inter-library dependencies are already handled by lib_deps below via Dep.deps,
         so here we ONLY handle intra-library dependencies (modules in the same library).

         We check if each dependency module is in Artifact.lib_modules artifact.
         If it's not found, it's an inter-library dependency and we skip it. *)
      Memo.return
        (let open Action_builder.O in
         let* lines = Action_builder.lines_of (Path.build deps_file) in
         let dep_modules =
           List.filter_map lines ~f:(fun line ->
             match String.split ~on:' ' line with
             | [ m; _hash ] -> Some (Module_name.of_string m)
             | _ -> None)
         in
         (* Find .odoc files for dependencies in the same library by matching the exact
           module name from odoc compile-deps against artifact filenames.

           odoc compile-deps returns the true module names as they appear in .cmt files:
           - For local wrapped modules: "Odoc_xref2__Subst" (wrapped name)
           - For external modules: "Subst" (from compiler), "Stdlib__List", etc.

           We match by checking if the dependency name appears in the artifact's odoc filename,
           which correctly distinguishes between our "Odoc_xref2__Subst" and the compiler's "Subst". *)
         let current_artifact_basename =
           Path.Build.basename (Artifact.odoc_file artifact) |> Filename.remove_extension
         in
         let dep_odoc_files =
           List.filter_map dep_modules ~f:(fun dep_module ->
             let dep_module_str = Module_name.to_string dep_module in
             let dep_module_lower = String.lowercase_ascii dep_module_str in
             (* Skip self-dependencies *)
             if
               String.equal
                 (String.lowercase_ascii current_artifact_basename)
                 dep_module_lower
             then None
             else
               (* Look for an artifact whose .odoc filename matches this dependency name.
                 For "Odoc_xref2__Subst", we'll find "odoc_xref2__Subst.odoc".
                 For compiler's "Subst", we won't find a match (no such artifact). *)
               List.find_map lib_artifacts ~f:(fun dep_artifact ->
                 let artifact_basename =
                   Path.Build.basename (Artifact.odoc_file dep_artifact)
                   |> Filename.remove_extension
                 in
                 (* Match case-insensitively since filenames use lowercase *)
                 if
                   String.equal
                     (String.lowercase_ascii artifact_basename)
                     dep_module_lower
                 then Some (Path.build (Artifact.odoc_file dep_artifact))
                 else None))
         in
         Dune_engine.Dep.Set.of_files dep_odoc_files |> Action_builder.deps)
  in
  (* Compute library dependencies from the artifact's target *)
  (* Get stdlib for dependency resolution *)
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  (* Check if this artifact is part of stdlib *)
  let is_stdlib_artifact =
    match stdlib_opt with
    | Some stdlib -> Lib_name.equal (Lib.name stdlib) (Artifact.lib_name artifact)
    | None -> false
  in
  (* Get TRANSITIVE closure of dependencies (not just direct requires) *)
  let* requires_closure =
    match Artifact.target artifact with
    | Lib (_, lib) | Private_lib (_, lib) ->
      (* Include stdlib in the closure unless we're compiling stdlib itself *)
      let libs_to_close =
        if is_stdlib_artifact then [ lib ] else lib :: Option.to_list stdlib_opt
      in
      Lib.closure libs_to_close ~linking:false
    | Pkg _ -> Memo.return (Resolve.return [])
    (* Package-level artifacts have no library dependencies *)
  in
  (* Filter out the library itself from the closure *)
  let requires_from_deps =
    match Artifact.target artifact with
    | Lib (_, lib) | Private_lib (_, lib) ->
      Resolve.map requires_closure ~f:(fun all_libs ->
        List.filter all_libs ~f:(fun dep_lib ->
          not (Lib_name.equal (Lib.name dep_lib) (Lib.name lib))))
    | Pkg _ -> requires_closure
  in
  let requires = requires_from_deps in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  (* Create dependencies on all required libraries' .odoc files (via .odoc-all aliases)
     IMPORTANT: Pass None for pkg during compilation to avoid creating a dependency cycle
     on our own package's .odoc-all alias. The package alias is only needed during linking. *)
  let lib_deps = Dep.deps ctx None requires in
  let run_odoc =
    let open Action_builder.With_targets.O in
    (* Suppress output for installed packages *)
    let quiet =
      match Artifact.source artifact with
      | Installed_source _ -> true
      | Local_source _ -> false
    in
    (* Depend on: 1) intra-library module deps, 2) inter-library deps *)
    Action_builder.with_no_targets module_deps
    >>> Action_builder.with_no_targets lib_deps
    >>> Action_builder.With_targets.add
          ~file_targets:[ Artifact.odoc_file artifact ]
          (run_odoc
             sctx
             ~dir:(Path.build compile_dir)
             "compile"
             ~quiet
             ~flags_for:(Some (Artifact.odoc_file artifact))
             [ (* Include paths for all dependency libraries including stdlib *)
               odoc_include_flags ctx None ~stdlib_opt requires pkg_discovery
             ; (* Add -I for current library directory so modules can reference each other *)
               (match Artifact.target artifact with
                | Lib (pkg, lib) ->
                  let lib_dir = Paths.odocs ctx (Lib (pkg, lib)) in
                  Command.Args.S
                    [ Command.Args.A "-I"; Command.Args.Path (Path.build lib_dir) ]
                | Private_lib (lib_unique_name, lib) ->
                  let lib_dir = Paths.odocs ctx (Private_lib (lib_unique_name, lib)) in
                  Command.Args.S
                    [ Command.Args.A "-I"; Command.Args.Path (Path.build lib_dir) ]
                | Pkg _ -> Command.Args.S [])
             ; Command.Args.A "--output-dir"
             ; Command.Args.A "_doc/_odoc"
             ; Command.Args.A "--parent-id"
             ; Command.Args.A (Artifact.parent_id artifact)
             ; Command.Args.A "--enable-missing-root-warning"
             ; (* Add --unique-id and --warnings-tag flags for library artifacts.
             Both use the package name to identify which package the module belongs to. *)
               (match Artifact.target artifact with
                | Lib (_, lib) | Private_lib (_, lib) ->
                  let pkg_name = lib_unique_id_string lib in
                  Command.Args.As [ "--unique-id"; pkg_name; "--warnings-tag"; pkg_name ]
                | Pkg _ ->
                  (* Package-level artifacts don't have a library unique ID *)
                  Command.Args.S [])
             ; Command.Args.Dep source_file
             ])
  in
  (* For installed packages or vendored libraries, suppress output (both stdout and stderr) *)
  let* should_suppress =
    match Artifact.source artifact with
    | Installed_source _ -> Memo.return true
    | Local_source _ ->
      (* Check if this is a vendored library *)
      (match Artifact.target artifact with
       | Lib (_, lib) | Private_lib (_, lib) -> is_lib_vendored lib
       | Pkg _ -> Memo.return false)
  in
  let run_odoc =
    if should_suppress
    then
      Action_builder.With_targets.map run_odoc ~f:(fun action ->
        Action.Full.map action ~f:Action.ignore_outputs)
    else run_odoc
  in
  add_rule sctx run_odoc
;;

(* Unified HTML generation function for artifacts.
   Takes an artifact, search_db, and optional sidebar file, generates HTML for it.
   This follows the same pattern as compile_artifact and link_artifact.
   Mode parameter determines output directory and whether to use remap file. *)
let generate_html_artifact
      sctx
      ~artifact
      ~search_db
      ~sidebar_file
      ?(remap_file : Path.Build.t option = None)
      ?(mode = Doc_mode.Local_only)
      ()
  =
  let ctx = Super_context.context sctx in
  let html_root = Paths.html_root ctx mode in
  let odoc_support_path = Paths.odoc_support ctx mode in
  let output_subdir = Doc_mode.output_subdir mode in
  let search_args = Sherlodoc.odoc_args sctx ~search_db ~dir_sherlodoc_dot_js:html_root in
  (* Generate HTML for all output formats *)
  Memo.List.iter Output_format.all ~f:(fun out ->
    let html_file = Output_format.target ctx output_subdir out artifact in
    Log.info
      [ Pp.textf
          "odoc v3: generate_html_artifact for html_file=%s"
          (Path.Build.to_string html_file)
      ];
    (* Check if the HTML file is in a subdirectory (v3 path for modules) or not (v2 path or package mlds) *)
    let html_dir_opt =
      let html_dir = Path.Build.parent_exn html_file in
      if Path.Build.equal html_dir html_root then None else Some html_dir
    in
    (* Suppress output for installed packages *)
    let quiet =
      match Artifact.source artifact with
      | Installed_source _ -> true
      | Local_source _ -> false
    in
    let run_odoc =
      run_odoc
        sctx
        ~dir:(Path.build html_root)
        "html-generate"
        ~quiet
        ~flags_for:None
        [ search_args
        ; A "-o"
        ; Path (Path.build html_root)
        ; A "--support-uri"
        ; Path (Path.build odoc_support_path)
        ; A "--theme-uri"
        ; Path (Path.build odoc_support_path)
        ; (match remap_file with
           | None -> S []
           | Some rf -> S [ A "--remap-file"; Dep (Path.build rf) ])
        ; (match sidebar_file with
           | Some sf -> S [ A "--sidebar"; Dep (Path.build sf) ]
           | None -> S [])
        ; Dep (Path.build (Artifact.odocl_file artifact))
        ; Output_format.args out
        ; (match html_dir_opt with
           | None -> Hidden_targets [ html_file ]
           | Some _ -> Command.Args.empty)
        ]
    in
    (* For installed packages or vendored libraries, suppress output (both stdout and stderr) *)
    let* should_suppress =
      match Artifact.source artifact with
      | Installed_source _ -> Memo.return true
      | Local_source _ ->
        (* Check if this is a vendored library *)
        (match Artifact.target artifact with
         | Lib (_, lib) | Private_lib (_, lib) -> is_lib_vendored lib
         | Pkg _ -> Memo.return false)
    in
    let run_odoc =
      if should_suppress
      then
        Action_builder.With_targets.map run_odoc ~f:(fun action ->
          Action.Full.map action ~f:Action.ignore_outputs)
      else run_odoc
    in
    (* Add explicit dependency on CSS/support files *)
    let rule =
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets (Action_builder.path (Path.build odoc_support_path))
      >>> Action_builder.With_targets.add ~file_targets:[ html_file ] run_odoc
    in
    add_rule sctx rule)
;;

let setup_css_rule sctx ~mode =
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx mode in
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

let setup_toplevel_index_rule sctx output mode =
  let* packages = Dune_load.packages () in
  let index = Toplevel_index.of_packages packages in
  let content = Toplevel_index.content output index in
  let ctx = Super_context.context sctx in
  let path = Output_format.toplevel_index_path output ctx mode in
  add_rule sctx (Action_builder.write_file path content)
;;

let setup_toplevel_index_rules sctx mode =
  Output_format.iter ~f:(fun output -> setup_toplevel_index_rule sctx output mode)
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

(* Compute requires for linking an artifact.
   For modules: use the library's requires + the library itself
   For pages: use all libraries in the package (since pages document the whole package)
   Also includes extra libraries from odoc-config.sexp if present. *)
let compute_link_requires sctx ~artifact =
  let ctx = Super_context.context sctx in
  let* base_requires =
    match Artifact.kind artifact, Artifact.target artifact with
    | Module _, Lib (_, lib) | Module _, Private_lib (_, lib) ->
      (* Module in a library: use library's dependencies PLUS the library itself.
       This ensures all modules in the library (including hidden/wrapped ones) are compiled
       before any module is linked. Critical for wrapped libraries. *)
      let* external_requires = Lib.requires lib in
      Memo.return
        (Resolve.bind external_requires ~f:(fun libs ->
           (* Add the library itself to ensure .odoc-all dependency includes all modules *)
           Resolve.return (lib :: libs)))
    | Page { pkg_libs; _ }, Pkg _ ->
      (* Page in a package: use the libraries that were recorded when the artifact was created *)
      Memo.return (Resolve.return pkg_libs)
    | Module _, Pkg _ ->
      (* This shouldn't happen - modules should have Lib targets *)
      Memo.return (Resolve.return [])
    | Page { pkg_libs = _; _ }, Lib (_, lib) | Page { pkg_libs = _; _ }, Private_lib (_, lib) ->
      (* Page in a library target - just use that library *)
      Memo.return (Resolve.return [ lib ])
  in
  (* Add extra libraries from odoc_config if present *)
  let odoc_config = Artifact.odoc_config artifact in
  let extra_lib_names = odoc_config.Odoc_config.deps.libraries in
  let extra_pkg_names = odoc_config.Odoc_config.deps.packages in
  if List.is_empty extra_lib_names && List.is_empty extra_pkg_names
  then Memo.return base_requires
  else
    (* Look up the extra libraries directly specified *)
    let* lib_db = Lib.DB.installed ctx in
    let* extra_libs_from_names =
      Memo.parallel_map extra_lib_names ~f:(fun lib_name -> Lib.DB.find lib_db lib_name)
      >>| List.filter_map ~f:Fun.id
    in
    (* Get all libraries from extra packages using Package_discovery *)
    let* extra_libs_from_pkgs =
      match Artifact.pkg artifact with
      | None -> Memo.return []
      | Some _pkg ->
        let* pkg_discovery = Package_discovery.create ~context:ctx in
        Memo.parallel_map extra_pkg_names ~f:(fun pkg_name ->
          Memo.return (Package_discovery.libraries_of_package pkg_discovery pkg_name))
        >>| List.concat
    in
    let all_extra_libs = extra_libs_from_names @ extra_libs_from_pkgs in
    (* Combine base requires with extra libraries *)
    Memo.return
      (Resolve.bind base_requires ~f:(fun base_libs ->
         Resolve.return (base_libs @ all_extra_libs)))
;;

(* Unified linking function that works for all artifact types.
   This follows the driver's pattern where all information needed to link
   is derived from the artifact itself. *)
let link_artifact sctx ~artifact =
  let* requires = compute_link_requires sctx ~artifact in
  (* Call the existing link_odoc_rules with computed requires *)
  link_odoc_rules sctx artifact ~pkg:(Artifact.pkg artifact) ~requires
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
let create_artifact_local ctx ~target ~source ~kind ~odoc_config ~lib_modules =
  let doc_root = Paths.root ctx in
  Artifact.create
    ~doc_root
    ~kind
    ~source:(Local_source source)
    ~target
    ~odoc_config
    ~lib_modules
;;

(* Create an artifact for an installed library module *)
let create_artifact_installed
      ctx
      ~pkg
      ~lib
      ~module_name
      ~archive
      ~visible
      ~src_path
      ~odoc_config
      ~lib_modules
  =
  let doc_root = Paths.root ctx in
  let kind = Module { visible; module_name = Module_name.of_string module_name } in
  let target = Lib (pkg, lib) in
  Artifact.create
    ~doc_root
    ~kind
    ~source:(Installed_source { src_path; module_name; archive })
    ~target
    ~odoc_config
    ~lib_modules
;;

(* Create an artifact for an installed package mld file *)
let create_artifact_installed_mld ctx ~pkg ~mld_path ~page_name ~odoc_config ~pkg_libs =
  let doc_root = Paths.root ctx in
  let pkg_name_str = Package.Name.to_string pkg in
  let kind = Page { name = page_name; pkg_libs } in
  let target = Pkg pkg in
  Artifact.create
    ~doc_root
    ~kind
    ~source:
      (Installed_source
         { src_path = mld_path; module_name = page_name; archive = pkg_name_str })
    ~target
    ~odoc_config
    ~lib_modules:Module_name.Set.empty
;;

(* Discover mld files for an installed package and create artifacts *)
let discover_installed_pkg_mld_artifacts ctx ~pkg ~pkg_libs : artifact list Memo.t =
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let mld_files = Package_discovery.mlds_of_package pkg_discovery pkg in
  Memo.List.filter_map mld_files ~f:(fun mld_path ->
    (* Extract hierarchical path from full path by looking for "odoc-pages/" prefix
       Example: /path/to/doc/odoc/odoc-pages/deprecated/index.mld
       We want to extract "deprecated/index" *)
    let path_str = Path.to_string mld_path in
    (* Split on "/" and find "odoc-pages" to get the relative path after it *)
    let parts = String.split path_str ~on:'/' in
    let page_name_with_path =
      match List.drop_while parts ~f:(fun s -> not (String.equal s "odoc-pages")) with
      | "odoc-pages" :: rest ->
        (* Join the parts after odoc-pages and remove .mld extension *)
        let relative_path = String.concat ~sep:"/" rest in
        (match String.drop_suffix relative_path ~suffix:".mld" with
         | Some n -> n
         | None -> relative_path)
      | _ ->
        (* Fallback to basename if pattern not found *)
        let mld_basename = Path.basename mld_path in
        (match String.drop_suffix mld_basename ~suffix:".mld" with
         | Some n -> n
         | None -> mld_basename)
    in
    (* Include all mld files, including index.mld if hand-written *)
    let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
    Memo.return
      (Some
         (create_artifact_installed_mld
            ctx
            ~pkg
            ~mld_path
            ~page_name:page_name_with_path
            ~odoc_config
            ~pkg_libs)))
;;

(* Discover modules for an installed library and create artifacts *)
let discover_installed_lib_artifacts _sctx ctx ~pkg ~lib_name ~lib : artifact list Memo.t =
  let pkg_name_str = Package.Name.to_string pkg in
  let lib_name_str = Lib_name.to_string lib_name in
  (* Get Lib_info to access module information *)
  let info = Lib.info lib in
  (* Get archive information - this tells us which .cma/.cmxa belongs to this library *)
  let archives = Lib_info.archives info in
  let archive_names =
    let byte_archives = Mode.Dict.get archives Mode.Byte in
    match byte_archives with
    | [] ->
      if Lib_name.equal lib_name (Lib_name.of_string "stdlib") then [ "stdlib" ] else []
    | archives ->
      List.map archives ~f:(fun p -> Path.basename p |> Filename.remove_extension)
  in
  if List.is_empty archive_names
  then (
    Log.info
      [ Pp.textf
          "odoc v3: No archives found for installed library %s/%s, skipping"
          pkg_name_str
          lib_name_str
      ];
    Memo.return [])
  else (
    let default_archive = List.hd archive_names in
    (* Read the classify file to get ALL modules for this library's archive *)
    let classify_path =
      Paths.root ctx ++ "classify" ++ pkg_name_str ++ lib_name_str ++ "odoc.classify"
    in
    let* classify_content = Build_system.read_file (Path.build classify_path) in
    let classify_lines = String.split_lines classify_content in
    (* Parse classify output: each line is "archive_name Module1 Module2 ..." *)
    let all_module_names =
      List.concat_map classify_lines ~f:(fun line ->
        match
          String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s))
        with
        | [] -> []
        | archive :: mods ->
          (* Check if this line is for one of our archives *)
          if List.mem archive_names archive ~equal:String.equal then mods else [])
    in
    Log.info
      [ Pp.textf
          "odoc v3: Found %d modules for installed library %s/%s via odoc classify"
          (List.length all_module_names)
          pkg_name_str
          lib_name_str
      ];
    if List.is_empty all_module_names
    then Memo.return []
    else (
      (* Get entry modules to determine visibility *)
      let entry_modules_source = Lib_info.entry_modules info in
      let entry_module_names =
        match entry_modules_source with
        | Lib_info.Source.Local -> []
        | Lib_info.Source.External result ->
          (match result with
           | Error _msg -> []
           | Ok module_names -> List.map module_names ~f:Module_name.to_string)
      in
      (* Compute the set of all module names in this library *)
      let lib_modules =
        Module_name.Set.of_list (List.map all_module_names ~f:Module_name.of_string)
      in
      (* Get Package_discovery for source file resolution and config *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
      (* Create artifacts for ALL modules *)
      let+ all_module_artifacts =
        Memo.parallel_map all_module_names ~f:(fun module_name ->
          match Package_discovery.module_source_file pkg_discovery ~lib ~module_name with
          | Some src_path ->
            (* Determine visibility: entry modules are visible, others are hidden *)
            let visible = List.mem entry_module_names module_name ~equal:String.equal in
            Memo.return
              (Some
                 (create_artifact_installed
                    ctx
                    ~pkg
                    ~lib
                    ~module_name
                    ~archive:default_archive
                    ~visible
                    ~src_path
                    ~odoc_config
                    ~lib_modules))
          | None ->
            Log.info
              [ Pp.textf
                  "odoc v3: Could not find source file for module %s in %s/%s"
                  module_name
                  pkg_name_str
                  lib_name_str
              ];
            Memo.return None)
      in
      List.filter_map all_module_artifacts ~f:Fun.id))
;;

(* Create an artifact for a local library module *)
let create_artifact_local_module ctx ~pkg ~local_lib ~module_ ~odoc_config ~lib_modules =
  let doc_root = Paths.root ctx in
  let kind =
    Module
      { visible = Module.visibility module_ = Visibility.Public
      ; module_name = Module.name module_
      }
  in
  let lib_t = Lib.Local.to_lib local_lib in
  let target = Lib (pkg, lib_t) in
  let obj_dir = Lib.Local.obj_dir local_lib in
  let source_file = Obj_dir.Module.cmti_file obj_dir module_ ~cm_kind:(Ocaml Cmi) in
  Artifact.create
    ~doc_root
    ~kind
    ~source:(Local_source source_file)
    ~target
    ~odoc_config
    ~lib_modules
;;

(* Create an artifact for a v2 library module (library without package) *)
let create_artifact_v2_module
      ctx
      ~lib_unique_name
      ~local_lib
      ~module_
      ~odoc_config
      ~lib_modules
  =
  let doc_root = Paths.root ctx in
  let kind =
    Module
      { visible = Module.visibility module_ = Visibility.Public
      ; module_name = Module.name module_
      }
  in
  let lib_t = Lib.Local.to_lib local_lib in
  let target = Private_lib (lib_unique_name, lib_t) in
  let obj_dir = Lib.Local.obj_dir local_lib in
  let source_file = Obj_dir.Module.cmti_file obj_dir module_ ~cm_kind:(Ocaml Cmi) in
  Artifact.create
    ~doc_root
    ~kind
    ~source:(Local_source source_file)
    ~target
    ~odoc_config
    ~lib_modules
;;

(* Discover modules for a local library and create artifacts.
   Handles both v3 libraries (with packages) and v2 libraries (without packages). *)
let discover_local_lib_artifacts sctx ctx ~pkg ~lib_name ~local_lib : artifact list Memo.t
  =
  let* all_modules = Dir_contents.modules_of_local_lib sctx local_lib in
  let modules = Modules.fold all_modules ~init:[] ~f:(fun m acc -> m :: acc) in
  (* Compute the set of all module names in this library *)
  let lib_modules =
    modules |> List.map ~f:(fun m -> Module.name m) |> Module_name.Set.of_list
  in
  (* Check if this is a v2 library (no package) *)
  let info = Lib.Local.info local_lib in
  let actual_pkg = Lib_info.package info in
  (* Get odoc config for the package *)
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
  let artifacts =
    match actual_pkg with
    | None ->
      (* v2 library - use lib_unique_name for directory structure *)
      let status = Lib_info.status info in
      let lib_unique_name =
        match status with
        | Lib_info.Status.Private (project, _) -> Scope_key.to_string lib_name project
        | _ ->
          Lib_name.to_string lib_name (* Fallback, shouldn't happen for private libs *)
      in
      List.map modules ~f:(fun module_ ->
        create_artifact_v2_module
          ctx
          ~lib_unique_name
          ~local_lib
          ~module_
          ~odoc_config
          ~lib_modules)
    | Some _ ->
      (* v3 library - use pkg/lib directory structure *)
      List.map modules ~f:(fun module_ ->
        create_artifact_local_module
          ctx
          ~pkg
          ~local_lib
          ~module_
          ~odoc_config
          ~lib_modules)
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
    discover_installed_lib_artifacts sctx ctx ~pkg ~lib_name ~lib
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.rev_map mlds ~f:(fun ((_path, mld_name) as mld) -> mld_name, mld)
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, (p1, _name1), (p2, _name2)) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld's with the same name %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]
;;

(* Helper to group artifacts by library name *)
let group_artifacts_by_lib artifacts =
  List.fold_left artifacts ~init:Lib_name.Map.empty ~f:(fun acc artifact ->
    let lib_name = Artifact.lib_name artifact in
    let existing = Lib_name.Map.find acc lib_name |> Option.value ~default:[] in
    Lib_name.Map.set acc lib_name (artifact :: existing))
;;

(* Helper function to compile artifacts for a single library with proper dependencies *)
let compile_library_artifacts sctx _ctx ~pkg_name:_ ~lib_name ~lib_artifacts
  : Path.Build.t Memo.t
  =
  (* For both local and installed libraries, compile all artifacts and create .odoc-all alias *)
  let first_artifact = List.hd lib_artifacts in
  Log.info
    [ Pp.textf "odoc v3: compile_library_artifacts - examining first artifact's target" ];
  match Artifact.target first_artifact with
  | Pkg _ | Lib _ | Private_lib _ ->
    Log.info
      [ Pp.textf
          "odoc v3: compile_library_artifacts for lib=%s with %d artifacts"
          (Lib_name.to_string lib_name)
          (List.length lib_artifacts)
      ];
    (* Compile each artifact - each .odoc file depends on the CMT/CMTI source,
       not on other .odoc files. We rely on odoc's -I paths to find dependencies. *)
    let* () =
      Memo.parallel_iter lib_artifacts ~f:(fun artifact ->
        compile_artifact sctx ~artifact ~lib_artifacts)
    in
    (* Set up .odoc-all alias for this library.
       This alias is used by LINKING, not by compilation of other libraries.
       This avoids cycles during compilation. *)
    let odoc_files =
      List.map lib_artifacts ~f:(fun artifact -> Path.build (Artifact.odoc_file artifact))
    in
    Log.info
      [ Pp.textf
          "odoc v3: Creating .odoc-all alias with %d odoc files for lib=%s"
          (List.length odoc_files)
          (Lib_name.to_string lib_name)
      ];
    let odoc_path_set = Path.Set.of_list odoc_files in
    let lib_odoc_dir = Artifact.output_dir (List.hd lib_artifacts) in
    Log.info
      [ Pp.textf
          "odoc v3: Library .odoc-all alias dir: %s"
          (Path.Build.to_string lib_odoc_dir)
      ];
    let alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_odoc_dir in
    Log.info
      [ Pp.textf "odoc v3: Calling Rules.Produce.Alias.add_deps for library alias" ];
    let* () =
      Rules.Produce.Alias.add_deps alias (Action_builder.path_set odoc_path_set)
    in
    Log.info [ Pp.textf "odoc v3: Library alias registered successfully" ];
    (* Return the library's output directory *)
    Memo.return lib_odoc_dir
;;

(* Discover ALL artifacts for a package identifier (either a v3 package name or v2 lib_unique_name).
   For v3 packages: returns artifacts for all libraries in the package + package-level mld files
   For v2 libraries: returns artifacts for all modules in the library (no mld files since v2 has no packages)

   This is the unified entry point that all handlers (odoc, odocls, html) should use. *)
let discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name
  : (artifact list * string list) Memo.t
  =
  (* Check if this is a v2 library (contains '@') or a v3 package *)
  if String.contains pkg_or_lib_unique_name '@'
  then
    (* v2 library: lib_unique_name format like "dune_pkg@e6ee5b2bc981" *)
    let* lib_name, lib_db =
      Scope_key.of_string (Context.name ctx) pkg_or_lib_unique_name
    in
    let* lib_opt =
      let+ lib = Lib.DB.find lib_db lib_name in
      Option.bind ~f:Lib.Local.of_lib lib
    in
    match lib_opt with
    | None -> Memo.return ([], [])
    | Some local_lib ->
      (* Discover artifacts for this v2 library - discover_local_lib_artifacts will detect no package *)
      let dummy_pkg = Package.Name.of_string pkg_or_lib_unique_name in
      let+ artifacts =
        discover_local_lib_artifacts sctx ctx ~pkg:dummy_pkg ~lib_name ~local_lib
      in
      (* v2 libraries don't have subdirectories in the same sense as v3 packages *)
      artifacts, []
  else (
    (* v3 package: regular package name like "dyn" *)
    let pkg = Package.Name.of_string pkg_or_lib_unique_name in
    (* Check if this is a local project package or an installed package *)
    let* is_project_pkg =
      let* packages = Dune_load.packages () in
      Memo.return (Package.Name.Map.mem packages pkg)
    in
    if is_project_pkg
    then
      (* Local package - discover mld artifacts + all library artifacts *)
      let* local_libs = Context.name ctx |> libs_of_pkg ~pkg in
      (* Get library subdirectory names *)
      let lib_subdirs =
        List.map local_libs ~f:(fun local_lib ->
          Lib.name (Lib.Local.to_lib local_lib) |> Lib_name.to_string)
      in
      (* For local packages, get documentation dependencies from the package info *)
      let* odoc_config =
        let* packages = Dune_load.packages () in
        match Package.Name.Map.find packages pkg with
        | None -> Memo.return Odoc_config.empty
        | Some local_pkg ->
          let doc = Package.info local_pkg |> Package_info.documentation in
          let dep_packages =
            List.map doc.packages ~f:(fun (dep : Package_dependency.t) -> dep.name)
          in
          Memo.return { Odoc_config.deps = { packages = dep_packages; libraries = [] } }
      in
      let pkg_libs = List.map local_libs ~f:Lib.Local.to_lib in
      let* pkg_artifacts =
        let+ mlds_list = Packages.mlds sctx pkg in
        (* Convert mld list to (path, in_doc, name) triples, preserving hierarchy *)
        let mlds_triples =
          List.map mlds_list ~f:(fun (mld : Doc_sources.mld) ->
            (* Use full in_doc path to preserve hierarchy, e.g. "deprecated/index.mld" *)
            let in_doc_str = Path.Local.to_string mld.in_doc in
            let name = Filename.remove_extension in_doc_str in
            mld.path, mld.in_doc, name)
        in
        (* Check for duplicates using the hierarchical name *)
        let mlds_map =
          List.fold_left
            mlds_triples
            ~init:String.Map.empty
            ~f:(fun acc (path, in_doc, name) -> String.Map.set acc name (path, in_doc))
        in
        (* Add generated index.mld if not present *)
        let mlds_map =
          String.Map.update mlds_map "index" ~f:(function
            | None ->
              let gen_path = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
              let gen_in_doc = Path.Local.of_string "index.mld" in
              Some (gen_path, gen_in_doc)
            | Some _ as s -> s)
        in
        let lib_modules = Module_name.Set.empty in
        let target = Pkg pkg in
        String.Map.to_list mlds_map
        |> List.map ~f:(fun (mld_name, (mld_path, _in_doc)) ->
          let kind = Page { name = mld_name; pkg_libs } in
          create_artifact_local
            ctx
            ~target
            ~source:mld_path
            ~kind
            ~odoc_config
            ~lib_modules)
      in
      (* Get artifacts for all libraries in this package *)
      let* lib_artifacts_list =
        Memo.List.map local_libs ~f:(fun local_lib ->
          let lib = Lib.Local.to_lib local_lib in
          let lib_name = Lib.name lib in
          discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib)
      in
      (* Flatten and combine all artifacts *)
      let all_artifacts = pkg_artifacts @ List.concat lib_artifacts_list in
      Memo.return (all_artifacts, lib_subdirs)
    else
      (* Installed package - discover mld artifacts + library artifacts *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let installed_libs = Package_discovery.libraries_of_package pkg_discovery pkg in
      (* Get library subdirectory names for build_dir_only_sub_dirs.
         libraries_of_package now includes ALL libraries, including those without archives. *)
      let lib_subdirs =
        List.filter_map installed_libs ~f:(fun lib ->
          match Lib.Local.of_lib lib with
          | Some _ -> None (* Skip local libs *)
          | None ->
            let lib_name = Lib.name lib in
            Some (Lib_name.to_string lib_name))
      in
      (* Get package-level mld artifacts *)
      let* mld_artifacts =
        discover_installed_pkg_mld_artifacts ctx ~pkg ~pkg_libs:installed_libs
      in
      (* Get artifacts for all installed libraries in this package *)
      let* lib_artifacts_list =
        Memo.List.map installed_libs ~f:(fun lib ->
          let lib_name = Lib.name lib in
          discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib)
      in
      (* Flatten and combine all artifacts *)
      let all_artifacts = mld_artifacts @ List.concat lib_artifacts_list in
      Memo.return (all_artifacts, lib_subdirs))
;;

(* Unified handler for _odoc and _odocls package/library directories.
   Both handlers follow the same pattern:
   1. Discover artifacts for the package/library
   2. Group artifacts by library
   3. Process each library (either compile for _odoc or link for _odocls)
   4. Return Build_config with rules *)
(* Helper to compute library directory path for v2 vs v3 *)
let lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name =
  if String.contains pkg_or_lib_name '@'
  then
    (* v2 library: pkg_or_lib_name already identifies the library *)
    Paths.root ctx ++ path_prefix ++ pkg_or_lib_name
  else
    (* v3 library: path is pkg/lib *)
    Paths.root ctx ++ path_prefix ++ pkg_or_lib_name ++ Lib_name.to_string lib_name
;;

(* Helper to create a library-level .odoc-all alias *)
let create_lib_alias ctx ~path_prefix ~pkg_or_lib_name ~lib_name ~file_paths =
  let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
  let alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir in
  let+ () = Rules.Produce.Alias.add_deps alias (Action_builder.paths file_paths) in
  lib_dir
;;

(* Helper to create package-level .odoc-all alias (only for v3 packages) *)
let create_pkg_alias_if_v3 ctx ~path_prefix ~pkg_or_lib_name ~lib_alias_dirs =
  if not (String.contains pkg_or_lib_name '@')
  then (
    let pkg_dir = Paths.root ctx ++ path_prefix ++ pkg_or_lib_name in
    let pkg_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:pkg_dir in
    let lib_alias_deps =
      List.map lib_alias_dirs ~f:(fun lib_dir ->
        Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir
        |> Dune_engine.Dep.alias)
      |> Dune_engine.Dep.Set.of_list
    in
    Rules.Produce.Alias.add_deps pkg_alias (Action_builder.deps lib_alias_deps))
  else Memo.return ()
;;

(* Generate index file for a package from its linked .odocl files *)
let generate_index sctx ~pkg ~odocl_files =
  let ctx = Super_context.context sctx in
  let index_file = Paths.index_file ctx pkg in
  (* compile-index needs all .odocl files:
     - Library .odocl files
     - Package-level mld .odocl files *)
  let odocl_dir = Paths.odocl ctx (Pkg pkg) in
  let open Command.Args in
  (* Pass all .odocl files as dependencies *)
  let odocl_file_args =
    List.map odocl_files ~f:(fun odocl_file -> Dep (Path.build odocl_file))
  in
  let action =
    let open Action_builder.With_targets.O in
    (* Depend on the package's .odoc-all alias *)
    let pkg_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:odocl_dir in
    Action_builder.with_no_targets (Action_builder.dep (Dune_engine.Dep.alias pkg_alias))
    >>> run_odoc
          sctx
          ~dir:(Path.build (Paths.sidebar_root ctx))
          "compile-index"
          ~quiet:false
          ~flags_for:None
          ([ A "-o"; Target index_file ] @ odocl_file_args)
  in
  let* () = add_rule sctx action in
  Memo.return index_file
;;

(* Generate binary sidebar file for a package from its index - called in _sidebar handler *)
let generate_sidebar_binary sctx ~pkg ~index_file =
  let ctx = Super_context.context sctx in
  let sidebar_file = Paths.sidebar_file ctx pkg in
  (* Generate binary sidebar - run from _sidebar directory with relative path to index *)
  let* () =
    let action =
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets (Action_builder.path (Path.build index_file))
      >>> run_odoc
            sctx
            ~dir:(Path.build (Paths.sidebar_root ctx))
            "sidebar-generate"
            ~quiet:false
            ~flags_for:None
            [ A "-o"
            ; Target sidebar_file
            ; A (sprintf "%s/index.odoc-index" (Package.Name.to_string pkg))
            ]
    in
    add_rule sctx action
  in
  Memo.return sidebar_file
;;

(* Generate JSON sidebar file for a package from its index - called in _html handler *)
let generate_sidebar_json sctx ~mode ~pkg ~index_file =
  let ctx = Super_context.context sctx in
  let sidebar_json = Paths.sidebar_json ctx mode pkg in
  (* Generate JSON sidebar - run from mode-specific html directory with relative path to index *)
  let action =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.path (Path.build index_file))
    >>> run_odoc
          sctx
          ~dir:(Path.build (Paths.html_root ctx mode))
          "sidebar-generate"
          ~quiet:false
          ~flags_for:None
          [ A "--json"
          ; A "-o"
          ; Target sidebar_json
          ; A (sprintf "../_sidebar/%s/index.odoc-index" (Package.Name.to_string pkg))
          ]
  in
  add_rule sctx action
;;

(* Handle sidebar generation for a package *)
let handle_sidebar_artifacts sctx pkg_or_lib_name =
  let ctx = Super_context.context sctx in
  Log.info [ Pp.textf "odoc v3: handle_sidebar_artifacts called for %s" pkg_or_lib_name ];
  (* Skip sidebar generation for synthetic packages *)
  if String.contains pkg_or_lib_name '@'
  then (
    Log.info
      [ Pp.textf "odoc v3: Skipping sidebar for synthetic package %s" pkg_or_lib_name ];
    Memo.return (Build_config.Gen_rules.make (Memo.return Rules.empty)))
  else (
    let rules =
      Rules.collect_unit (fun () ->
        let pkg = Package.Name.of_string pkg_or_lib_name in
        (* Discover artifacts to get all .odocl files *)
        let* all_artifacts, _lib_subdirs =
          discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_or_lib_name
        in
        (* Collect all .odocl files from all artifacts (libraries and package pages) *)
        let odocl_files =
          List.filter_map all_artifacts ~f:(fun artifact ->
            if Artifact.hidden artifact then None else Some (Artifact.odocl_file artifact))
        in
        (* Generate index file with all .odocl files *)
        let* index_file = generate_index sctx ~pkg ~odocl_files in
        (* Generate binary sidebar *)
        let* _sidebar_file = generate_sidebar_binary sctx ~pkg ~index_file in
        Memo.return ())
    in
    Memo.return (Build_config.Gen_rules.make rules))
;;

(* Handle remap file generation - single file for all external dependencies *)
let handle_remap_artifacts sctx =
  let ctx = Super_context.context sctx in
  Log.info [ Pp.textf "odoc v3: handle_remap_artifacts called" ];
  let rules =
    Rules.collect_unit (fun () ->
      (* Get all workspace packages *)
      let* workspace_pkgs = get_workspace_packages () in
      (* Collect all libraries from all workspace packages *)
      let* all_workspace_libs =
        Memo.List.concat_map workspace_pkgs ~f:(fun pkg ->
          let pkg_name = Package.Name.to_string pkg in
          (* Skip synthetic packages *)
          if String.contains pkg_name '@'
          then Memo.return []
          else
            let* all_artifacts, _lib_subdirs =
              discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_name
            in
            Memo.return
              (List.filter_map all_artifacts ~f:(fun artifact ->
                 match Artifact.target artifact with
                 | Lib (_, lib) | Private_lib (_, lib) -> Some lib
                 | Pkg _ -> None)))
      in
      (* Create package discovery for package identification and version lookup *)
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      (* Get transitive closure of all dependencies *)
      let* dep_targets =
        if List.is_empty all_workspace_libs
        then Memo.return []
        else
          let* dep_closures =
            Memo.List.map all_workspace_libs ~f:(fun lib ->
              let* closure = Lib.closure [ lib ] ~linking:false in
              Resolve.read_memo closure)
          in
          (* Flatten and deduplicate, excluding workspace libraries *)
          let workspace_lib_set = Lib.Set.of_list all_workspace_libs in
          let all_deps =
            dep_closures
            |> List.concat
            |> Lib.Set.of_list
            |> Lib.Set.to_list
            |> List.filter ~f:(fun lib -> not (Lib.Set.mem workspace_lib_set lib))
          in
          (* Convert to targets using Package_discovery *)
          Memo.List.map all_deps ~f:(fun dep_lib ->
            (* Use Package_discovery to find which package this library belongs to *)
            match Package_discovery.package_of_library pkg_discovery dep_lib with
            | Some pkg -> Memo.return (Lib (pkg, dep_lib))
            | None ->
              (* No package found: this is a private library *)
              let lib_unique_name = lib_unique_id_string dep_lib in
              Memo.return (Private_lib (lib_unique_name, dep_lib)))
      in
      (* Generate remap mappings for external dependencies *)
      let* mappings =
        generate_remap_mappings
          pkg_discovery
          ~local_packages:workspace_pkgs
          ~all_deps:dep_targets
      in
      (* Always create the remap file, even if empty, since it's required as a dependency *)
      let remap_file = Paths.remap_file ctx in
      write_remap_file sctx ~remap_file ~mappings)
  in
  Memo.return (Build_config.Gen_rules.make rules)
;;

(* Helper function to generate HTML for a package in a specific mode *)
let generate_html_for_package
      sctx
      ~ctx
      ~pkg_or_lib_name
      ~library_artifacts
      ~package_pages
      ~artifacts_by_lib_complete
      ~dir
      ~mode
      ()
  =
  (* Combine library artifacts and package pages for HTML generation *)
  let all_artifacts_for_html = library_artifacts @ package_pages in
  (* Filter to only visible artifacts for HTML generation *)
  let visible_artifacts =
    List.filter all_artifacts_for_html ~f:(fun a -> not (Artifact.hidden a))
  in
  Log.info
    [ Pp.textf
        "odoc v3: generate_html_for_package for %s (mode=%s): %d visible artifacts"
        pkg_or_lib_name
        (match mode with
         | Doc_mode.Local_only -> "Local_only"
         | Doc_mode.Full -> "Full")
        (List.length visible_artifacts)
    ];
  (* Generate JSON sidebar and reference binary sidebar for non-synthetic packages *)
  let* sidebar_file_opt =
    if String.contains pkg_or_lib_name '@'
    then
      (* Synthetic package (private lib) - no sidebar *)
      Memo.return None
    else (
      (* Real package - generate JSON sidebar and reference binary sidebar *)
      let pkg = Package.Name.of_string pkg_or_lib_name in
      let index_file = Paths.index_file ctx pkg in
      let* () = generate_sidebar_json sctx ~mode ~pkg ~index_file in
      Memo.return (Some (Paths.sidebar_file ctx pkg)))
  in
  (* Create search_db for the entire package (all visible artifacts) *)
  let* search_db =
    let odocls =
      List.map visible_artifacts ~f:(fun artifact -> Artifact.odocl_file artifact)
    in
    Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
  in
  Log.info
    [ Pp.textf
        "odoc v3: created search_db with %d odocls for %s"
        (List.length visible_artifacts)
        pkg_or_lib_name
    ];
  (* Use shared remap file for Local_only mode (skip for synthetic packages) *)
  let remap_file_opt =
    match mode with
    | Doc_mode.Local_only ->
      if String.contains pkg_or_lib_name '@'
      then None (* Synthetic package - no remap *)
      else Some (Paths.remap_file ctx)
    | Doc_mode.Full -> None
  in
  (* Generate HTML for all visible artifacts *)
  let* () =
    Memo.parallel_iter visible_artifacts ~f:(fun artifact ->
      let call =
        match remap_file_opt with
        | None ->
          generate_html_artifact
            sctx
            ~artifact
            ~search_db
            ~sidebar_file:sidebar_file_opt
            ~mode
            ()
        | Some rf ->
          generate_html_artifact
            sctx
            ~artifact
            ~search_db
            ~sidebar_file:sidebar_file_opt
            ~remap_file:(Some rf)
            ~mode
            ()
      in
      call)
  in
  (* Create format aliases for all output formats *)
  let pkg_name = Package.Name.of_string pkg_or_lib_name in
  let output_subdir = Doc_mode.output_subdir mode in
  let* () =
    Output_format.iter ~f:(fun output ->
      (* Create package-level alias with all HTML files *)
      let all_paths =
        List.map visible_artifacts ~f:(fun artifact ->
          Path.build (Output_format.target ctx output_subdir output artifact))
      in
      let pkg_alias = Dep.format_alias output mode ctx (Pkg pkg_name) in
      Rules.Produce.Alias.add_deps pkg_alias (Action_builder.paths all_paths))
  in
  (* Also create library-level aliases for each library *)
  Lib_name.Map.to_list artifacts_by_lib_complete
  |> Memo.parallel_iter ~f:(fun (lib_name, lib_artifacts) ->
    let visible_lib_artifacts =
      List.filter lib_artifacts ~f:(fun a -> not (Artifact.hidden a))
    in
    if List.is_empty visible_lib_artifacts
    then (
      (* Even for libraries with no artifacts, create empty aliases *)
      (* We need to construct a target for this library *)
      (* Since we have no artifacts, we need to look up the library *)
      let pkg_name = Package.Name.of_string pkg_or_lib_name in
      let* lib_opt =
        let* pkg_discovery = Package_discovery.create ~context:ctx in
        let installed_libs =
          Package_discovery.libraries_of_package pkg_discovery pkg_name
        in
        Memo.return
          (List.find installed_libs ~f:(fun lib -> Lib_name.equal (Lib.name lib) lib_name))
      in
      match lib_opt with
      | Some lib ->
        Output_format.iter ~f:(fun output ->
          let lib_alias = Dep.format_alias output mode ctx (Lib (pkg_name, lib)) in
          Rules.Produce.Alias.add_deps lib_alias (Action_builder.paths []))
      | None -> Memo.return ())
    else
      Output_format.iter ~f:(fun output ->
        let lib_paths =
          List.map visible_lib_artifacts ~f:(fun artifact ->
            Path.build (Output_format.target ctx output_subdir output artifact))
        in
        (* Create library-level alias - need to find the Lib.Local.t for this lib_name *)
        (* For now, use the artifact's target which should be Lib lib *)
        match Artifact.target (List.hd visible_lib_artifacts) with
        | Lib (pkg, lib) ->
          let lib_alias = Dep.format_alias output mode ctx (Lib (pkg, lib)) in
          Rules.Produce.Alias.add_deps lib_alias (Action_builder.paths lib_paths)
        | Private_lib (lib_unique_name, lib) ->
          let lib_alias = Dep.format_alias output mode ctx (Private_lib (lib_unique_name, lib)) in
          Rules.Produce.Alias.add_deps lib_alias (Action_builder.paths lib_paths)
        | Pkg _ -> Memo.return () (* Package artifacts don't have library aliases *)))
;;

let handle_package_artifacts sctx ~dir ~path_prefix pkg_or_lib_name =
  let ctx = Super_context.context sctx in
  Log.info
    [ Pp.textf
        "odoc v3: handle_package_artifacts called for %s with path_prefix=%s"
        pkg_or_lib_name
        path_prefix
    ];
  (* Use unified artifact discovery *)
  let* all_artifacts, lib_subdirs =
    discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_or_lib_name
  in
  Log.info
    [ Pp.textf
        "odoc v3: discovered %d artifacts for %s"
        (List.length all_artifacts)
        pkg_or_lib_name
    ];
  (* Separate package-level pages from library artifacts *)
  let library_artifacts, package_pages =
    List.partition_map all_artifacts ~f:(fun artifact ->
      match Artifact.target artifact with
      | Lib _ | Private_lib _ -> Left artifact (* Library artifacts *)
      | Pkg _ -> Right artifact (* Package-level pages *))
  in
  (* Group library artifacts by library *)
  let artifacts_by_lib = group_artifacts_by_lib library_artifacts in
  Log.info
    [ Pp.textf
        "odoc v3: grouped into %d lib groups for %s"
        (Lib_name.Map.cardinal artifacts_by_lib)
        pkg_or_lib_name
    ];
  (* Ensure all lib_subdirs are represented in artifacts_by_lib, even if they have no artifacts. *)
  let all_lib_names =
    let from_subdirs = List.map lib_subdirs ~f:Lib_name.of_string in
    Lib_name.Set.of_list from_subdirs
  in
  let artifacts_by_lib_complete =
    Lib_name.Set.fold all_lib_names ~init:artifacts_by_lib ~f:(fun lib_name acc ->
      if Lib_name.Map.mem acc lib_name then acc else Lib_name.Map.set acc lib_name [])
  in
  Log.info
    [ Pp.textf
        "odoc v3: complete lib map has %d entries (including empty) for %s"
        (Lib_name.Map.cardinal artifacts_by_lib_complete)
        pkg_or_lib_name
    ];
  (* Determine which operation to perform based on path_prefix *)
  let rules =
    match path_prefix with
    | "_odoc" ->
      (* Compilation *)
      Rules.collect_unit (fun () ->
        let* lib_alias_dirs =
          Lib_name.Map.to_list artifacts_by_lib_complete
          |> Memo.List.map ~f:(fun (lib_name, lib_artifacts) ->
            if List.is_empty lib_artifacts
            then (
              Log.info
                [ Pp.textf
                    "odoc v3: _odoc handler for lib_name=%s: no artifacts, creating \
                     empty .odoc-all alias"
                    (Lib_name.to_string lib_name)
                ];
              (* For libraries with no modules, still create an empty alias *)
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              let alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir in
              let+ () = Rules.Produce.Alias.add_deps alias (Action_builder.paths []) in
              lib_dir)
            else (
              Log.info
                [ Pp.textf
                    "odoc v3: _odoc handler for lib_name=%s: %d artifacts"
                    (Lib_name.to_string lib_name)
                    (List.length lib_artifacts)
                ];
              (* Add all .odoc files to the .odoc-all alias BEFORE compiling,
                 so dependencies on the alias will wait for all files to be built.
                 Group by target type to ensure MLD pages (Pkg target) and modules (Lib target)
                 go to the correct aliases. *)
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              (* Separate artifacts by target type *)
              let artifacts_by_target =
                List.fold_left lib_artifacts ~init:[] ~f:(fun acc artifact ->
                  match
                    List.find acc ~f:(fun (target, _) ->
                      match target, Artifact.target artifact with
                      | Pkg p1, Pkg p2 -> Package.Name.equal p1 p2
                      | Lib (p1, l1), Lib (p2, l2) ->
                        Package.Name.equal p1 p2
                        && Lib_name.equal (Lib.name l1) (Lib.name l2)
                      | _ -> false)
                  with
                  | Some (target, _) ->
                    List.map acc ~f:(fun (t, arts) ->
                      if
                        match t, target with
                        | Pkg p1, Pkg p2 -> Package.Name.equal p1 p2
                        | Lib (p1, l1), Lib (p2, l2) ->
                          Package.Name.equal p1 p2
                          && Lib_name.equal (Lib.name l1) (Lib.name l2)
                        | _ -> false
                      then t, artifact :: arts
                      else t, arts)
                  | None -> (Artifact.target artifact, [ artifact ]) :: acc)
              in
              let* () =
                Memo.parallel_iter artifacts_by_target ~f:(fun (target, arts) ->
                  let odoc_paths =
                    Path.Set.of_list_map arts ~f:(fun a ->
                      Path.build (Artifact.odoc_file a))
                  in
                  Dep.setup_deps ctx target odoc_paths)
              in
              (* Now compile all artifacts *)
              let* () =
                Memo.parallel_iter lib_artifacts ~f:(fun artifact ->
                  compile_artifact sctx ~artifact ~lib_artifacts)
              in
              (* Return the library directory for the package-level alias *)
              Memo.return lib_dir))
        in
        (* Compile package-level pages *)
        let* () =
          Memo.parallel_iter package_pages ~f:(fun artifact ->
            compile_artifact sctx ~artifact ~lib_artifacts:package_pages)
        in
        create_pkg_alias_if_v3 ctx ~path_prefix ~pkg_or_lib_name ~lib_alias_dirs)
    | "_odocls" ->
      (* Linking *)
      Rules.collect_unit (fun () ->
        (* Link library artifacts *)
        let* () =
          Lib_name.Map.to_list artifacts_by_lib_complete
          |> Memo.parallel_iter ~f:(fun (lib_name, lib_artifacts) ->
            if List.is_empty lib_artifacts
            then (
              Log.info
                [ Pp.textf
                    "odoc v3: _odocls handler for lib_name=%s: no artifacts, creating \
                     empty .odoc-all alias"
                    (Lib_name.to_string lib_name)
                ];
              (* Create empty .odoc-all alias for libraries with no modules *)
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              let lib_alias =
                Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir
              in
              Rules.Produce.Alias.add_deps lib_alias (Action_builder.paths []))
            else (
              (* Filter to only visible artifacts for linking *)
              let visible_artifacts =
                List.filter lib_artifacts ~f:(fun a -> not (Artifact.hidden a))
              in
              Log.info
                [ Pp.textf
                    "odoc v3: _odocls handler for lib_name=%s: %d visible artifacts"
                    (Lib_name.to_string lib_name)
                    (List.length visible_artifacts)
                ];
              (* Link all visible artifacts using link_artifact, which determines requires based on artifact type *)
              let* () =
                Memo.parallel_iter visible_artifacts ~f:(fun artifact ->
                  link_artifact sctx ~artifact)
              in
              (* Create library .odoc-all alias with odocl files (only visible artifacts) *)
              let odocl_files =
                List.map visible_artifacts ~f:(fun a ->
                  Path.build (Artifact.odocl_file a))
              in
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              let lib_alias =
                Alias.make (Alias.Name.of_string ".odoc-all") ~dir:lib_dir
              in
              Rules.Produce.Alias.add_deps lib_alias (Action_builder.paths odocl_files)))
        in
        (* Link package-level pages *)
        let* () =
          Memo.parallel_iter package_pages ~f:(fun artifact ->
            link_artifact sctx ~artifact)
        in
        (* Create package-level .odoc-all alias that aggregates all library aliases *)
        if String.contains pkg_or_lib_name '@'
        then Memo.return () (* Synthetic package - no package-level alias *)
        else (
          let pkg_dir = Paths.odocl_root ctx ++ pkg_or_lib_name in
          let pkg_alias = Alias.make (Alias.Name.of_string ".odoc-all") ~dir:pkg_dir in
          (* Collect all odocl files from libraries and pages *)
          let all_odocl_paths =
            let lib_odocls =
              List.concat_map
                (Lib_name.Map.values artifacts_by_lib_complete)
                ~f:(fun lib_artifacts ->
                  List.filter_map lib_artifacts ~f:(fun a ->
                    if Artifact.hidden a
                    then None
                    else Some (Path.build (Artifact.odocl_file a))))
            in
            let page_odocls =
              List.map package_pages ~f:(fun a -> Path.build (Artifact.odocl_file a))
            in
            lib_odocls @ page_odocls
          in
          Rules.Produce.Alias.add_deps pkg_alias (Action_builder.paths all_odocl_paths)))
    | "_html" ->
      (* HTML generation for local packages only *)
      Rules.collect_unit (fun () ->
        generate_html_for_package
          sctx
          ~ctx
          ~pkg_or_lib_name
          ~library_artifacts
          ~package_pages
          ~artifacts_by_lib_complete
          ~dir
          ~mode:Doc_mode.Local_only
          ())
    | "_html_full" ->
      (* HTML generation for all packages (full mode) *)
      Rules.collect_unit (fun () ->
        generate_html_for_package
          sctx
          ~ctx
          ~pkg_or_lib_name
          ~library_artifacts
          ~package_pages
          ~artifacts_by_lib_complete
          ~dir
          ~mode:Doc_mode.Full
          ())
    | _ -> failwith ("Unexpected path_prefix: " ^ path_prefix)
  in
  Memo.return
    (Build_config.Gen_rules.make
       ~build_dir_only_sub_dirs:
         (Build_config.Gen_rules.Build_only_sub_dirs.singleton
            ~dir
            (Subdir_set.of_list lib_subdirs))
       rules)
;;

(* setup_lib_odocl_rules and setup_pkg_odocl_rules were removed - dead code never called.
   The v3 system handles odocl linking through handle_package_artifacts which calls
   link_artifact for each artifact. This is cleaner and more unified. *)

let out_file (output : Output_format.t) odoc =
  match output with
  | Html -> Artifact.html_file odoc
  | Json -> Artifact.json_file odoc
;;

(* Generate a default index for an installed package *)
let default_index_installed ~pkg lib_names =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  Printf.bprintf b "\nThis package provides the following libraries:\n\n";
  lib_names
  |> List.sort ~compare:Lib_name.compare
  |> List.iter ~f:(fun lib_name ->
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib_name);
    Printf.bprintf
      b
      "\nDocumentation for library {!%s}.\n\n"
      (Lib_name.to_string lib_name));
  Buffer.contents b
;;

(* Expand a set of libraries with their odoc-config dependencies transitively.
   For each library, we look at its package's odoc-config.sexp and add:
   - Extra libraries listed in the config
   - All libraries from packages listed in the config
   Returns the expanded set of libraries. *)
let expand_libs_with_odoc_config ctx initial_libs =
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let* lib_db = Lib.DB.installed ctx in
  let rec expand seen_libs worklist =
    match worklist with
    | [] -> Memo.return seen_libs
    | lib :: rest ->
      if Lib.Set.mem seen_libs lib
      then expand seen_libs rest
      else (
        let seen_libs = Lib.Set.add seen_libs lib in
        (* Find the package this library belongs to *)
        let* pkg_opt =
          match Lib.Local.of_lib lib with
          | Some local_lib ->
            Memo.return (Some (Package.Name.of_string (pkg_or_lnu local_lib)))
          | None ->
            (match Package_discovery.package_of_library pkg_discovery lib with
             | Some p -> Memo.return (Some p)
             | None ->
               (match Lib_info.package (Lib.info lib) with
                | Some p -> Memo.return (Some p)
                | None -> Memo.return None))
        in
        match pkg_opt with
        | None -> expand seen_libs rest
        | Some lib_pkg ->
          (* Check if this is a local or installed package *)
          let* packages = Dune_load.packages () in
          (match Package.Name.Map.find packages lib_pkg with
           | Some local_pkg ->
             (* Local package: use Package_info.documentation *)
             let doc = Package.info local_pkg |> Package_info.documentation in
             let doc_pkg_names =
               List.map doc.packages ~f:(fun (dep : Package_dependency.t) -> dep.name)
             in
             (* Get libraries from dependency packages using libs_of_pkg *)
             let* pkg_libs =
               Memo.List.map doc_pkg_names ~f:(fun pkg_name ->
                 libs_of_pkg (Context.name ctx) ~pkg:pkg_name
                 >>| List.map ~f:Lib.Local.to_lib)
               >>| List.concat
             in
             let new_libs = pkg_libs in
             expand seen_libs (new_libs @ rest)
           | None ->
             (* Installed package: use Package_discovery *)
             let odoc_config =
               Package_discovery.config_of_package pkg_discovery lib_pkg
             in
             (* Get additional libraries from odoc-config *)
             let* extra_libs =
               Memo.List.filter_map
                 odoc_config.Odoc_config.deps.libraries
                 ~f:(fun lib_name -> Lib.DB.find lib_db lib_name)
             in
             (* Get libraries from additional packages in odoc-config *)
             let* pkg_libs =
               Memo.List.map odoc_config.Odoc_config.deps.packages ~f:(fun pkg_name ->
                 Memo.return
                   (Package_discovery.libraries_of_package pkg_discovery pkg_name))
               >>| List.concat
             in
             let new_libs = extra_libs @ pkg_libs in
             expand seen_libs (new_libs @ rest)))
  in
  let+ expanded = expand Lib.Set.empty initial_libs in
  Lib.Set.to_list expanded
;;

let setup_package_aliases_format sctx (pkg : Package.t) (output : Output_format.t) (mode : Doc_mode.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source (Context.build_dir ctx) pkg_dir in
    (* Inline Doc_mode.alias logic *)
    match mode with
    | Doc_mode.Local_only -> Output_format.alias output ~dir
    | Doc_mode.Full ->
      (match output with
       | Output_format.Html -> Alias.make (Alias.Name.of_string "doc-full") ~dir
       | Output_format.Json -> Alias.make (Alias.Name.of_string "doc-json-full") ~dir)
  in
  (* Wrap the entire transitive closure computation in Action_builder. *)
  let deps_action =
    let open Action_builder.O in
    let* dep_set =
      Action_builder.of_memo
        (let open Memo.O in
         let* local_libs = Context.name ctx |> libs_of_pkg ~pkg:name in
         let* doc_dep_libs =
           let doc = Package.info pkg |> Package_info.documentation in
           let doc_pkg_names =
             List.map doc.packages ~f:(fun (dep : Package_dependency.t) -> dep.name)
           in
           let* pkg_discovery = Package_discovery.create ~context:ctx in
           Memo.List.map doc_pkg_names ~f:(fun pkg_name ->
             Memo.return (Package_discovery.libraries_of_package pkg_discovery pkg_name))
           >>| List.concat
         in
         let seed_libs = List.map local_libs ~f:Lib.Local.to_lib @ doc_dep_libs in
         let* stdlib_opt = stdlib_lib (Context.name ctx) in
         let* all_dep_libs =
           let+ closures =
             Memo.List.map seed_libs ~f:(fun lib ->
               let* closure =
                 Lib.closure (lib :: Option.to_list stdlib_opt) ~linking:false
               in
               Resolve.read_memo closure)
           in
           let libs_from_closure =
             closures |> List.concat |> Lib.Set.of_list |> Lib.Set.to_list
           in
           match stdlib_opt with
           | Some stdlib -> stdlib :: libs_from_closure
           | None -> libs_from_closure
         in
         let* all_expanded_libs = expand_libs_with_odoc_config ctx all_dep_libs in
         let* pkg_discovery = Package_discovery.create ~context:ctx in
         let* all_targets =
           Memo.List.map all_expanded_libs ~f:(fun lib ->
             let lib_info = Lib.info lib in
             let has_real_package = Option.is_some (Lib_info.package lib_info) in
             if not has_real_package
             then (
               (* Library without a real package - use Private_lib *)
               let lib_unique_name = lib_unique_id_string lib in
               Memo.return (Private_lib (lib_unique_name, lib)))
             else (
               (* Library with a real package - use Lib *)
               let* pkg =
                 match Lib.Local.of_lib lib with
                 | Some local_lib ->
                   Memo.return (Package.Name.of_string (pkg_or_lnu local_lib))
                 | None ->
                   (match Package_discovery.package_of_library pkg_discovery lib with
                    | Some p -> Memo.return p
                    | None ->
                      (match Lib_info.package lib_info with
                       | Some p -> Memo.return p
                       | None ->
                         Memo.return
                           (Package.Name.of_string (Lib_name.to_string (Lib.name lib)))))
               in
               Memo.return (Lib (pkg, lib))))
         in
         let pkg_targets_from_libs =
           List.filter_map all_targets ~f:(fun target ->
             match target with
             | Lib (pkg, _) -> Some (Pkg pkg)
             | Private_lib _ -> None (* Private libs have no package *)
             | Pkg _ -> None)
         in
         let all_targets_with_pkg = (Pkg name :: all_targets) @ pkg_targets_from_libs in
         (* Filter based on mode: Local_only includes only workspace packages *)
         let* filtered_targets =
           match mode with
           | Doc_mode.Local_only ->
             let+ workspace_pkgs = get_workspace_packages () in
             let workspace_pkg_set = Package.Name.Set.of_list workspace_pkgs in
             List.filter all_targets_with_pkg ~f:(fun target ->
               match target with
               | Pkg p -> Package.Name.Set.mem workspace_pkg_set p
               | Lib (p, _) -> Package.Name.Set.mem workspace_pkg_set p
               | Private_lib _ -> true (* Private libs are always local *))
           | Doc_mode.Full ->
             (* Include all dependencies *)
             Memo.return all_targets_with_pkg
         in
         let unique_targets =
           List.sort_uniq filtered_targets ~compare:(fun t1 t2 ->
             match t1, t2 with
             | Pkg p1, Pkg p2 -> Package.Name.compare p1 p2
             | Lib (_, l1), Lib (_, l2) ->
               let name1 = Lib.name l1 in
               let name2 = Lib.name l2 in
               Lib_name.compare name1 name2
             | Private_lib (_, l1), Private_lib (_, l2) ->
               Lib_name.compare (Lib.name l1) (Lib.name l2)
             | Pkg _, (Lib _ | Private_lib _) -> Ordering.Lt
             | (Lib _ | Private_lib _), Pkg _ -> Ordering.Gt
             | Lib _, Private_lib _ -> Ordering.Lt
             | Private_lib _, Lib _ -> Ordering.Gt)
         in
         Memo.return
           (unique_targets
            |> List.map ~f:(Dep.format_alias output mode ctx)
            |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)))
    in
    let* dep_set_with_remap =
      match mode with
      | Doc_mode.Local_only ->
        (* Add remap file as dependency for Local_only mode *)
        let remap_file = Paths.remap_file ctx in
        let+ _ = Action_builder.path (Path.build remap_file) in
        dep_set
      | Doc_mode.Full -> Action_builder.return dep_set
    in
    Action_builder.deps dep_set_with_remap
  in
  Rules.Produce.Alias.add_deps alias deps_action
;;

let setup_package_aliases sctx (pkg : Package.t) =
  (* Set up aliases for both modes *)
  Memo.List.iter Doc_mode.all ~f:(fun mode ->
    Output_format.iter ~f:(fun output ->
      setup_package_aliases_format sctx pkg output mode))
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

(* Stub function for reporting warnings - warnings are now handled by Packages.mlds *)
let report_warnings (_ : Doc_sources.mld list) = ()

(* Wrapper function to convert new Packages.mlds format to old format expected by interface *)
let mlds sctx pkg =
  let* mlds_list = Packages.mlds sctx pkg in
  (* Convert mld list to (path, name) pairs, preserving hierarchical paths *)
  let mlds_pairs =
    List.map mlds_list ~f:(fun (mld : Doc_sources.mld) ->
      (* Use full in_doc path to preserve hierarchy (e.g., "deprecated/index.mld")
         Remove .mld extension to get the page name *)
      let in_doc_str = Path.Local.to_string mld.in_doc in
      let name =
        match String.drop_suffix in_doc_str ~suffix:".mld" with
        | Some n -> n
        | None -> Filename.remove_extension in_doc_str
      in
      mld.path, name)
  in
  Memo.return (mlds_pairs, mlds_list)
;;

let package_mlds =
  let memo =
    Memo.create
      "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package_name)
      (fun (sctx, pkg) ->
         Rules.collect (fun () ->
           let* mlds_pairs, _mlds_list = mlds sctx pkg in
           let mlds = check_mlds_no_dupes ~pkg ~mlds:mlds_pairs in
           let ctx = Super_context.context sctx in
           if String.Map.mem mlds "index.mld"
           then Memo.return mlds
           else (
             let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
             let* entry_modules = entry_modules sctx ~pkg in
             let+ () =
               add_rule
                 sctx
                 (Action_builder.write_file gen_mld (default_index ~pkg entry_modules))
             in
             String.Map.set mlds "index.mld" (gen_mld, "index.mld"))))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)
;;

(* setup_package_odoc_rules removed - unused old function *)

let gen_project_rules sctx project =
  let* mask =
    let+ mask = Dune_load.mask () in
    Option.map ~f:Package.Name.Map.keys mask
  in
  Dune_project.packages project
  |> Dune_lang.Package_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (_, (pkg : Package.t)) ->
    (* Check if this package is in the mask (honors -p flag) *)
    let should_build =
      match mask with
      | None -> true
      | Some mask_pkgs -> List.mem ~equal:Package.Name.equal mask_pkgs (Package.name pkg)
    in
    if should_build
    then
      (* setup @doc to build the correct html for the package *)
      setup_package_aliases sctx pkg
    else Memo.return ())
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
    let local_lib = Lib.Local.of_lib_exn lib in
    (* Wrap the transitive closure computation in Action_builder for lazy evaluation *)
    let deps_action =
      let open Action_builder.O in
      let* dep_set =
        Action_builder.of_memo
          (let open Memo.O in
           (* Collect the transitive closure of all dependencies including stdlib *)
           let* stdlib_opt = stdlib_lib (Context.name ctx) in
           let* all_dep_libs =
             let+ closures =
               Memo.List.map [ local_lib ] ~f:(fun lib ->
                 let* closure =
                   Lib.closure
                     (Lib.Local.to_lib lib :: Option.to_list stdlib_opt)
                     ~linking:false
                 in
                 Resolve.read_memo closure)
             in
             let libs_from_closure =
               closures |> List.concat |> Lib.Set.of_list |> Lib.Set.to_list
             in
             (* Explicitly add stdlib if it exists, since Lib.closure may not include it *)
             match stdlib_opt with
             | Some stdlib -> stdlib :: libs_from_closure
             | None -> libs_from_closure
           in
           (* Convert to targets, using Private_lib for libraries without real packages *)
           let* pkg_discovery = Package_discovery.create ~context:ctx in
           let* all_targets =
             Memo.List.map all_dep_libs ~f:(fun lib ->
               let lib_info = Lib.info lib in
               let has_real_package = Option.is_some (Lib_info.package lib_info) in
               if not has_real_package
               then (
                 (* Library without a real package - use Private_lib *)
                 let lib_unique_name = lib_unique_id_string lib in
                 Memo.return (Private_lib (lib_unique_name, lib)))
               else (
                 (* Library with a real package - use Lib *)
                 let* pkg =
                   match Lib.Local.of_lib lib with
                   | Some local_lib ->
                     (* Local library - use pkg_or_lnu which handles both v3 and v2 *)
                     Memo.return (Package.Name.of_string (pkg_or_lnu local_lib))
                   | None ->
                     (* Installed library - use Package_discovery to get correct package *)
                     (match Package_discovery.package_of_library pkg_discovery lib with
                      | Some p -> Memo.return p
                      | None ->
                        (* Fallback if Package_discovery doesn't know about it *)
                        (match Lib_info.package lib_info with
                         | Some p -> Memo.return p
                         | None ->
                           Memo.return
                             (Package.Name.of_string (Lib_name.to_string (Lib.name lib)))))
                 in
                 Memo.return (Lib (pkg, lib))))
           in
           (* Filter to only local libraries (exclude installed/external dependencies) *)
           let filtered_targets =
             List.filter all_targets ~f:(fun target ->
               match target with
               | Pkg _ -> false (* We don't add Pkg targets for private libs *)
               | Lib (_, lib) | Private_lib (_, lib) ->
                 (* Keep only local libraries, filter out installed ones *)
                 Option.is_some (Lib.Local.of_lib lib))
           in
           (* Deduplicate targets *)
           let unique_targets =
             List.sort_uniq filtered_targets ~compare:(fun t1 t2 ->
               match t1, t2 with
               | Pkg p1, Pkg p2 -> Package.Name.compare p1 p2
               | Lib (_, l1), Lib (_, l2) ->
                 (* Compare libraries by their names *)
                 let name1 = Lib.name l1 in
                 let name2 = Lib.name l2 in
                 Lib_name.compare name1 name2
               | Private_lib (_, l1), Private_lib (_, l2) ->
                 Lib_name.compare (Lib.name l1) (Lib.name l2)
               | Pkg _, (Lib _ | Private_lib _) -> Ordering.Lt
               | (Lib _ | Private_lib _), Pkg _ -> Ordering.Gt
               | Lib _, Private_lib _ -> Ordering.Lt
               | Private_lib _, Lib _ -> Ordering.Gt)
           in
           (* Return the dep set *)
           Memo.return
             (unique_targets
              |> List.map ~f:(Dep.format_alias Html Doc_mode.Local_only ctx)
              |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)))
      in
      Action_builder.deps dep_set
    in
    Rules.Produce.Alias.add_deps (Alias.make ~dir Alias0.private_doc) deps_action
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
    let* local_lib_opt =
      Memo.List.find_map pkg_libs ~f:(fun lib ->
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
       if Option.is_some lib_pkg && Package.Name.equal (Option.value_exn lib_pkg) pkg
       then (
         Log.info
           [ Pp.textf
               "odoc v3: Found local library %s in package %s"
               (Lib_name.to_string lib_name)
               (Package.Name.to_string pkg)
           ];
         Memo.return (Some lib))
       else (
         Log.info
           [ Pp.textf
               "odoc v3: Local library %s not in package %s (has package %s)"
               (Lib_name.to_string lib_name)
               (Package.Name.to_string pkg)
               (match lib_pkg with
                | Some p -> Package.Name.to_string p
                | None -> "none")
           ];
         Memo.return None)
     | None ->
       (* Installed library - use Package_discovery *)
       let* pkg_discovery = Package_discovery.create ~context:ctx in
       let discovered_pkg = Package_discovery.package_of_library pkg_discovery lib in
       (match discovered_pkg with
        | Some discovered_pkg when Package.Name.equal discovered_pkg pkg ->
          Log.info
            [ Pp.textf
                "odoc v3: Found installed library %s in package %s"
                (Lib_name.to_string lib_name)
                (Package.Name.to_string pkg)
            ];
          Memo.return (Some lib)
        | Some other_pkg ->
          Log.info
            [ Pp.textf
                "odoc v3: Installed library %s is in package %s, not %s"
                (Lib_name.to_string lib_name)
                (Package.Name.to_string other_pkg)
                (Package.Name.to_string pkg)
            ];
          Memo.return None
        | None ->
          Log.info
            [ Pp.textf
                "odoc v3: Installed library %s not in package %s (not found in \
                 Package_discovery)"
                (Lib_name.to_string lib_name)
                (Package.Name.to_string pkg)
            ];
          Memo.return None))
;;

let handle_classify_dir sctx ~pkg_name ~lib_name =
  (* classify library directory: _doc/classify/{package}/{library} *)
  Log.info
    [ Pp.textf "odoc v3: Handling classify dir for pkg=%s lib=%s" pkg_name lib_name ];
  let pkg = Package.Name.of_string pkg_name in
  let lib_name = Lib_name.of_string lib_name in
  let ctx = Super_context.context sctx in
  Log.info [ Pp.textf "odoc v3: (classify handler) calling find_lib_for_package" ];
  let* lib_opt = find_lib_for_package sctx ~pkg ~lib_name in
  match lib_opt with
  | None ->
    Log.info
      [ Pp.textf
          "odoc v3: Library %s not found for classify"
          (Lib_name.to_string lib_name)
      ];
    Memo.return ()
  | Some lib ->
    (* Only generate classify for installed libraries, not local ones *)
    (match Lib.Local.of_lib lib with
     | Some _local_lib ->
       Log.info
         [ Pp.textf
             "odoc v3: Library %s is local, skipping classify"
             (Lib_name.to_string lib_name)
         ];
       Memo.return () (* Local library - no classify needed *)
     | None ->
       (* Library is installed - generate odoc classify *)
       Log.info
         [ Pp.textf
             "odoc v3: Library %s is installed, generating classify"
             (Lib_name.to_string lib_name)
         ];
       let info = Lib.info lib in
       let src_dir = Lib_info.src_dir info in
       let classify_output =
         Paths.root ctx
         ++ "classify"
         ++ pkg_name
         ++ Lib_name.to_string lib_name
         ++ "odoc.classify"
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
       add_rule sctx run_classify)
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
        | None -> true)
    in
    if List.is_empty truly_installed_libs
    then Memo.return ()
    else (
      (* Generate index.mld for installed package *)
      let lib_names = List.map truly_installed_libs ~f:Lib.name in
      let index_content = default_index_installed ~pkg lib_names in
      let index_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
      add_rule sctx (Action_builder.write_file index_mld index_content))
;;

(* NOTE: Old HTML generation functions (handle_html_dir, setup_pkg_html_rules,
   setup_lib_html_rules, setup_installed_pkg_html_rules, setup_generate,
   setup_generate_all, search_db_for_lib, out_files) were deleted.

   HTML generation now goes through the unified handle_package_artifacts function
   with path_prefix="_html", which calls generate_html_artifact for each visible artifact.

   This provides a single unified code path for compilation (_odoc), linking (_odocls),
   and HTML generation (_html) for both local and installed packages/libraries. *)

let gen_rules sctx ~dir rest =
  let rest_str = String.concat ~sep:"/" rest in
  let rest_len = List.length rest in
  Log.info
    [ Pp.textf
        "odoc v3: gen_rules ENTRY for dir %s with rest '%s' (length=%d)"
        (Path.Build.to_string dir)
        rest_str
        rest_len
    ];
  (match rest with
   | [ "_html"; a; b ] ->
     Log.info
       [ Pp.textf "odoc v3: PRE-MATCH: 3-element _html pattern [_html; %s; %s]" a b ]
   | _ -> ());
  let result =
    match rest with
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
        Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
      in
      let directory_targets =
        Path.Build.Map.singleton (Paths.odoc_support ctx Doc_mode.Local_only) Loc.none
      in
      let rules =
        Rules.collect_unit (fun () ->
          Sherlodoc.sherlodoc_dot_js sctx ~dir:(Paths.html_root ctx Doc_mode.Local_only)
          >>> setup_css_rule sctx ~mode:Doc_mode.Local_only
          >>> setup_toplevel_index_rules sctx Doc_mode.Local_only)
      in
      Memo.return
        (Build_config.Gen_rules.make
           ~directory_targets
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list pkg_subdirs))
           rules)
    | [ "_html"; pkg_name; lib_name ] ->
      (* Library directory: _doc/_html/{package}/{library} *)
      (* Redirect to parent - the package level handler will generate HTML for all libraries *)
      Log.info
        [ Pp.textf
            "odoc v3: Library directory handler for pkg=%s lib=%s - redirecting to parent"
            pkg_name
            lib_name
        ];
      Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
    | [ "_html"; pkg_name; lib_name; module_name ] ->
      Log.info
        [ Pp.textf
            "odoc v3: Module directory handler for pkg=%s lib=%s module=%s - redirecting \
             to parent"
            pkg_name
            lib_name
            module_name
        ];
      Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
    | [ "_mlds"; pkg_name ] -> has_rules (fun () -> handle_mlds_dir sctx ~pkg_name)
    | [ "_odoc" ] ->
      (* Root odoc directory - allow subdirs *)
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
           (Memo.return Rules.empty))
    | [ "_odoc"; pkg_or_lib_name ] ->
      (* Compilation: use unified handler *)
      handle_package_artifacts sctx ~dir ~path_prefix:"_odoc" pkg_or_lib_name
    | [ "_odoc"; pkg_name; lib_name ] ->
      (* Library directory: _doc/_odoc/{package}/{library} *)
      (* Redirect to parent - the package level handler will generate rules for all libraries *)
      Log.info
        [ Pp.textf
            "odoc v3: Library directory handler for pkg=%s lib=%s - redirecting to parent"
            pkg_name
            lib_name
        ];
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
      Log.info
        [ Pp.textf
            "odoc v3: Library directory handler for pkg=%s lib=%s - redirecting to parent"
            pkg_name
            lib_name
        ];
      Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
    | [ "_html"; pkg_or_lib_name ] ->
      (* HTML generation: use unified handler *)
      handle_package_artifacts sctx ~dir ~path_prefix:"_html" pkg_or_lib_name
    | [ "_html_full" ] ->
      (* Root HTML_full directory - allow all package subdirectories *)
      let ctx = Super_context.context sctx in
      let* packages = Dune_load.packages () in
      let pkg_subdirs =
        Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
      in
      let directory_targets =
        Path.Build.Map.singleton (Paths.odoc_support ctx Doc_mode.Full) Loc.none
      in
      let rules =
        Rules.collect_unit (fun () ->
          (* Set up CSS/support files and sherlodoc for _html_full *)
          let html_root = Paths.html_root ctx Doc_mode.Full in
          Sherlodoc.sherlodoc_dot_js sctx ~dir:html_root
          >>> setup_css_rule sctx ~mode:Doc_mode.Full
          >>> Memo.return ())
      in
      Memo.return
        (Build_config.Gen_rules.make
           ~directory_targets
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list pkg_subdirs))
           rules)
    | [ "_html_full"; pkg_or_lib_name ] ->
      (* HTML generation (full mode): use unified handler *)
      handle_package_artifacts sctx ~dir ~path_prefix:"_html_full" pkg_or_lib_name
    | [ "_html_full"; pkg_name; lib_name ] ->
      (* Library directory: _doc/_html_full/{package}/{library} *)
      (* Redirect to parent - the package level handler will generate HTML for all libraries *)
      Log.info
        [ Pp.textf
            "odoc v3: Library directory handler (full mode) for pkg=%s lib=%s - \
             redirecting to parent"
            pkg_name
            lib_name
        ];
      Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
    | [ "_html_full"; pkg_name; lib_name; module_name ] ->
      Log.info
        [ Pp.textf
            "odoc v3: Module directory handler (full mode) for pkg=%s lib=%s module=%s - \
             redirecting to parent"
            pkg_name
            lib_name
            module_name
        ];
      Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
    | [ "_sidebar" ] ->
      (* Root sidebar directory - allow subdirs *)
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
           (Memo.return Rules.empty))
    | [ "_sidebar"; pkg_or_lib_name ] ->
      (* Sidebar generation for a package *)
      handle_sidebar_artifacts sctx pkg_or_lib_name
    | [ "_remap" ] ->
      (* Remap directory - generate single remap file for all external dependencies *)
      handle_remap_artifacts sctx
    | [ "classify"; pkg_name; lib_name ] ->
      has_rules (fun () -> handle_classify_dir sctx ~pkg_name ~lib_name)
    | other ->
      Log.info
        [ Pp.textf
            "odoc v3: No handler matched for rest=%s (pattern=%s)"
            (String.concat ~sep:"/" rest)
            (match other with
             | [] -> "[]"
             | [ a ] -> sprintf "[%s]" a
             | [ a; b ] -> sprintf "[%s; %s]" a b
             | [ a; b; c ] -> sprintf "[%s; %s; %s]" a b c
             | [ a; b; c; d ] -> sprintf "[%s; %s; %s; %s]" a b c d
             | _ -> sprintf "[%d elements]" (List.length other))
        ];
      (* For unmatched paths, return empty rules with no subdirectories allowed.
       Subdirectories should be explicitly listed in parent handlers. *)
      Memo.return (Build_config.Gen_rules.make (Memo.return Rules.empty))
  in
  Log.info
    [ Pp.textf
        "odoc v3: gen_rules EXIT for dir %s with result from pattern: %s"
        (Path.Build.to_string dir)
        (match rest with
         | [] -> "empty"
         | [ a ] -> sprintf "[%s]" a
         | [ a; b ] -> sprintf "[%s; %s]" a b
         | [ a; b; c ] -> sprintf "[%s; %s; %s]" a b c
         | _ -> sprintf "[%d elements]" (List.length rest))
    ];
  result
;;
