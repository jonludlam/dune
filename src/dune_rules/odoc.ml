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

(* Artifact data types *)
type page = { name : string; pkg_libs : Lib.t list }

type mod_ =
  { visible : bool
  ; module_name : Module_name.t
  }

(* GADT target - enforces that Module artifacts can only have Lib/Private_lib targets,
   and Page artifacts can only have Pkg/Toplevel targets *)
type _ target =
  | Lib : Package.Name.t * Lib.t -> mod_ target
    (* Library with a real package - package overrides Lib_info.package for installed libs *)
  | Private_lib : string * Lib.t -> mod_ target
    (* Library without a real package - uses lib_unique_name as identifier *)
  | Pkg : Package.Name.t -> page target
  | Toplevel : page target (* The toplevel index above all packages *)

(* Existential wrapper for targets - used when we need heterogeneous lists *)
type any_target = Any_target : 'a target -> any_target

let compare_any_target (Any_target t1) (Any_target t2) =
  (* Assign a rank to each target type for ordering *)
  let rank : type a. a target -> int = function
    | Pkg _ -> 0
    | Lib _ -> 1
    | Private_lib _ -> 2
    | Toplevel -> 3
  in
  let r1 = rank t1 in
  let r2 = rank t2 in
  (* First compare by rank (target type) *)
  match Int.compare r1 r2 with
  | Eq ->
    (* Same rank, so same target type - compare by contents *)
    (match t1, t2 with
     | Pkg p1, Pkg p2 -> Package.Name.compare p1 p2
     | Lib (_, l1), Lib (_, l2) -> Lib_name.compare (Lib.name l1) (Lib.name l2)
     | Private_lib (_, l1), Private_lib (_, l2) -> Lib_name.compare (Lib.name l1) (Lib.name l2)
     | Toplevel, Toplevel -> Ordering.Eq
     | _ -> assert false (* Ranks are equal, so types must match *))
  | ordering -> ordering
;;

(* Artifact kind - contains data and target together in an existential *)
type artifact_kind =
  | Module : mod_ * mod_ target -> artifact_kind
  | Page : page * page target -> artifact_kind

type artifact_source =
  | Local_source of Path.Build.t (* cmti, cmt, mld from local build *)
  | Installed_source of { src_path : Path.t (* From Lib_info.src_dir *) }

(* Check if a library is vendored using dune's vendored_dirs mechanism *)
let is_lib_vendored lib =
  let lib_info = Lib.info lib in
  match Lib_info.status lib_info with
  | Installed_private | Installed -> Memo.return false
  | Public _ | Private _ ->
    let src_path = Path.drop_optional_build_context (Lib_info.src_dir lib_info) in
    (match Path.as_in_source_tree src_path with
     | Some src_dir -> Source_tree.is_vendored src_dir
     | None -> Memo.return false)
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

type sidebar_scope =
  | Per_package of Package.Name.t
  | Global

module Paths = struct
  let odoc_support_dirname = "odoc.support"
  let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"

  let odocs : type a. Context.t -> a target -> Path.Build.t =
    fun ctx -> function
      | Lib (pkg, lib) ->
        let lib_name = Lib.name lib in
        root ctx ++ "_odoc" ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
      | Private_lib (lib_unique_name, _) ->
        root ctx ++ "_odoc" ++ lib_unique_name
      | Pkg pkg -> root ctx ++ "_odoc" ++ Package.Name.to_string pkg
      | Toplevel -> root ctx ++ "_odoc"
  ;;

  let html_root ctx mode = root ctx ++ Doc_mode.output_subdir mode
  let odocl_root ctx = root ctx ++ "_odocls"

  let html : type a. Context.t -> Doc_mode.t -> a target -> Path.Build.t =
    fun ctx mode target ->
      match target with
      | Lib (pkg, lib) ->
        let lib_name = Lib.name lib in
        html_root ctx mode ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
      | Private_lib (lib_unique_name, _) ->
        html_root ctx mode ++ lib_unique_name
      | Pkg pkg -> html_root ctx mode ++ Package.Name.to_string pkg
      | Toplevel -> html_root ctx mode
  ;;

  let odocl : type a. Context.t -> a target -> Path.Build.t =
    fun ctx -> function
      | Lib (pkg, lib) ->
        let lib_name = Lib.name lib in
        odocl_root ctx ++ Package.Name.to_string pkg ++ Lib_name.to_string lib_name
      | Private_lib (lib_unique_name, _) ->
        odocl_root ctx ++ lib_unique_name
      | Pkg pkg -> odocl_root ctx ++ Package.Name.to_string pkg
      | Toplevel -> odocl_root ctx
  ;;

  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg

  let lib_mld_dir ctx pkg lib_name =
    gen_mld_dir ctx pkg ++ Lib_name.to_string lib_name
  ;;

  let lib_index_mld ctx pkg lib_name = lib_mld_dir ctx pkg lib_name ++ "index.mld"
  let odoc_support ctx mode = html_root ctx mode ++ odoc_support_dirname
  let toplevel_index_mld ctx = root ctx ++ "_mlds" ++ "index.mld"

  let sidebar_root ctx = root ctx ++ "_sidebar"

  let index_file ctx scope =
    match scope with
    | Global -> sidebar_root ctx ++ "index.odoc-index"
    | Per_package pkg -> sidebar_root ctx ++ Package.Name.to_string pkg ++ "index.odoc-index"
  ;;

  let sidebar_file ctx scope =
    match scope with
    | Global -> sidebar_root ctx ++ "sidebar.odoc-sidebar"
    | Per_package pkg ->
      sidebar_root ctx ++ Package.Name.to_string pkg ++ "sidebar.odoc-sidebar"
  ;;

  let sidebar_json ctx mode scope =
    match scope with
    | Global -> html_root ctx mode ++ "sidebar.json"
    | Per_package pkg -> html_root ctx mode ++ Package.Name.to_string pkg ++ "sidebar.json"

  let remap_file ctx = root ctx ++ "_remap" ++ "remap.txt"
end

module Artifact : sig
  type t

  val kind : t -> artifact_kind
  val source_file : t -> Path.t
  val odoc_file : Context.t -> t -> Path.Build.t
  val odocl_file : Context.t -> t -> Path.Build.t
  val odoc_dir : Context.t -> t -> Path.Build.t
  val html_file : Context.t -> Doc_mode.t -> t -> Path.Build.t
  val json_file : Context.t -> Doc_mode.t -> t -> Path.Build.t
  val pkg : t -> Package.Name.t option
  val lib_name : t -> Lib_name.t
  val lib : t -> Lib.t option
  val odoc_config : t -> Odoc_config.t
  val hidden : t -> bool
  val parent_id : t -> string
  val should_suppress_output : t -> bool Memo.t

  val create
    :  kind:artifact_kind
    -> source:artifact_source
    -> odoc_config:Odoc_config.t
    -> t
end = struct
  type t =
    { kind : artifact_kind
    ; source : artifact_source
    ; odoc_config : Odoc_config.t
    }

  let kind t = t.kind
  let odoc_config t = t.odoc_config

  let source_file t =
    match t.source with
    | Local_source path -> Path.build path
    | Installed_source { src_path; _ } -> src_path
  ;;

  let pkg t =
    match t.kind with
    | Module (_, Lib (pkg, _)) -> Some pkg
    | Module (_, Private_lib _) -> None
    | Page (_, Pkg pkg) -> Some pkg
    | Page (_, Toplevel) -> None
  ;;

  let lib t =
    match t.kind with
    | Module (_, (Lib (_, lib) | Private_lib (_, lib))) -> Some lib
    | Page _ -> None
  ;;

  let lib_name t =
    match t.kind with
    | Module (_, (Lib (_, lib) | Private_lib (_, lib))) -> Lib.name lib
    | Page (_, Pkg pkg) -> Lib_name.of_string (Package.Name.to_string pkg)
    | Page (_, Toplevel) -> Lib_name.of_string "index"
  ;;

  (* Get the odoc directory for an artifact *)
  let odoc_dir ctx t =
    match t.kind with
    | Module (_, target) -> Paths.odocs ctx target
    | Page (_, target) -> Paths.odocs ctx target
  ;;

  (* Split hierarchical page name like "foo/baz" into (Some "foo", "baz").
     For non-hierarchical pages like "index", returns (None, "index"). *)
  let split_page_name name =
    match String.rsplit2 name ~on:'/' with
    | Some (parent, leaf) -> Some parent, leaf
    | None -> None, name
  ;;

  (* Extract the basename from kind (for pages) or source (for modules).
     For hierarchical pages like "foo/baz", returns just the leaf "baz"
     since the parent path "foo" is already in parent_id. *)
  let get_basename t =
    match t.kind, t.source with
    | Page (page, _), _ -> snd (split_page_name page.name)
    | Module (_, _), Local_source src_path ->
      Path.Build.basename src_path |> Filename.remove_extension
    | Module (mod_, _), Installed_source _ ->
      Module_name.to_string mod_.module_name |> String.uncapitalize_ascii
  ;;

  let odoc_file ctx t =
    let basename = get_basename t in
    match t.kind with
    | Page (page, target) ->
      let base_dir = Paths.odocs ctx target in
      (* For hierarchical pages like "deprecated/index", include parent path in directory *)
      (match fst (split_page_name page.name) with
       | Some parent_path -> base_dir ++ parent_path ++ ("page-" ^ basename ^ ".odoc")
       | None -> base_dir ++ ("page-" ^ basename ^ ".odoc"))
    | Module (_, target) ->
      let base_dir = Paths.odocs ctx target in
      base_dir ++ (basename ^ ".odoc")
  ;;

  let odocl_file ctx t =
    let basename = get_basename t in
    match t.kind with
    | Page (page, target) ->
      let base_dir = Paths.odocl ctx target in
      (* For hierarchical pages like "deprecated/index", include parent path in directory *)
      (match fst (split_page_name page.name) with
       | Some parent_path -> base_dir ++ parent_path ++ ("page-" ^ basename ^ ".odocl")
       | None -> base_dir ++ ("page-" ^ basename ^ ".odocl"))
    | Module (_, target) ->
      let base_dir = Paths.odocl ctx target in
      base_dir ++ (basename ^ ".odocl")
  ;;

  let html_output_file ctx mode t ~suffix =
    let basename = get_basename t in
    match t.kind with
    | Module (_, target) ->
      let html_base = Paths.html ctx mode target in
      let html_dir = html_base ++ Stdune.String.capitalize basename in
      html_dir ++ ("index" ^ suffix)
    | Page (page, target) ->
      let html_base = Paths.html ctx mode target in
      (* For hierarchical pages like "deprecated/index", include parent path in directory *)
      let html_path =
        match fst (split_page_name page.name) with
        | Some parent_path -> html_base ++ parent_path ++ basename
        | None -> html_base ++ basename
      in
      Path.Build.extend_basename html_path ~suffix
  ;;

  let html_file ctx mode t = html_output_file ctx mode t ~suffix:".html"
  let json_file ctx mode t = html_output_file ctx mode t ~suffix:".html.json"

  let hidden t =
    match t.kind with
    | Page _ -> false
    | Module _ ->
      let basename = get_basename t in
      String.contains_double_underscore basename
  ;;

  let parent_id t =
    let base_id =
      match t.kind with
      | Module (_, Lib (pkg, lib)) ->
        sprintf "%s/%s" (Package.Name.to_string pkg) (Lib_name.to_string (Lib.name lib))
      | Module (_, Private_lib (lib_unique_name, _)) -> lib_unique_name
      | Page (_, Pkg pkg) -> Package.Name.to_string pkg
      | Page (_, Toplevel) -> ""
    in
    match t.kind with
    | Module _ -> base_id
    | Page (page, _) ->
      (* For hierarchical pages like "deprecated/index", include parent path without "page-" prefix *)
      (match fst (split_page_name page.name) with
       | Some parent_path -> sprintf "%s/%s" base_id parent_path
       | None -> base_id)
  ;;

  let should_suppress_output t =
    match t.source with
    | Installed_source _ -> Memo.return true
    | Local_source _ ->
      (* Check if this is a vendored library *)
      (match t.kind with
       | Module (_, (Lib (_, lib) | Private_lib (_, lib))) -> is_lib_vendored lib
       | Page _ -> Memo.return false)
  ;;

  let create ~kind ~source ~odoc_config = { kind; source; odoc_config }
end

let add_rule sctx =
  let dir = Super_context.context sctx |> Context.build_dir in
  Super_context.add_rule sctx ~dir
;;

module Output_format = struct
  type t =
    | Html
    | Json

  let all = [ Html; Json ]

  let args = function
    | Html -> Command.Args.empty
    | Json -> A "--as-json"
  ;;

  let target ctx mode t odoc_file =
    match t with
    | Html -> Artifact.html_file ctx mode odoc_file
    | Json -> Artifact.json_file ctx mode odoc_file
  ;;

  let alias t ~dir =
    match t with
    | Html -> Alias.make Alias0.doc ~dir
    | Json -> Alias.make Alias0.doc_json ~dir
  ;;
end

module Dep : sig
  (** Create a .odoc-all alias at any directory *)
  val odoc_all_alias : dir:Path.Build.t -> Alias.t

  (** Create a format alias (.doc or .doc-json) for a target *)
  val format_alias : Output_format.t -> Doc_mode.t -> Context.t -> 'a target -> Alias.t

  (** Add file dependencies to an alias *)
  val add_file_deps : Alias.t -> Path.t list -> unit Memo.t

  (** Make an alias depend on .odoc-all aliases in the given directories *)
  val add_odoc_all_deps : Alias.t -> dirs:Path.Build.t list -> unit Memo.t

  (** High-level: Get dependencies for libraries during compilation/linking *)
  val deps
    :  Context.t
    -> Package.Name.t list
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (** High-level: Set up .odoc-all dependencies for a target *)
  val setup_deps : Context.t -> 'a target -> Path.Set.t -> unit Memo.t
end = struct
  let odoc_all_alias ~dir = Alias.make (Alias.Name.of_string ".odoc-all") ~dir

  let odoc_all_alias_for_target : type a. Context.t -> a target -> Alias.t =
    fun ctx target -> odoc_all_alias ~dir:(Paths.odocs ctx target)
  ;;

  let format_alias : type a. Output_format.t -> Doc_mode.t -> Context.t -> a target -> Alias.t
    =
    fun f mode ctx m -> Output_format.alias f ~dir:(Paths.html ctx mode m)
  ;;

  let add_file_deps alias files =
    Rules.Produce.Alias.add_deps alias (Action_builder.paths files)
  ;;

  let add_odoc_all_deps alias ~dirs =
    let dep_set =
      List.map dirs ~f:(fun dir ->
        Dune_engine.Dep.alias (odoc_all_alias ~dir))
      |> Dune_engine.Dep.Set.of_list
    in
    Rules.Produce.Alias.add_deps alias (Action_builder.deps dep_set)
  ;;

  let deps ctx pkgs requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    let* pkg_discovery = Action_builder.of_memo (Package_discovery.create ~context:ctx) in
    Action_builder.deps
      (let init =
         List.fold_left pkgs ~init:Dep.Set.empty ~f:(fun acc p ->
           Dep.Set.add acc
             (Dep.alias (odoc_all_alias ~dir:(Paths.odocs ctx (Pkg p)))))
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match Lib.Local.of_lib lib with
         | None ->
           let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
           (match lib_pkg_opt with
            | Some lib_pkg ->
              let dir =
                Paths.root ctx
                ++ "_odoc"
                ++ Package.Name.to_string lib_pkg
                ++ Lib_name.to_string (Lib.name lib)
              in
              Dep.Set.add acc (Dep.alias (odoc_all_alias ~dir))
            | None -> acc)
         | Some local_lib ->
           let lib_t = Lib.Local.to_lib local_lib in
           let info = Lib.info lib_t in
           let target =
             match Lib_info.package info with
             | Some pkg -> Lib (pkg, lib_t)
             | None ->
               let lib_unique_name = lib_unique_name local_lib in
               Private_lib (lib_unique_name, lib_t)
           in
           let dir = Paths.odocs ctx target in
           Dep.Set.add acc (Dep.alias (odoc_all_alias ~dir))))
  ;;

  let setup_deps : type a. Context.t -> a target -> Path.Set.t -> unit Memo.t =
    fun ctx m files ->
      let target_name =
        match m with
        | Lib (pkg, lib) ->
          "lib:" ^ Package.Name.to_string pkg ^ "/" ^ Lib_name.to_string (Lib.name lib)
        | Private_lib (lib_unique_name, lib) ->
          "private_lib:" ^ lib_unique_name ^ "/" ^ Lib_name.to_string (Lib.name lib)
        | Pkg pkg -> "pkg:" ^ Package.Name.to_string pkg
        | Toplevel -> "toplevel"
      in
      Log.info
        [ Pp.textf
            "Dep.setup_deps: Adding %d files to .odoc-all for %s"
            (Path.Set.cardinal files)
            target_name
        ];
      add_file_deps (odoc_all_alias_for_target ctx m) (Path.Set.to_list files)
  ;;
end

(* Remap generation helpers *)

(* Get list of local workspace packages *)
let get_workspace_packages () =
  let* packages = Dune_load.packages () in
  let* projects = Dune_load.projects () in
  let* vendored_packages =
    Memo.List.fold_left
      projects
      ~init:Package.Name.Set.empty
      ~f:(fun vendored (project : Dune_project.t) ->
        let project_root = Dune_project.root project in
        let* dir_opt = Source_tree.find_dir project_root in
        match dir_opt with
        | None -> Memo.return vendored
        | Some dir ->
          (match Source_tree.Dir.status dir with
          | Vendored ->
            let pkgs = Dune_project.including_hidden_packages project in
            Memo.return (Package.Name.Set.of_keys pkgs |> Package.Name.Set.union vendored)
          | Normal | Data_only -> Memo.return vendored))
  in
  let non_vendored_list =
    Package.Name.Map.keys packages
    |> List.filter ~f:(fun pkg_name -> not (Package.Name.Set.mem vendored_packages pkg_name))
  in
  Memo.return non_vendored_list
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
      | Lib (pkg_name, lib) ->
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
        let lib_path = pkg_path ^ "/" ^ Lib_name.to_string (Lib.name lib) in
        (* Library also maps to package URL, matching odoc_driver behavior *)
        Memo.return [ pkg_mapping; lib_path, pkg_url ])
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

(* Convert a Lib.t to a target using Package_discovery for installed libraries.
   For local libraries, uses Lib_info.package (which is accurate for local libs).
   For installed libraries, uses Package_discovery (which is accurate for installed libs). *)
let target_of_lib pkg_discovery (lib : Lib.t) =
  match Lib.Local.of_lib lib with
  | Some local_lib ->
    (* Local library - check if it has a package using Lib_info *)
    let lib_info = Lib.info lib in
    (match Lib_info.package lib_info with
     | Some pkg -> Memo.return (Lib (pkg, lib))
     | None ->
       (* Private library without a package *)
       let lib_unique_name = lib_unique_name local_lib in
       Memo.return (Private_lib (lib_unique_name, lib)))
  | None ->
    (* Installed library - use Package_discovery to get correct package *)
    (match Package_discovery.package_of_library pkg_discovery lib with
     | Some pkg -> Memo.return (Lib (pkg, lib))
     | None ->
       (* Installed library without a package - treat as private lib *)
       let lib_unique_name = Lib_name.to_string (Lib.name lib) in
       Memo.return (Private_lib (lib_unique_name, lib)))
;;

module Flags = struct
  type warnings = Dune_env.Odoc.warnings =
    | Fatal
    | Nonfatal

  type sidebar = Dune_env.Odoc.sidebar =
    | Global
    | Per_package

  type t =
    { warnings : warnings
    ; sidebar : sidebar
    }

  let default = { warnings = Nonfatal; sidebar = Global }

  let get_memo ~dir =
    Env_stanza_db.value ~default ~dir ~f:(fun config ->
      let warnings = Option.value config.odoc.warnings ~default:default.warnings in
      let sidebar = Option.value config.odoc.sidebar ~default:default.sidebar in
      Memo.return (Some { warnings; sidebar }))
  ;;

  let get ~dir = get_memo ~dir |> Action_builder.of_memo
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

let run_odoc sctx command ~quiet ~flags_for args =
  let ctx = Super_context.context sctx in
  let build_dir = Context.build_dir ctx in
  let program = odoc_program sctx build_dir in
  let dir = Path.build (Paths.root ctx) in
  let base_flags =
    let open Action_builder.O in
    let* () = Action_builder.return () in
    match flags_for with
    | None -> Action_builder.return Command.Args.empty
    | Some path -> odoc_base_flags quiet path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  let run =
    Action_builder.with_no_targets deps
    >>> Command.run_dyn_prog ~dir program [ A command; Dyn base_flags; S args ]
  in
  (* For external artifacts (quiet=true), suppress both stdout and stderr *)
  if quiet
  then Action_builder.With_targets.map run ~f:(fun action -> Action.Full.map action ~f:Action.ignore_outputs)
  else run
;;

(* Get the stdlib library, if available *)
let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

(* Common helper to get library paths with optional stdlib *)
let get_lib_paths ctx ~stdlib_opt requires pkg_discovery =
  let open Resolve.O in
  let+ libs = requires in
  (* Add stdlib to the list of libraries if provided and not already present *)
  let libs =
    match stdlib_opt with
    | Some stdlib ->
      if
        List.exists libs ~f:(fun lib ->
          Lib_name.equal (Lib.name lib) (Lib.name stdlib))
      then libs
      else stdlib :: libs
    | None -> libs
  in
  List.filter_map libs ~f:(fun lib ->
    match Lib.Local.of_lib lib with
    | None ->
      (* Installed library *)
      let lib_pkg_opt = Package_discovery.package_of_library pkg_discovery lib in
      Option.map lib_pkg_opt ~f:(fun lib_pkg -> lib, Paths.odocs ctx (Lib (lib_pkg, lib)))
    | Some local_lib ->
      (* Local library *)
      let lib_t = Lib.Local.to_lib local_lib in
      let lib_info = Lib.info lib_t in
      let target =
        match Lib_info.package lib_info with
        | Some pkg -> Lib (pkg, lib_t)
        | None ->
          let lib_unique_name = lib_unique_name local_lib in
          Private_lib (lib_unique_name, lib_t)
      in
      Some (lib, Paths.odocs ctx target))
;;

let odoc_include_flags ctx pkg requires pkg_discovery =
  let open Memo.O in
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  let args =
    Resolve.args
      (let open Resolve.O in
       let+ lib_paths = get_lib_paths ctx ~stdlib_opt requires pkg_discovery in
       let paths =
         List.fold_left lib_paths ~init:Path.Set.empty ~f:(fun paths (_lib, path) ->
           Path.Set.add paths (Path.build path))
       in
       let paths =
         match pkg with
         | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
         | None -> paths
       in
       Command.Args.S
         (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
            [ Command.Args.A "-I"; Path dir ])))
  in
  Memo.return args
;;

(* Generate -L library:path flags for odoc link
   These tell odoc where to find .odocl files for library dependencies *)
let odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery =
  Resolve.args
    (let open Resolve.O in
     let+ lib_paths = get_lib_paths ctx ~stdlib_opt requires pkg_discovery in
     (* Deduplicate by library name and make paths relative *)
     let doc_root = Paths.root ctx in
     let lib_paths_map =
       List.fold_left lib_paths ~init:Lib_name.Map.empty ~f:(fun acc (lib, odoc_dir) ->
         let lib_name = Lib.name lib in
         if Lib_name.Map.mem acc lib_name
         then acc
         else (
           let lib_name_str = Lib_name.to_string lib_name in
           (* Compute path relative to doc_root using proper path functions *)
           let odoc_path_rel =
             Path.reach (Path.build odoc_dir) ~from:(Path.build doc_root)
           in
           let lib_path_arg = lib_name_str ^ ":" ^ odoc_path_rel in
           Lib_name.Map.set acc lib_name lib_path_arg))
     in
     (* Convert map to args *)
     let lib_args =
       Lib_name.Map.values lib_paths_map
       |> List.concat_map ~f:(fun lib_path_arg -> [ Command.Args.A "-L"; A lib_path_arg ])
     in
     Command.Args.S lib_args)
;;

(* Get package dependencies from odoc-config.sexp for a set of packages *)
let get_config_package_deps pkg_discovery pkgs =
  List.concat_map pkgs ~f:(fun pkg ->
    let config = Package_discovery.config_of_package pkg_discovery pkg in
    config.Odoc_config.deps.packages)
;;

(* Generate -P package:path flags for odoc link.
   These tell odoc where to find .odoc files for package dependencies. *)
let odoc_pkg_flags ctx pkg_discovery ~current_pkg_opt ~artifact_config =
  let doc_root = Paths.root ctx in
  (* Collect direct packages (config deps + current package) and their transitive config deps *)
  let direct_pkgs =
    artifact_config.Odoc_config.deps.packages @ Option.to_list current_pkg_opt
  in
  let all_pkgs = direct_pkgs @ get_config_package_deps pkg_discovery direct_pkgs in
  (* Build unique package map with their odoc paths *)
  let pkg_paths =
    List.fold_left all_pkgs ~init:Package.Name.Map.empty ~f:(fun acc pkg ->
      let odoc_dir = Paths.odocs ctx (Pkg pkg) in
      let path = Path.reach (Path.build odoc_dir) ~from:(Path.build doc_root) in
      Package.Name.Map.set acc pkg path)
  in
  (* Generate -P pkg:path flags *)
  let flags =
    Package.Name.Map.to_list_map pkg_paths ~f:(fun pkg path ->
      Log.info
        [ Pp.textf "odoc_pkg_flags: Adding -P %s:%s" (Package.Name.to_string pkg) path ];
      [ Command.Args.A "-P"; A (Package.Name.to_string pkg ^ ":" ^ path) ])
    |> List.concat
  in
  Command.Args.S flags
;;

(* Compute library dependencies for an artifact.

   Returns a pair (closure, external_requires):
   - closure: Full transitive closure of library dependencies including self
   - external_requires: Transitive dependencies excluding self and same-package libraries

   Handles the special case of stdlib: when compiling stdlib itself, we don't add
   stdlib to its own dependency list to avoid cycles. *)
let compute_artifact_library_deps ctx ~artifact ~package_lib_names =
  (* Get stdlib for dependency resolution *)
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  (* Check if this artifact is part of stdlib *)
  let is_stdlib_artifact =
    match stdlib_opt with
    | Some stdlib -> Lib_name.equal (Lib.name stdlib) (Artifact.lib_name artifact)
    | None -> false
  in
  (* Get TRANSITIVE closure of dependencies (not just direct requires) *)
  let* closure =
    match Artifact.lib artifact with
    | Some lib ->
      (* Include stdlib in the closure unless we're compiling stdlib itself *)
      let libs_to_close =
        if is_stdlib_artifact then [ lib ] else lib :: Option.to_list stdlib_opt
      in
      Lib.closure libs_to_close ~linking:false
    | None -> Memo.return (Resolve.return [])
    (* Package-level and toplevel artifacts have no library dependencies *)
  in
  (* For library dependency aliases, filter out:
     1. The library itself (to avoid self-dependency)
     2. Other libraries in the same package (to avoid circular dependencies)
     Module-level dependencies from odoc compile-deps will handle the actual file dependencies.
     This prevents circular dependencies when libraries in the same package have circular module deps
     (e.g., OCaml's compiler-libs where compiler-libs.common's Meta depends on compiler-libs.bytecomp's Instruct). *)
  let external_requires =
    match Artifact.lib artifact with
    | Some lib ->
      Resolve.map closure ~f:(fun all_libs ->
        List.filter all_libs ~f:(fun dep_lib ->
          let dep_lib_name = Lib.name dep_lib in
          not (Lib_name.equal dep_lib_name (Lib.name lib))
          && not (Lib_name.Set.mem package_lib_names dep_lib_name)))
    | None -> closure
  in
  Memo.return (closure, external_requires)
;;

(* Compute intra-library module dependencies using odoc compile-deps.

   For Module artifacts, generates a .deps file and parses it to find which other
   modules in the same library this module depends on. Returns an Action_builder
   that creates dependencies on the .odoc files of those modules.

   For Page artifacts, returns an empty Action_builder since pages don't have module deps. *)
let compute_intra_library_module_deps sctx ~ctx ~artifact ~lib_artifacts_by_module =
  let source_file = Artifact.source_file artifact in
  match Artifact.kind artifact with
  | Page _ ->
    (* Pages don't have module dependencies *)
    Memo.return (Action_builder.return ())
  | Module ({ module_name; _ }, _) ->
    (* Generate deps file using odoc compile-deps *)
    let module_name_str = Module_name.to_string module_name in
    let output_dir = Artifact.odoc_dir ctx artifact in
    let deps_file = Path.Build.relative output_dir (module_name_str ^ ".deps") in
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
       (* Find .odoc files for dependencies in the same library using the prebuilt map *)
       let current_module_name =
         match Artifact.kind artifact with
         | Module ({ module_name; _ }, _) -> Some module_name
         | Page _ -> None
       in
       let dep_odoc_files =
         List.filter_map dep_modules ~f:(fun dep_module ->
           (* Skip self-dependencies *)
           match current_module_name with
           | Some current when Module_name.equal current dep_module -> None
           | _ ->
             Module_name.Map.find lib_artifacts_by_module dep_module)
       in
       Dune_engine.Dep.Set.of_files dep_odoc_files |> Action_builder.deps)
;;

(* Unified compilation function that computes all dependencies from the artifact.
   This replaces the separate compile_module, compile_mld, compile_installed_module_artifact functions.
   All information needed for compilation is now in the artifact itself.

   For Module artifacts, we use `odoc compile-deps` to determine intra-library module dependencies.
   For Page artifacts (mld files), we compile directly without module dependencies. *)
let compile_artifact sctx ~artifact ~lib_artifacts_by_module ~package_lib_names =
  let ctx = Super_context.context sctx in
  let source_file = Artifact.source_file artifact in
  (* For Module artifacts, run compile-deps to find intra-library module dependencies *)
  let* module_deps =
    compute_intra_library_module_deps sctx ~ctx ~artifact ~lib_artifacts_by_module
  in
  (* Compute library dependencies from the artifact's target *)
  let* closure, external_requires =
    compute_artifact_library_deps ctx ~artifact ~package_lib_names
  in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let* include_flags = odoc_include_flags ctx None closure pkg_discovery in
  (* For installed packages or vendored libraries, suppress output (both stdout and stderr) *)
  let* should_suppress = Artifact.should_suppress_output artifact in
  (* Create dependencies on all required libraries' .odoc files (via .odoc-all aliases)
     IMPORTANT: Pass empty list for pkg during compilation to avoid creating a dependency cycle
     on our own package's .odoc-all alias. The package alias is only needed during linking.
     Use external_requires which excludes self and same-package libraries. *)
  let lib_deps = Dep.deps ctx [] external_requires in
  let run_odoc =
    let open Action_builder.With_targets.O in
    (* Depend on: 1) intra-library module deps, 2) inter-library deps *)
    Action_builder.with_no_targets module_deps
    >>> Action_builder.with_no_targets lib_deps
    >>> Action_builder.With_targets.add
          ~file_targets:[ Artifact.odoc_file ctx artifact ]
          (run_odoc
             sctx
             "compile"
             ~quiet:should_suppress
             ~flags_for:(Some (Artifact.odoc_file ctx artifact))
             [ (* Include paths for all dependency libraries including stdlib and current library *)
               include_flags
             ; (* Use --output-dir + --parent-id for artifacts with parents,
                  or -o for toplevel without parent *)
               (let parent = Artifact.parent_id artifact in
                if String.is_empty parent
                then
                  Command.Args.S
                    [ Command.Args.A "-o"
                    ; Command.Args.Target (Artifact.odoc_file ctx artifact)
                    ]
                else
                  Command.Args.S
                    [ Command.Args.A "--output-dir"
                    ; Command.Args.A "_odoc"
                    ; Command.Args.A "--parent-id"
                    ; Command.Args.A parent
                    ])
             ; Command.Args.A "--enable-missing-root-warning"
             ; (* Add --warnings-tag flag for library artifacts to identify which package warnings come from.
                  Skip for vendored libraries since their warnings are suppressed anyway. *)
               (if should_suppress
                then Command.Args.S []
                else
                  match Artifact.kind artifact with
                  | Module (_, Lib (pkg, _)) ->
                    Command.Args.As [ "--warnings-tag"; Package.Name.to_string pkg ]
                  | Module (_, Private_lib _) ->
                    Command.Args.As [ "--warnings-tag"; "__private_lib__" ]
                  | Page _ ->
                    (* Package-level and toplevel artifacts don't have a warnings tag *)
                    Command.Args.S [])
             ; Command.Args.Dep source_file
             ])
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

let link_odoc_rules sctx (odoc_file : Artifact.t) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  (* Collect all packages we need dependencies for: current package + config packages *)
  let all_pkgs =
    Option.to_list pkg @ (Artifact.odoc_config odoc_file).Odoc_config.deps.packages
  in
  let deps = Dep.deps ctx all_pkgs requires in
  let* stdlib_opt = stdlib_lib (Context.name ctx) in
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  (* Get all packages in the workspace to pass as --warnings-tags *)
  let* packages = Dune_load.packages () in
  let all_pkg_names =
    Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
  in
  (* Build --warnings-tags arguments for all packages, plus __private_lib__ for private libraries *)
  let warnings_tags_args =
    Command.Args.S
      (List.concat_map ("__private_lib__" :: all_pkg_names) ~f:(fun pkg_name ->
         [ Command.Args.A "--warnings-tags"; Command.Args.A pkg_name ]))
  in
  (* Suppress output for installed packages and vendored libraries *)
  let* quiet = Artifact.should_suppress_output odoc_file in
  (* Note: odoc_lib_flags handles -L flags for all libraries in requires,
     including the library itself (which compute_link_requires adds to requires) *)
  let run_odoc =
    run_odoc
      sctx
      "link"
      ~quiet
      ~flags_for:(Some (Artifact.odoc_file ctx odoc_file))
      [ odoc_lib_flags ctx ~stdlib_opt requires pkg_discovery
      ; odoc_pkg_flags ctx pkg_discovery ~current_pkg_opt:(Artifact.pkg odoc_file) ~artifact_config:(Artifact.odoc_config odoc_file)
      ; (* Add --current-package flag when we have a package *)
        (match Artifact.pkg odoc_file with
         | Some pkg_name ->
           Command.Args.As [ "--current-package"; Package.Name.to_string pkg_name ]
         | None -> Command.Args.S [])
      ; A "--enable-missing-root-warning"
      ; warnings_tags_args
      ; A "-o"
      ; Target (Artifact.odocl_file ctx odoc_file)
      ; Dep (Path.build (Artifact.odoc_file ctx odoc_file))
      ]
  in
  (* For installed packages or vendored libraries, suppress output (both stdout and stderr) *)
  let run_odoc =
    if quiet
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
  let doc_root = Paths.root ctx in
  (* Compute relative paths from doc_root (_doc) for working directory paths *)
  let html_root_rel = Path.reach (Path.build html_root) ~from:(Path.build doc_root) in
  (* Compute relative paths from html_root for URIs (since URIs are relative to -o argument) *)
  let odoc_support_uri = Path.reach (Path.build odoc_support_path) ~from:(Path.build html_root) in
  let search_args = Sherlodoc.odoc_args sctx ~search_db ~dir_sherlodoc_dot_js:html_root ~html_root:html_root in
  (* Generate HTML for all output formats *)
  Memo.List.iter Output_format.all ~f:(fun out ->
    let html_file = Output_format.target ctx mode out artifact in
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
    let* quiet = Artifact.should_suppress_output artifact in
    let run_odoc =
      run_odoc
        sctx
        "html-generate"
        ~quiet
        ~flags_for:None
        [ search_args
        ; A "-o"
        ; A html_root_rel
        ; A "--support-uri"
        ; A odoc_support_uri
        ; A "--theme-uri"
        ; A odoc_support_uri
        ; (match remap_file with
           | None -> S []
           | Some rf -> S [ A "--remap-file"; Dep (Path.build rf) ])
        ; (match sidebar_file with
           | Some sf -> S [ A "--sidebar"; Dep (Path.build sf) ]
           | None -> S [])
        ; Dep (Path.build (Artifact.odocl_file ctx artifact))
        ; Output_format.args out
        ; (match html_dir_opt with
           | None -> Hidden_targets [ html_file ]
           | Some _ -> Command.Args.empty)
        ]
    in
    (* Add explicit dependency on CSS/support files *)
    (* Note: output suppression for external artifacts is handled by run_odoc via the quiet flag *)
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
    }

  let of_packages packages =
    Package.Name.Map.to_list_map packages ~f:(fun name package ->
      let name = Package.Name.to_string name in
      { name; version = Package.version package })
  ;;

  let mld_content t =
    let b = Buffer.create 1024 in
    Printf.bprintf b "{0 OCaml package documentation}\n\n";
    List.iter t ~f:(fun { name; version } ->
      let version_suffix =
        match version with
        | None -> ""
        | Some v -> sp " (%s)" (Package_version.to_string v)
      in
      Printf.bprintf b "- {{!/%s/page-index}%s}%s\n" name name version_suffix);
    Buffer.contents b
  ;;
end

let library_index_content_from_artifacts ~lib_name ~artifacts =
  let b = Buffer.create 256 in
  Printf.bprintf b "@toc_status hidden\n";
  Printf.bprintf b "@order_category libraries\n";
  Printf.bprintf b "{0 Library [%s]}\n" (Lib_name.to_string lib_name);
  (* Extract non-hidden, visible modules from artifacts.
     Artifact.hidden filters out implementation modules (Foo__Bar, Foo__). *)
  let module_names =
    List.filter_map artifacts ~f:(fun artifact ->
      if Artifact.hidden artifact then None
      else
        match Artifact.kind artifact with
        | Module ({ visible = true; module_name; _ }, _) -> Some module_name
        | Module ({ visible = false; _ }, _) | Page _ -> None)
    |> List.sort ~compare:Module_name.compare
  in
  if not (List.is_empty module_names) then (
    Printf.bprintf b "{!modules:";
    List.iter module_names ~f:(fun m ->
      Printf.bprintf b " %s" (Module_name.to_string m));
    Printf.bprintf b "}\n"
  );
  Buffer.contents b
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
    match Artifact.kind artifact with
    | Module (_, (Lib (_, lib) | Private_lib (_, lib))) ->
      (* Module in a library: use library's transitive dependencies PLUS the library itself.
       This ensures all modules in the library (including hidden/wrapped ones) are compiled
       before any module is linked. Critical for wrapped libraries.
       Use closure to get transitive dependencies, needed for resolving installed library deps. *)
      let* closure = Lib.closure [ lib ] ~linking:false in
      Memo.return
        (Resolve.bind closure ~f:(fun libs ->
           (* Add the library itself to ensure .odoc-all dependency includes all modules *)
           Resolve.return (lib :: libs)))
    | Page ({ pkg_libs; _ }, (Pkg _ | Toplevel)) ->
      (* Page in a package or toplevel: use the libraries that were recorded when the artifact was created *)
      Memo.return (Resolve.return pkg_libs)
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
    (* Get all libraries from extra packages *)
    let* extra_libs_from_pkgs =
      match Artifact.kind artifact with
      | Page (_, Toplevel) ->
        (* For Toplevel, don't try to load libraries - just use packages for -P flags *)
        Memo.return []
      | Module _ | Page (_, Pkg _) ->
        if List.is_empty extra_pkg_names
        then Memo.return []
        else (
          let* packages = Dune_load.packages () in
          let* pkg_discovery = Package_discovery.create ~context:ctx in
          Memo.parallel_map extra_pkg_names ~f:(fun pkg_name ->
            (* Check if this is a local package or an installed package *)
            if Package.Name.Map.mem packages pkg_name
            then
              (* Local package: use libs_of_pkg to get libraries *)
              let* local_libs = Context.name ctx |> libs_of_pkg ~pkg:pkg_name in
              Memo.return (List.map local_libs ~f:Lib.Local.to_lib)
            else
              (* Installed package: use Package_discovery *)
              Memo.return (Package_discovery.libraries_of_package pkg_discovery pkg_name))
          >>| List.concat)
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

(* Helper to create the toplevel index artifact with package dependencies *)
let toplevel_index_artifact ctx packages =
  let mld_path = Paths.toplevel_index_mld ctx in
  let page = { name = "index"; pkg_libs = [] } in
  let source = Local_source mld_path in
  let kind = Page (page, Toplevel) in
  (* Add all packages as dependencies so -P flags are generated during linking *)
  let package_names = Package.Name.Map.keys packages in
  let odoc_config = { Odoc_config.deps = { packages = package_names; libraries = [] } } in
  Artifact.create ~kind ~source ~odoc_config
;;

(* Generate the .mld file for the toplevel index *)
let setup_toplevel_index_mld sctx =
  let ctx = Super_context.context sctx in
  let* packages = Dune_load.packages () in
  let index = Toplevel_index.of_packages packages in
  let mld_content = Toplevel_index.mld_content index in
  let mld_path = Paths.toplevel_index_mld ctx in
  add_rule sctx (Action_builder.write_file mld_path mld_content)
;;

(* Compile the toplevel index .mld to .odoc *)
let setup_toplevel_index_compile sctx =
  let ctx = Super_context.context sctx in
  let* packages = Dune_load.packages () in
  let artifact = toplevel_index_artifact ctx packages in
  compile_artifact
    sctx
    ~artifact
    ~lib_artifacts_by_module:Module_name.Map.empty
    ~package_lib_names:Lib_name.Set.empty
;;

(* Link the toplevel index .odoc to .odocl *)
let setup_toplevel_index_link sctx =
  let ctx = Super_context.context sctx in
  let* packages = Dune_load.packages () in
  let artifact = toplevel_index_artifact ctx packages in
  link_artifact sctx ~artifact
;;

(* Generate HTML for the toplevel index *)
let setup_toplevel_index_html sctx mode =
  let ctx = Super_context.context sctx in
  let* packages = Dune_load.packages () in
  let artifact = toplevel_index_artifact ctx packages in
  (* Create search_db for just the toplevel index *)
  let* search_db =
    let dir = Paths.html_root ctx mode in
    let odocls = [ Artifact.odocl_file ctx artifact ] in
    Sherlodoc.search_db sctx ~dir ~external_odocls:[] odocls
  in
  (* Determine sidebar file based on mode and env config *)
  let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
  let sidebar_file =
    match mode, flags.sidebar with
    | Doc_mode.Local_only, Flags.Global ->
      (* Use global sidebar for Local_only mode with global sidebar config *)
      Some (Paths.sidebar_file ctx Global)
    | Doc_mode.Local_only, Flags.Per_package
    | Doc_mode.Full, _ ->
      (* No sidebar for per-package mode or Full mode toplevel index *)
      None
  in
  (* Generate HTML for the artifact *)
  generate_html_artifact sctx ~artifact ~search_db ~sidebar_file ~mode ()
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

(* Discover mld files for an installed package and create artifacts *)
let discover_installed_pkg_mld_artifacts ctx ~pkg ~pkg_libs : Artifact.t list Memo.t =
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let mld_files = Package_discovery.mlds_of_package pkg_discovery pkg in
  let* pkg_mld_artifacts =
    Memo.List.filter_map mld_files ~f:(fun mld_path ->
      (* Extract hierarchical path from full path by looking for "odoc-pages" component.
         Example: /path/to/doc/pkg/odoc-pages/deprecated/index.mld
         We want to extract "deprecated/index" *)
      let page_name_with_path =
        (* Walk up the path to find the odoc-pages ancestor directory *)
        let rec find_odoc_pages_ancestor p =
          match Path.parent p with
          | None -> None
          | Some parent ->
            if Path.basename parent = "odoc-pages"
            then Some parent
            else find_odoc_pages_ancestor parent
        in
        match find_odoc_pages_ancestor mld_path with
        | Some odoc_pages_dir ->
          (* Get relative path using string operations since Path.descendant
             doesn't work for External paths *)
          let parent_str = Path.to_string odoc_pages_dir in
          let mld_str = Path.to_string mld_path in
          let prefix_len = String.length parent_str in
          if String.is_prefix mld_str ~prefix:parent_str
             && String.length mld_str > prefix_len
             && (mld_str.[prefix_len] = '/' || mld_str.[prefix_len] = '\\')
          then
            let rel_str = String.drop mld_str (prefix_len + 1) in
            (* Normalize path separators to forward slashes for odoc page names *)
            let rel_str = String.map rel_str ~f:(function '\\' -> '/' | c -> c) in
            (match Filename.remove_extension rel_str with
             | "" -> rel_str
             | s -> s)
          else
            (* Shouldn't happen, but fallback to basename *)
            Path.basename mld_path |> Filename.remove_extension
        | None ->
          (* odoc-pages not found in path, use basename *)
          Path.basename mld_path |> Filename.remove_extension
      in
      (* Include all mld files, including index.mld if hand-written *)
      let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
      let page = { name = page_name_with_path; pkg_libs } in
      let kind = Page (page, Pkg pkg) in
      Memo.return
        (Some
           (Artifact.create
              ~kind
              ~source:(Installed_source { src_path = mld_path })
              ~odoc_config)))
  in
  (* Library index.mld artifacts for installed packages - not yet implemented *)
  Memo.return pkg_mld_artifacts
;;

(* Discover modules for an installed library and create artifacts *)
(* Get archive names for a library (used to filter odoc classify output) *)
let get_archive_names lib_name archives =
  let byte_archives = Mode.Dict.get archives Mode.Byte in
  match byte_archives with
  | [] -> if Lib_name.equal lib_name (Lib_name.of_string "stdlib") then [ "stdlib" ] else []
  | archives -> List.map archives ~f:(fun p -> Path.basename p |> Filename.remove_extension)
;;

(* Parse odoc classify output to extract module names for specific archives *)
let parse_classify_output ~archive_names classify_content =
  let classify_lines = String.split_lines classify_content in
  List.concat_map classify_lines ~f:(fun line ->
    match String.split line ~on:' ' |> List.filter ~f:(fun s -> not (String.is_empty s)) with
    | [] -> []
    | archive :: mods ->
      if List.mem archive_names archive ~equal:String.equal then mods else [])
;;

(* Get entry module names for visibility determination *)
let get_entry_module_names info =
  match Lib_info.entry_modules info with
  | Lib_info.Source.Local -> []
  | Lib_info.Source.External (Ok module_names) ->
    List.map module_names ~f:Module_name.to_string
  | Lib_info.Source.External (Error _) -> []
;;

let discover_installed_lib_artifacts _sctx ctx ~pkg ~lib_name ~lib : Artifact.t list Memo.t =
  let pkg_name_str = Package.Name.to_string pkg in
  let lib_name_str = Lib_name.to_string lib_name in
  let info = Lib.info lib in
  let archive_names = get_archive_names lib_name (Lib_info.archives info) in
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
    (* Read and parse classify file to get module names *)
    let classify_path =
      Paths.root ctx ++ "classify" ++ pkg_name_str ++ lib_name_str ++ "odoc.classify"
    in
    let* classify_content = Build_system.read_file (Path.build classify_path) in
    let all_module_names = parse_classify_output ~archive_names classify_content in
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
      let entry_module_names = get_entry_module_names info in
      let* pkg_discovery = Package_discovery.create ~context:ctx in
      let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
      (* Create artifacts for all modules *)
      let+ all_module_artifacts =
        Memo.parallel_map all_module_names ~f:(fun module_name ->
          match Package_discovery.module_source_file pkg_discovery ~lib ~module_name with
          | Some src_path ->
            let mod_ =
              { visible = List.mem entry_module_names module_name ~equal:String.equal
              ; module_name = Module_name.of_string module_name
              }
            in
            Memo.return
              (Some
                 (Artifact.create
                    ~kind:(Module (mod_, Lib (pkg, lib)))
                    ~source:(Installed_source { src_path })
                    ~odoc_config))
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
let create_artifact_module ~target ~local_lib ~module_ ~odoc_config =
  let mod_ =
    { visible = Module.visibility module_ = Visibility.Public
    ; module_name = Module_name.Unique.to_name (Module.obj_name module_) ~loc:Loc.none
    }
  in
  let kind = Module (mod_, target) in
  let obj_dir = Lib.Local.obj_dir local_lib in
  let source_file = Obj_dir.Module.cmti_file obj_dir module_ ~cm_kind:(Ocaml Cmi) in
  Artifact.create ~kind ~source:(Local_source source_file) ~odoc_config
;;

(* Discover modules for a local library and create artifacts.
   Handles both v3 libraries (with packages) and v2 libraries (without packages). *)
let discover_local_lib_artifacts sctx ctx ~pkg ~lib_name ~local_lib : Artifact.t list Memo.t
  =
  let* all_modules = Dir_contents.modules_of_local_lib sctx local_lib in
  let modules = Modules.fold all_modules ~init:[] ~f:(fun m acc -> m :: acc) in
  (* Check if this is a v2 library (no package) *)
  let info = Lib.Local.info local_lib in
  let actual_pkg = Lib_info.package info in
  (* Get odoc config for the package *)
  let* pkg_discovery = Package_discovery.create ~context:ctx in
  let odoc_config = Package_discovery.config_of_package pkg_discovery pkg in
  let lib_t = Lib.Local.to_lib local_lib in
  let target =
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
      Private_lib (lib_unique_name, lib_t)
    | Some _ ->
      (* v3 library - use pkg/lib directory structure *)
      Lib (pkg, lib_t)
  in
  let artifacts =
    List.map modules ~f:(fun module_ ->
      create_artifact_module ~target ~local_lib ~module_ ~odoc_config)
  in
  Memo.return artifacts
;;

(* Unified artifact discovery for both local and installed libraries *)
let discover_lib_artifacts sctx ctx ~pkg ~lib_name ~lib : Artifact.t list Memo.t =
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
           let* mlds_list = Packages.mlds sctx pkg in
           (* Convert mld list to (path, name) pairs *)
           let mlds_pairs =
             List.map mlds_list ~f:(fun (mld : Doc_sources.mld) ->
               let in_doc_str = Path.Local.to_string mld.in_doc in
               let name = Filename.remove_extension in_doc_str in
               mld.path, name)
           in
           let mlds = check_mlds_no_dupes ~pkg ~mlds:mlds_pairs in
           let ctx = Super_context.context sctx in
           let* mlds =
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
               String.Map.set mlds "index.mld" (gen_mld, "index.mld"))
           in
           (* Generate library index.mld files for all libraries in the package *)
           let* local_libs = Context.name ctx |> libs_of_pkg ~pkg in
           let+ mlds =
             Memo.List.fold_left local_libs ~init:mlds ~f:(fun mlds local_lib ->
               let lib = Lib.Local.to_lib local_lib in
               let lib_name = Lib.name lib in
               let lib_index_key = sp "%s/index.mld" (Lib_name.to_string lib_name) in
               if String.Map.mem mlds lib_index_key
               then Memo.return mlds
               else (
                 (* Generate library index.mld based on discovered artifacts *)
                 let* artifacts = discover_local_lib_artifacts sctx ctx ~pkg ~lib_name ~local_lib in
                 let index_mld_path = Paths.lib_index_mld ctx pkg lib_name in
                 let content = library_index_content_from_artifacts ~lib_name ~artifacts in
                 let+ () = add_rule sctx (Action_builder.write_file index_mld_path content) in
                 String.Map.set mlds lib_index_key (index_mld_path, lib_index_key)))
           in
           mlds))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)
;;

(* Helper function to compile artifacts for a single library with proper dependencies *)
(* Discover ALL artifacts for a package identifier (either a v3 package name or v2 lib_unique_name).
   For v3 packages: returns artifacts for all libraries in the package + package-level mld files
   For v2 libraries: returns artifacts for all modules in the library (no mld files since v2 has no packages)

   This is the unified entry point that all handlers (odoc, odocls, html) should use. *)
let discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name
  : (Artifact.t list * string list) Memo.t
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
    let* local_libs =
      if is_project_pkg then Context.name ctx |> libs_of_pkg ~pkg else Memo.return []
    in
    if is_project_pkg && not (List.is_empty local_libs)
    then
      (* Local package with actual libraries - discover mld artifacts + all library artifacts *)
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
        (* Get source mld files and compute what library indices will be generated.
           Don't call package_mlds here as it would generate rules (rules are generated
           in handle_mlds_dir). We just need to know what artifacts will exist. *)
        let* source_mlds = Packages.mlds sctx pkg in
        let target = Pkg pkg in
        let ctx = Super_context.context sctx in

        (* Create artifacts for source mlds *)
        let source_artifacts =
          List.map source_mlds ~f:(fun (mld : Doc_sources.mld) ->
            let in_doc_str = Path.Local.to_string mld.in_doc in
            let name = Filename.remove_extension in_doc_str in
            let page = { name; pkg_libs } in
            let kind = Page (page, target) in
            Artifact.create ~kind ~source:(Local_source mld.path) ~odoc_config)
        in

        (* Create artifacts for generated package index if it doesn't exist in sources *)
        let has_index_mld =
          List.exists source_mlds ~f:(fun (mld : Doc_sources.mld) ->
            Path.Local.to_string mld.in_doc = "index.mld")
        in
        let package_index_artifact =
          if has_index_mld then []
          else
            let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
            let page = { name = "index"; pkg_libs } in
            let kind = Page (page, target) in
            [ Artifact.create ~kind ~source:(Local_source gen_mld) ~odoc_config ]
        in

        (* Create artifacts for library index.mld files *)
        let* library_index_artifacts =
          Memo.List.map local_libs ~f:(fun local_lib ->
            let lib = Lib.Local.to_lib local_lib in
            let lib_name = Lib.name lib in
            let lib_index_key = sp "%s/index.mld" (Lib_name.to_string lib_name) in
            (* Check if there's a source mld for this library index *)
            let has_source_lib_index =
              List.exists source_mlds ~f:(fun (mld : Doc_sources.mld) ->
                Path.Local.to_string mld.in_doc = lib_index_key)
            in
            if has_source_lib_index then
              Memo.return None
            else
              let index_mld_path = Paths.lib_index_mld ctx pkg lib_name in
              let name = sp "%s/index" (Lib_name.to_string lib_name) in
              let page = { name; pkg_libs } in
              let kind = Page (page, target) in
              Memo.return (Some (Artifact.create ~kind ~source:(Local_source index_mld_path) ~odoc_config)))
        in
        let library_index_artifacts = List.filter_map library_index_artifacts ~f:Fun.id in

        Memo.return (source_artifacts @ package_index_artifact @ library_index_artifacts)
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

(* Generate index file from linked .odocl files *)
let generate_index sctx ~scope ~packages ~odocl_files =
  let ctx = Super_context.context sctx in
  let index_file = Paths.index_file ctx scope in
  (* compile-index needs all .odocl files:
     - Library .odocl files
     - Package-level mld .odocl files *)
  let open Command.Args in
  (* Pass all .odocl files as dependencies *)
  let odocl_file_args =
    List.map odocl_files ~f:(fun odocl_file -> Dep (Path.build odocl_file))
  in
  let action =
    let open Action_builder.With_targets.O in
    (* Depend on all packages' .odoc-all aliases *)
    Action_builder.with_no_targets
      (Action_builder.all_unit
         (List.map packages ~f:(fun pkg ->
            let odocl_dir = Paths.odocl ctx (Pkg pkg) in
            let pkg_alias = Dep.odoc_all_alias ~dir:odocl_dir in
            Action_builder.dep (Dune_engine.Dep.alias pkg_alias))))
    >>> run_odoc
          sctx
          "compile-index"
          ~quiet:false
          ~flags_for:None
          ([ A "-o"; Target index_file ] @ odocl_file_args)
  in
  let* () = add_rule sctx action in
  Memo.return index_file
;;

(* Generate binary sidebar file from its index - called in _sidebar handler *)
let generate_sidebar_binary sctx ~scope ~index_file =
  let ctx = Super_context.context sctx in
  let sidebar_file = Paths.sidebar_file ctx scope in
  (* Generate binary sidebar - run from _doc directory with relative path to index *)
  let index_relative_path =
    match scope with
    | Global -> "_sidebar/index.odoc-index"
    | Per_package pkg -> sprintf "_sidebar/%s/index.odoc-index" (Package.Name.to_string pkg)
  in
  let* () =
    let action =
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets (Action_builder.path (Path.build index_file))
      >>> run_odoc
            sctx
            "sidebar-generate"
            ~quiet:false
            ~flags_for:None
            [ A "-o"; Target sidebar_file; A index_relative_path ]
    in
    add_rule sctx action
  in
  Memo.return sidebar_file
;;

(* Generate JSON sidebar file from its index - called in _html handler *)
let generate_sidebar_json sctx ~mode ~scope ~index_file =
  let ctx = Super_context.context sctx in
  let sidebar_json = Paths.sidebar_json ctx mode scope in
  (* Generate JSON sidebar - run from _doc directory with relative path to index *)
  let index_relative_path =
    match scope with
    | Global -> "_sidebar/index.odoc-index"
    | Per_package pkg ->
      sprintf "_sidebar/%s/index.odoc-index" (Package.Name.to_string pkg)
  in
  let action =
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets (Action_builder.path (Path.build index_file))
    >>> run_odoc
          sctx
          "sidebar-generate"
          ~quiet:false
          ~flags_for:None
          [ A "--json"; A "-o"; Target sidebar_json; A index_relative_path ]
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
        let scope = Per_package pkg in
        (* Discover artifacts to get all .odocl files *)
        let* all_artifacts, _lib_subdirs =
          discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_or_lib_name
        in
        (* Collect all .odocl files from all artifacts (libraries and package pages) *)
        let odocl_files =
          List.filter_map all_artifacts ~f:(fun artifact ->
            if Artifact.hidden artifact then None else Some (Artifact.odocl_file ctx artifact))
        in
        (* Generate index file with all .odocl files *)
        let* index_file = generate_index sctx ~scope ~packages:[ pkg ] ~odocl_files in
        (* Generate binary sidebar *)
        let* _sidebar_file = generate_sidebar_binary sctx ~scope ~index_file in
        Memo.return ())
    in
    Memo.return (Build_config.Gen_rules.make rules))
;;

(* Generate global sidebar for all packages - returns unit Memo.t for use in Rules.collect_unit *)
let generate_global_sidebar sctx =
  let ctx = Super_context.context sctx in
  (* Get all workspace packages *)
  let* workspace_pkgs = get_workspace_packages () in
  (* Filter out synthetic packages *)
  let real_pkgs =
    List.filter workspace_pkgs ~f:(fun pkg ->
      not (String.contains (Package.Name.to_string pkg) '@'))
  in
  (* Collect all .odocl files from all packages *)
  let* all_odocl_files =
    Memo.List.concat_map real_pkgs ~f:(fun pkg ->
      let pkg_name = Package.Name.to_string pkg in
      let* all_artifacts, _lib_subdirs =
        discover_package_artifacts sctx ctx ~pkg_or_lib_unique_name:pkg_name
      in
      Memo.return
        (List.filter_map all_artifacts ~f:(fun artifact ->
           if Artifact.hidden artifact
           then None
           else Some (Artifact.odocl_file ctx artifact))))
  in
  (* Generate global index file with all .odocl files *)
  let* index_file =
    generate_index sctx ~scope:Global ~packages:real_pkgs ~odocl_files:all_odocl_files
  in
  (* Generate global binary sidebar *)
  let* _sidebar_file = generate_sidebar_binary sctx ~scope:Global ~index_file in
  Memo.return ()
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
            Memo.return (List.filter_map all_artifacts ~f:Artifact.lib))
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
          (* Convert to targets using target_of_lib helper *)
          Memo.List.map all_deps ~f:(target_of_lib pkg_discovery)
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
      ~all_artifacts
      ~all_lib_names
      ~dir
      ~mode
      ()
  =
  (* Filter to only visible artifacts for HTML generation *)
  let visible_artifacts =
    List.filter all_artifacts ~f:(fun a -> not (Artifact.hidden a))
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
      (* Real package - determine sidebar scope based on env config and mode *)
      let pkg = Package.Name.of_string pkg_or_lib_name in
      let* flags = Flags.get_memo ~dir in
      let scope, should_generate_json =
        match mode, flags.sidebar with
        | Doc_mode.Local_only, Flags.Global ->
          (* Local_only with global sidebar - JSON already generated at _html root *)
          Global, false
        | Doc_mode.Local_only, Flags.Per_package
        | Doc_mode.Full, _ ->
          (* Per-package sidebar - generate JSON here *)
          Per_package pkg, true
      in
      let index_file = Paths.index_file ctx scope in
      let* () =
        if should_generate_json
        then generate_sidebar_json sctx ~mode ~scope ~index_file
        else Memo.return ()
      in
      Memo.return (Some (Paths.sidebar_file ctx scope)))
  in
  (* Create search_db for the entire package (all visible artifacts) *)
  let* search_db =
    let odocls =
      List.map visible_artifacts ~f:(fun artifact -> Artifact.odocl_file ctx artifact)
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
  let* () =
    Memo.parallel_iter Output_format.all ~f:(fun output ->
      (* Create package-level alias with all HTML files *)
      let all_paths =
        List.map visible_artifacts ~f:(fun artifact ->
          Path.build (Output_format.target ctx mode output artifact))
      in
      let pkg_alias = Dep.format_alias output mode ctx (Pkg pkg_name) in
      Dep.add_file_deps pkg_alias all_paths)
  in
  (* Also create library-level aliases for each library *)
  let visible_lib_artifacts =
    List.filter visible_artifacts ~f:(fun a ->
      match Artifact.kind a with
      | Module _ -> true
      | Page _ -> false)
  in
  (* Add each visible library artifact's HTML files to its library alias *)
  let* () =
    Memo.parallel_iter visible_lib_artifacts ~f:(fun artifact ->
      Memo.parallel_iter Output_format.all ~f:(fun output ->
        match Artifact.kind artifact with
        | Module (_, ((Lib (_, _) | Private_lib (_, _)) as target)) ->
          let lib_alias = Dep.format_alias output mode ctx target in
          let html_file = Path.build (Output_format.target ctx mode output artifact) in
          Dep.add_file_deps lib_alias [html_file]
        | Page _ -> Memo.return () (* Package artifacts don't have library aliases *)))
  in
  (* Create empty aliases for libraries with no visible artifacts *)
  let lib_names_with_artifacts =
    List.filter_map visible_lib_artifacts ~f:(fun a ->
      Option.map (Artifact.lib a) ~f:Lib.name)
    |> Lib_name.Set.of_list
  in
  Lib_name.Set.to_list all_lib_names
  |> Memo.parallel_iter ~f:(fun lib_name ->
    if Lib_name.Set.mem lib_names_with_artifacts lib_name
    then Memo.return ()
    else (
      (* Library with no artifacts - create empty aliases *)
      (* We need to look up the library to construct the target *)
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
        Memo.parallel_iter Output_format.all ~f:(fun output ->
          let lib_alias = Dep.format_alias output mode ctx (Lib (pkg_name, lib)) in
          Dep.add_file_deps lib_alias [])
      | None -> Memo.return ()))
;;

let handle_package_artifacts sctx ~dir ~path_prefix pkg_or_lib_name =
  let ctx = Super_context.context sctx in
  Log.info
    [ Pp.textf
        "odoc v3: handle_package_artifacts called for %s with path_prefix=%s"
        pkg_or_lib_name
        path_prefix
    ];
  (* Determine if this is a real package or a private library (has @ suffix).
     Private libraries have names like "foo@abc123" where @abc123 is a unique hash. *)
  let is_private_lib = String.contains pkg_or_lib_name '@' in
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
  let all_lib_names =
    List.map lib_subdirs ~f:Lib_name.of_string |> Lib_name.Set.of_list
  in
  let rules =
    match path_prefix with
    | "_odoc" ->
      Rules.collect_unit (fun () ->
        (* Build map from module name to odoc path once for all artifacts.
           module_name is already the obj_name (wrapped name like Top_closure__S),
           so odoc compile-deps output will match directly. *)
        let lib_artifacts_by_module =
          List.fold_left all_artifacts ~init:Module_name.Map.empty ~f:(fun acc artifact ->
            match Artifact.kind artifact with
            | Module ({ module_name; _ }, _) ->
              Module_name.Map.set acc module_name (Path.build (Artifact.odoc_file ctx artifact))
            | Page _ -> acc)
        in
        let* () =
          Memo.parallel_iter all_artifacts ~f:(fun artifact ->
            compile_artifact
              sctx
              ~artifact
              ~lib_artifacts_by_module
              ~package_lib_names:all_lib_names)
        in
        let* () =
          Memo.parallel_iter all_artifacts ~f:(fun artifact ->
            let odoc_file = Path.build (Artifact.odoc_file ctx artifact) in
            match Artifact.kind artifact with
            | Module (_, target) -> Dep.setup_deps ctx target (Path.Set.singleton odoc_file)
            | Page (_, target) -> Dep.setup_deps ctx target (Path.Set.singleton odoc_file))
        in
        let lib_names_with_artifacts =
          List.filter_map all_artifacts ~f:(fun a ->
            Option.map (Artifact.lib a) ~f:Lib.name)
          |> Lib_name.Set.of_list
        in
        let* lib_alias_dirs =
          Lib_name.Set.to_list all_lib_names
          |> Memo.List.filter_map ~f:(fun lib_name ->
            if Lib_name.Set.mem lib_names_with_artifacts lib_name
            then Memo.return (Some (lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name))
            else (
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              let alias = Dep.odoc_all_alias ~dir:lib_dir in
              let+ () = Dep.add_file_deps alias [] in
              Some lib_dir))
        in
        if is_private_lib
        then Memo.return ()
        else (
          let pkg_dir = Paths.root ctx ++ path_prefix ++ pkg_or_lib_name in
          let pkg_alias = Dep.odoc_all_alias ~dir:pkg_dir in
          Dep.add_odoc_all_deps pkg_alias ~dirs:lib_alias_dirs))
    | "_odocls" ->
      Rules.collect_unit (fun () ->
        let visible_artifacts =
          List.filter all_artifacts ~f:(fun a -> not (Artifact.hidden a))
        in
        let* () =
          Memo.parallel_iter visible_artifacts ~f:(fun artifact ->
            link_artifact sctx ~artifact)
        in
        let visible_lib_artifacts =
          List.filter visible_artifacts ~f:(fun a ->
            match Artifact.kind a with
            | Module _ -> true
            | Page _ -> false)
        in
        let* () =
          Memo.parallel_iter visible_lib_artifacts ~f:(fun artifact ->
            match Artifact.lib artifact with
            | Some lib ->
              let lib_name = Lib.name lib in
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              let lib_alias = Dep.odoc_all_alias ~dir:lib_dir in
              let odocl_file = Path.build (Artifact.odocl_file ctx artifact) in
              Dep.add_file_deps lib_alias [odocl_file]
            | None -> Memo.return ())
        in
        let lib_names_with_artifacts =
          List.filter_map visible_lib_artifacts ~f:(fun a ->
            Option.map (Artifact.lib a) ~f:Lib.name)
          |> Lib_name.Set.of_list
        in
        let* () =
          Lib_name.Set.to_list all_lib_names
          |> Memo.parallel_iter ~f:(fun lib_name ->
            if Lib_name.Set.mem lib_names_with_artifacts lib_name
            then Memo.return ()
            else (
              let lib_dir = lib_dir_path ctx ~path_prefix ~pkg_or_lib_name ~lib_name in
              let lib_alias = Dep.odoc_all_alias ~dir:lib_dir in
              Dep.add_file_deps lib_alias []))
        in
        (* Create package-level .odoc-all alias (skip private libraries) *)
        if is_private_lib
        then Memo.return ()
        else (
          let pkg_dir = Paths.odocl_root ctx ++ pkg_or_lib_name in
          let pkg_alias = Dep.odoc_all_alias ~dir:pkg_dir in
          let all_odocl_paths =
            List.map visible_artifacts ~f:(fun a -> Path.build (Artifact.odocl_file ctx a))
          in
          Dep.add_file_deps pkg_alias all_odocl_paths))
    | "_html" ->
      (* HTML generation for local packages (including private libraries) *)
      Rules.collect_unit (fun () ->
        generate_html_for_package
          sctx
          ~ctx
          ~pkg_or_lib_name
          ~all_artifacts
          ~all_lib_names
          ~dir
          ~mode:Doc_mode.Local_only
          ())
    | "_html_full" ->
      (* HTML generation for all packages in full mode (including private libraries) *)
      Rules.collect_unit (fun () ->
        generate_html_for_package
          sctx
          ~ctx
          ~pkg_or_lib_name
          ~all_artifacts
          ~all_lib_names
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

(* Generate library index.mld for an installed library *)
let generate_installed_lib_index sctx ctx ~pkg ~lib =
  let lib_name = Lib.name lib in
  (* Discover artifacts for this installed library *)
  let* artifacts = discover_installed_lib_artifacts sctx ctx ~pkg ~lib_name ~lib in
  (* If no artifacts (no modules), skip generating index *)
  if List.is_empty artifacts
  then Memo.return ()
  else (
    (* Generate library index from artifacts *)
    let index_mld_path = Paths.lib_index_mld ctx pkg lib_name in
    let content = library_index_content_from_artifacts ~lib_name ~artifacts in
    add_rule sctx (Action_builder.write_file index_mld_path content))
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
        let pkg_opt =
          match Lib.Local.of_lib lib with
          | Some _local_lib ->
            (* Local library - only expand if it has a real package *)
            Lib_info.package (Lib.info lib)
          | None ->
            (* For installed libraries, use Package_discovery.
               Don't trust Lib_info.package as it can be wrong (e.g., compiler-libs). *)
            Package_discovery.package_of_library pkg_discovery lib
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
           Memo.List.map all_expanded_libs ~f:(target_of_lib pkg_discovery)
         in
         let pkg_targets_from_libs =
           List.filter_map all_targets ~f:(fun target ->
             match target with
             | Lib (pkg, _) -> Some (Any_target (Pkg pkg))
             | Private_lib _ -> None (* Private libs have no package *))
         in
         let all_targets_wrapped = List.map all_targets ~f:(fun t -> Any_target t) in
         let all_targets_with_pkg = (Any_target (Pkg name) :: all_targets_wrapped) @ pkg_targets_from_libs @ [ Any_target Toplevel ] in
         (* Filter based on mode: Local_only includes only workspace packages *)
         let* filtered_targets =
           match mode with
           | Doc_mode.Local_only ->
             let* workspace_pkgs = get_workspace_packages () in
             let workspace_pkg_set = Package.Name.Set.of_list workspace_pkgs in
             Memo.return
               (List.filter all_targets_with_pkg ~f:(fun (Any_target target) ->
                  match target with
                  | Pkg p -> Package.Name.Set.mem workspace_pkg_set p
                  | Lib (p, lib) ->
                    Package.Name.Set.mem workspace_pkg_set p
                    && Option.is_some (Lib.Local.of_lib lib)
                  | Private_lib _ | Toplevel -> true (* Private libs and toplevel are always local *)))
           | Doc_mode.Full ->
             (* Include all dependencies *)
             Memo.return all_targets_with_pkg
         in
         let unique_targets = List.sort_uniq filtered_targets ~compare:compare_any_target in
         Memo.return
           (unique_targets
            |> List.map ~f:(fun (Any_target t) -> Dep.format_alias output mode ctx t)
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
    Memo.parallel_iter Output_format.all ~f:(fun output ->
      setup_package_aliases_format sctx pkg output mode))
;;

(* setup_package_odoc_rules removed - unused old function *)

let gen_project_rules sctx project =
  let* mask =
    let+ mask = Dune_load.mask () in
    Option.map ~f:Package.Name.Map.keys mask
  in
  (* Set up package aliases *)
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
             Memo.List.map all_dep_libs ~f:(target_of_lib pkg_discovery)
           in
           (* Filter to only local libraries (exclude installed/external dependencies) *)
           let filtered_targets =
             List.filter all_targets ~f:(fun target ->
               match target with
               | Lib (_, lib) | Private_lib (_, lib) ->
                 (* Keep only local libraries, filter out installed ones *)
                 Option.is_some (Lib.Local.of_lib lib))
           in
           (* Deduplicate targets *)
           let unique_targets =
             List.sort_uniq filtered_targets ~compare:(fun t1 t2 ->
               match t1, t2 with
               | Lib (_, l1), Lib (_, l2) ->
                 (* Compare libraries by their names *)
                 let name1 = Lib.name l1 in
                 let name2 = Lib.name l2 in
                 Lib_name.compare name1 name2
               | Private_lib (_, l1), Private_lib (_, l2) ->
                 Lib_name.compare (Lib.name l1) (Lib.name l2)
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
      let* () = add_rule sctx (Action_builder.write_file index_mld index_content) in
      (* Generate library index.mld files for installed libraries *)
      let* existing_mld_files =
        let mld_files = Package_discovery.mlds_of_package pkg_discovery pkg in
        Memo.return mld_files
      in
      Memo.List.iter truly_installed_libs ~f:(fun lib ->
        let lib_name = Lib.name lib in
        let lib_name_str = Lib_name.to_string lib_name in
        let lib_index_pattern = lib_name_str ^ "/index.mld" in
        (* Check if library already has an index.mld *)
        let has_index =
          List.exists existing_mld_files ~f:(fun mld_path ->
            let path_str = Path.to_string mld_path in
            String.is_suffix path_str ~suffix:lib_index_pattern)
        in
        if has_index
        then Memo.return ()
        else generate_installed_lib_index sctx ctx ~pkg ~lib))
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
          >>> setup_toplevel_index_html sctx Doc_mode.Local_only
          >>> Memo.parallel_iter Output_format.all ~f:(fun output ->
            let artifact = toplevel_index_artifact ctx packages in
            let html_file = Output_format.target ctx Doc_mode.Local_only output artifact in
            let alias = Dep.format_alias output Doc_mode.Local_only ctx Toplevel in
            Dep.add_file_deps alias [ Path.build html_file ])
          >>> (* Generate global sidebar JSON for Local_only mode if configured *)
          let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
          (match flags.sidebar with
           | Flags.Global ->
             let index_file = Paths.index_file ctx Global in
             generate_sidebar_json sctx ~mode:Doc_mode.Local_only ~scope:Global ~index_file
           | Flags.Per_package -> Memo.return ()))
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
    | [ "_mlds" ] ->
      (* Root _mlds directory - generate toplevel index.mld and allow package subdirs *)
      let* packages = Dune_load.packages () in
      let pkg_subdirs =
        Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
      in
      let rules = Rules.collect_unit (fun () -> setup_toplevel_index_mld sctx) in
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list pkg_subdirs))
           rules)
    | [ "_mlds"; pkg_name ] ->
      (* Package mlds directory - generate mld files and allow library subdirs *)
      let pkg = Package.Name.of_string pkg_name in
      let* packages = Dune_load.packages () in
      let* lib_subdirs =
        match Package.Name.Map.find packages pkg with
        | Some _ ->
          (* Local package - get library names *)
          let ctx = Super_context.context sctx in
          let* local_libs = Context.name ctx |> libs_of_pkg ~pkg in
          Memo.return
            (List.map local_libs ~f:(fun lib ->
               Lib.name (Lib.Local.to_lib lib) |> Lib_name.to_string))
        | None ->
          (* Installed package *)
          let ctx = Super_context.context sctx in
          let* pkg_discovery = Package_discovery.create ~context:ctx in
          let installed_libs = Package_discovery.libraries_of_package pkg_discovery pkg in
          Memo.return
            (List.filter_map installed_libs ~f:(fun lib ->
               match Lib.Local.of_lib lib with
               | Some _ -> None
               | None -> Some (Lib.name lib |> Lib_name.to_string)))
      in
      let rules = Rules.collect_unit (fun () -> handle_mlds_dir sctx ~pkg_name) in
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list lib_subdirs))
           rules)
    | [ "_mlds"; pkg_name; lib_name ] ->
      (* Library mld directory: _doc/_mlds/{package}/{library} *)
      (* Redirect to parent - the package level handler generates rules for all libraries *)
      Log.info
        [ Pp.textf
            "odoc v3: Library mlds directory for pkg=%s lib=%s - redirecting to parent"
            pkg_name
            lib_name
        ];
      Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
    | [ "_odoc" ] ->
      (* Root odoc directory - compile toplevel index and allow package subdirs *)
      let* packages = Dune_load.packages () in
      let pkg_subdirs =
        Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
      in
      let rules = Rules.collect_unit (fun () -> setup_toplevel_index_compile sctx) in
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list pkg_subdirs))
           rules)
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
      (* Root odocls directory - link toplevel index and allow package subdirs *)
      let* packages = Dune_load.packages () in
      let pkg_subdirs =
        Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
      in
      let rules = Rules.collect_unit (fun () -> setup_toplevel_index_link sctx) in
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list pkg_subdirs))
           rules)
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
          >>> setup_toplevel_index_html sctx Doc_mode.Full
          >>> Memo.parallel_iter Output_format.all ~f:(fun output ->
            let artifact = toplevel_index_artifact ctx packages in
            let html_file = Output_format.target ctx Doc_mode.Full output artifact in
            let alias = Dep.format_alias output Doc_mode.Full ctx Toplevel in
            Dep.add_file_deps alias [ Path.build html_file ]))
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
      (* Root sidebar directory - conditionally generate global sidebar and allow package subdirs *)
      let ctx = Super_context.context sctx in
      let* packages = Dune_load.packages () in
      let pkg_subdirs =
        Package.Name.Map.keys packages |> List.map ~f:Package.Name.to_string
      in
      let* flags = Flags.get_memo ~dir:(Context.build_dir ctx) in
      let rules =
        match flags.sidebar with
        | Flags.Global -> Rules.collect_unit (fun () -> generate_global_sidebar sctx)
        | Flags.Per_package -> Memo.return Rules.empty
      in
      Memo.return
        (Build_config.Gen_rules.make
           ~build_dir_only_sub_dirs:
             (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                ~dir
                (Subdir_set.of_list pkg_subdirs))
           rules)
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
