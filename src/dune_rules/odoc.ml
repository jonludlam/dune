open Import
open Dune_file
open Memo.O

let ( ++ ) = Path.Build.relative

let find_project_by_key =
  let memo =
    let make_map projects =
      Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
          (Dune_project.file_key project, project))
      |> Memo.return
    in
    let module Input = struct
      type t = Dune_project.t list

      let equal = List.equal Dune_project.equal

      let hash = List.hash Dune_project.hash

      let to_dyn = Dyn.list Dune_project.to_dyn
    end in
    Memo.create "project-by-keys" ~input:(module Input) make_map
  in
  fun key ->
    let* { projects; _ } = Dune_load.load () in
    let+ map = Memo.exec memo projects in
    Dune_project.File_key.Map.find_exn map key

module Scope_key : sig
  val of_string : Context.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t

  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      (Lib_name.parse_string_exn (Loc.none, s), public_libs)
    | Some (lib, key) ->
      let+ scope =
        let key = Dune_project.File_key.of_string key in
        find_project_by_key key >>= Scope.DB.find_by_project context
      in
      (Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope)

  let to_string lib project =
    let key = Dune_project.file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib)
      (Dune_project.File_key.to_string key)
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project

let pkg_or_lnu lib =
  match Lib_info.package (Lib.info lib) with
  | Some p -> Package.Name.to_string p
  | None -> lib_unique_name lib

type target =
  | Lib of Lib.Local.t
  | Pkg of Package.Name.t
  | ExtLib of string (* local path, to convert to some Path.* *)

type source =
  | Module of (Path.t * bool) (* is not hidden *)
  | Mld of Path.t

type odoc_artefact =
  { odoc_file : Path.Build.t
  ; odocl_file : Path.Build.t
  ; html_dir : Path.Build.t
  ; html_file : Path.Build.t
  ; source : source  (** source of the [odoc_file], either module or mld *)
  }

let add_rule sctx =
  let dir = (Super_context.context sctx).build_dir in
  Super_context.add_rule sctx ~dir

module Paths = struct
  let odoc_support_dirname = "_odoc_support"

  let root (context : Context.t) =
    Path.Build.relative context.Context.build_dir "_doc"

  let odocs ctx = function
    | Lib lib ->
      let obj_dir = Lib.Local.obj_dir lib in
      Obj_dir.odoc_dir obj_dir
    | Pkg pkg -> root ctx ++ sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)
    | ExtLib p -> root ctx ++ sprintf "_odoc/external/%s" p

  let html_root ctx = root ctx ++ "_html"

  let odocl_root ctx = root ctx ++ "_odocls" ++ "internal"

  let add_pkg_lnu base m =
    base
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib lib -> pkg_or_lnu (Lib.Local.to_lib lib)
    | ExtLib p -> p

  let html ctx m = add_pkg_lnu (html_root ctx) m

  let odocl ctx m = add_pkg_lnu (odocl_root ctx) m

  let odoc_support ctx = html_root ctx ++ odoc_support_dirname

  let toplevel_index ctx = html_root ctx ++ "index.html"

  let package_index_mld ctx pkg =
    let p = Package.Name.to_string pkg in
    root ctx ++ "_index_pages" ++ p ++ (p ^ ".mld")

  let package_index_mld_lnu ctx lnu =
    root ctx ++ "_index_private" ++ lnu ++ (lnu ^ ".mld")

  let ext_package_index_mld ctx pkg =
    let p = Package.Name.to_string pkg in
    root ctx ++ "_index_external" ++ p ++ (p ^ ".mld")

  let fallback_index_mld ctx localdir =
    let p = String.split_on_char ~sep:'/' localdir |> List.hd in
    root ctx ++ "_index_fallback" ++ p ++ (p ^ ".mld")

  let local_path_of_findlib_path ctx obj_dir =
    let obj_dir_str = Path.to_string obj_dir in
    let findlib_paths_unsorted =
      List.map ~f:(fun x -> Path.to_string x ^ "/") ctx.Context.findlib_paths
    in
    let findlib_paths =
      List.sort
        ~compare:(fun p1 p2 ->
          Int.compare (String.length p1) (String.length p2))
        findlib_paths_unsorted
    in
    List.find_map
      ~f:(fun prefix -> String.drop_prefix obj_dir_str ~prefix)
      findlib_paths
    |> Option.value_exn
end

module Dep : sig
  (** [html_alias ctx target] returns the alias that depends on all html targets
      produced by odoc for [target] *)
  val html_alias : Context.t -> target -> Alias.t

  (** [deps ctx pkg libraries] returns all odoc dependencies of [libraries]. If
      [libraries] are all part of a package [pkg], then the odoc dependencies of
      the package are also returned*)
  val deps :
       Context.t
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx target odocs] Adds [odocs] as dependencies for [target].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> target -> Path.Set.t -> unit Memo.t
end = struct
  let html_alias ctx m = Alias.doc ~dir:(Paths.html ctx m)

  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p ->
           Dep.Set.singleton (Dep.alias (alias ~dir:(Paths.odocs ctx (Pkg p))))
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
           match Lib.Local.of_lib lib with
           | None ->
             let obj_dir =
               Lib.info lib |> Lib_info.obj_dir |> Obj_dir.obj_dir
             in
             let local_path = Paths.local_path_of_findlib_path ctx obj_dir in
             let dir = String.split local_path ~on:'/' |> List.hd in
             let dir = Paths.odocs ctx (ExtLib dir) in
             let alias = alias ~dir in
             Dep.Set.add acc (Dep.alias alias)
           | Some lib ->
             let dir = Paths.odocs ctx (Lib lib) in
             let alias = alias ~dir in
             Dep.Set.add acc (Dep.alias alias)))

  let alias ctx m = alias ~dir:(Paths.odocs ctx m)

  let setup_deps ctx m files =
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
end

let odoc_ext = ".odoc"

module Mld : sig
  type ty =
    | PkgIndex of Package.Name.t
    | PrivateIndex of string
    | PkgPage of Package.Name.t
    | ExtIndex of Package.Name.t
    | FallbackIndex of string (* local dir *)

  type t

  val create : ty -> Path.Build.t -> t

  val odoc_file : Context.t -> t -> Path.Build.t

  val reference : t -> string

  val odoc_dir : Context.t -> t -> Path.Build.t

  val odoc_input : t -> Path.Build.t
end = struct
  type ty =
    | PkgIndex of Package.Name.t
    | PrivateIndex of string
    | PkgPage of Package.Name.t
    | ExtIndex of Package.Name.t
    | FallbackIndex of string

  type t = ty * Path.Build.t

  let create ty p = (ty, p)

  let odoc_dir ctx (ty, _) =
    match ty with
    | PkgPage pkg -> Paths.odocs ctx (Pkg pkg)
    | PkgIndex pkg -> Path.Build.parent_exn (Paths.package_index_mld ctx pkg)
    | PrivateIndex lnu ->
      Path.Build.parent_exn (Paths.package_index_mld_lnu ctx lnu)
    | ExtIndex pkg ->
      Path.Build.parent_exn (Paths.ext_package_index_mld ctx pkg)
    | FallbackIndex dir ->
      Path.Build.parent_exn (Paths.fallback_index_mld ctx dir)

  let reference (_, t) =
    let t = Filename.chop_extension (Path.Build.basename t) in
    sprintf "page-\"%s\"" t

  let odoc_file ctx t =
    let doc_dir = odoc_dir ctx t in
    let _, f = t in
    let r = Filename.chop_extension (Path.Build.basename f) in
    Path.Build.relative doc_dir (sprintf "page-%s%s" r odoc_ext)

  let odoc_input (_, t) = t
end

let odoc_base_flags sctx build_dir =
  let open Memo.O in
  let+ conf = Super_context.odoc sctx ~dir:build_dir in
  match conf.Env_node.Odoc.warnings with
  | Fatal -> Command.Args.A "--warn-error"
  | Nonfatal -> S []

let odoc_program sctx dir =
  Super_context.resolve_program sctx ~dir "odoc" ~loc:None
    ~hint:"opam install odoc"

let run_odoc sctx ~dir command ~flags_for args =
  let build_dir = (Super_context.context sctx).build_dir in
  let open Memo.O in
  let* program = odoc_program sctx build_dir in
  let+ base_flags =
    match flags_for with
    | None -> Memo.return Command.Args.empty
    | Some path -> odoc_base_flags sctx path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets deps
  >>> Command.run ~dir program [ A command; base_flags; S args ]

let module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  Action_builder.dyn_paths_unit
    (let open Action_builder.O in
    let+ deps =
      if Module.has m ~ml_kind:Intf then Dep_graph.deps_of dep_graphs.intf m
      else
        (* When a module has no .mli, use the dependencies for the .ml *)
        Dep_graph.deps_of dep_graphs.impl m
    in
    List.map deps ~f:(fun m -> Path.build (Obj_dir.Module.odoc obj_dir m)))

let external_module_deps_rule sctx artefact =
  match artefact.source with
  | Module (modpath, _) ->
    let ctx = Super_context.context sctx in
    let* odoc = odoc_program sctx (Paths.root ctx) in
    let deps_file = Path.Build.set_extension artefact.odoc_file ~ext:".deps" in
    Log.info
      [ Pp.textf "Adding rule for %s"
          (Path.Build.to_string_maybe_quoted deps_file)
      ];
    let* () =
      Super_context.add_rule sctx ~dir:(Paths.root ctx)
        (Command.run odoc
           ~dir:(Path.parent_exn (Path.build deps_file))
           ~stdout_to:deps_file
           [ A "compile-deps"; Path modpath ])
    in
    Memo.return (Some deps_file)
  | _ -> Memo.return None

let parse_odoc_deps lines =
  let rec getdeps cur = function
    | x :: rest -> (
      match String.split ~on:' ' x with
      | [ m; hash ] -> getdeps ((Module_name.of_string m, hash) :: cur) rest
      | _ -> getdeps cur rest)
    | [] -> cur
  in
  getdeps [] lines

let parent_args ctx parent_opt =
  match parent_opt with
  | None -> []
  | Some mld ->
    let dir = Mld.odoc_dir ctx mld in
    let reference = Mld.reference mld in
    let odoc_file =
      Mld.odoc_file ctx mld |> Path.build |> Dune_engine.Dep.file
      |> Dune_engine.Dep.Set.singleton
    in
    Command.Args.
      [ A "-I"
      ; Path (Path.build dir)
      ; A "--parent"
      ; A reference
      ; Hidden_deps odoc_file
      ]

let odoc_include_flags ctx pkg requires =
  Resolve.args
    (let open Resolve.O in
    let+ libs = requires in
    let paths =
      List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
          let dep =
            match Lib.Local.of_lib lib with
            | None ->
              let obj_dir =
                Lib.info lib |> Lib_info.obj_dir |> Obj_dir.obj_dir
              in
              let local_path = Paths.local_path_of_findlib_path ctx obj_dir in
              ExtLib local_path
            | Some lib -> Lib lib
          in
          Path.Set.add paths (Path.build (Paths.odocs ctx dep)))
    in
    let paths =
      match pkg with
      | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
      | None -> paths
    in
    Command.Args.S
      (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
           [ Command.Args.A "-I"; Path dir ])))

let compile_module sctx ~artefact ~requires ~package ~module_deps ~parent_opt =
  let odoc_file = artefact.odoc_file in
  let open Memo.O in
  let cmti =
    match artefact.source with
    | Module (f, _) -> f
    | Mld _ -> assert false
  in
  let ctx = Super_context.context sctx in
  let iflags = Command.Args.memo (odoc_include_flags ctx package requires) in
  let file_deps = Dep.deps ctx package requires in
  let parent_args = parent_args ctx parent_opt in
  let+ () =
    let* action_with_targets =
      let doc_dir = Path.parent_exn (Path.build artefact.odoc_file) in
      let+ run_odoc =
        run_odoc sctx ~dir:doc_dir "compile" ~flags_for:(Some odoc_file)
          ([ Command.Args.A "-I"
           ; Path doc_dir
           ; iflags
           ; A "-o"
           ; Target odoc_file
           ; Dep cmti
           ]
          @ parent_args)
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets module_deps
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  odoc_file

type mld_child =
  | Module of Module_name.t
  | Page of string

let compile_mld sctx (m : Mld.t) ~doc_dir ~parent_opt ~children =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let odoc_file = Mld.odoc_file ctx m in
  Log.info
    [ Pp.textf "compile_mld: output_file: %s" (Path.Build.to_string odoc_file) ];
  let odoc_input = Mld.odoc_input m in
  let parent_args = parent_args ctx parent_opt in
  let child_args =
    List.fold_left children ~init:[] ~f:(fun args child ->
        match child with
        | Module mname ->
          "--child" :: ("module-" ^ Module_name.to_string mname) :: args
        | Page pname -> "--child" :: sprintf "page-\"%s\"" pname :: args)
  in
  let* run_odoc =
    run_odoc sctx ~dir:(Path.build doc_dir) "compile"
      ~flags_for:(Some odoc_input)
      (A "-o" :: Target odoc_file
      :: Dep (Path.build odoc_input)
      :: As child_args :: parent_args)
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file

let link_odoc_rules sctx (artefact : odoc_artefact) ~package ~requires =
  let ctx = Super_context.context sctx in
  let deps = Dep.deps ctx package requires in
  Log.info
    [ Pp.textf "NFT: Link rules for %s"
        (Path.Build.to_string artefact.odocl_file)
    ];
  let open Memo.O in
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.parent_exn (Path.build artefact.odocl_file))
      "link" ~flags_for:(Some artefact.odoc_file)
      [ odoc_include_flags ctx package requires
      ; A "-o"
      ; Target artefact.odocl_file
      ; A "-I"
      ; A "."
      ; Dep (Path.build artefact.odoc_file)
      ]
  in
  add_rule sctx
    (let open Action_builder.With_targets.O in
    Action_builder.with_no_targets deps >>> run_odoc)

let pkg_or_lnu_parent ctx lib =
  match Lib_info.package (Lib.info lib) with
  | Some p -> Mld.create (PkgIndex p) (Paths.package_index_mld ctx p)
  | None ->
    let lnu = lib_unique_name lib in
    Mld.create (PrivateIndex lnu) (Paths.package_index_mld_lnu ctx lnu)

let create_odoc ctx ~target ~source ~odocl_base ~is_index odoc_file =
  let html_base = Paths.html ctx target in
  let basename = Path.Build.basename odoc_file |> Filename.chop_extension in
  let odocl_file = odocl_base ++ (basename ^ ".odocl") in
  match target with
  | Lib _ | ExtLib _ ->
    let html_dir = html_base ++ Stdune.String.capitalize basename in
    { odoc_file
    ; odocl_file
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; source
    }
  | Pkg _ ->
    let page_name =
      basename |> String.drop_prefix ~prefix:"page-" |> Option.value_exn
    in
    let html_file =
      if is_index then html_base ++ "index.html"
      else html_base ++ sprintf "%s.html" page_name
    in
    { odoc_file; odocl_file; html_dir = html_base; html_file; source }

let setup_library_odoc_rules cctx (local_lib : Lib.Local.t) =
  let open Memo.O in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let parent = pkg_or_lnu_parent ctx (Lib.Local.to_lib local_lib) in
  let* requires = Compilation_context.requires_compile cctx in
  let info = Lib.Local.info local_lib in
  let package = Lib_info.package info in

  let obj_dir = Compilation_context.obj_dir cctx in
  let modules = Compilation_context.modules cctx in
  let entry_modules = Modules.entry_modules modules in
  let modules_and_odoc_files =
    Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
        let visible = List.mem entry_modules m ~equal:(fun m1 m2 ->
          Module_name.equal (Module.name m1) (Module.name m2))
        in
        let module_deps =
          module_deps m ~obj_dir
            ~dep_graphs:(Compilation_context.dep_graphs cctx)
        in
        let target = Lib local_lib in
        let odocl_base = Paths.odocl ctx target in
        let odoc_file = Obj_dir.Module.odoc obj_dir m in
        let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind:(Ocaml Cmi) m in
        let artefact =
          create_odoc ctx ~target ~odocl_base ~is_index:false odoc_file
            ~source:(Module (Path.build cmti_file, visible))
        in
        let parent_opt =
          if visible
          then Some parent
          else None
        in
        let compiled =
          compile_module sctx ~artefact ~requires ~package ~module_deps
            ~parent_opt
        in
        compiled :: acc)
  in
  let* modules_and_odoc_files = Memo.all_concurrently modules_and_odoc_files in
  Dep.setup_deps ctx (Lib local_lib)
    (Path.Set.of_list_map modules_and_odoc_files ~f:(fun p -> Path.build p))

let setup_html sctx (odoc_file : odoc_artefact) =
  let ctx = Super_context.context sctx in
  let to_remove, dummy =
    match odoc_file.source with
    | Mld _ -> (odoc_file.html_file, [])
    | Module _ ->
      (* Dummy target so that the below rule as at least one target. We do this
         because we don't know the targets of odoc in this case. The proper way
         to support this would be to have directory targets. *)
      let dummy = Action_builder.create_file (odoc_file.html_dir ++ ".dummy") in
      (odoc_file.html_dir, [ dummy ])
  in
  let open Memo.O in
  let odoc_support_path = Paths.odoc_support ctx in
  let html_output = Paths.html_root ctx in
  let support_relative =
    Path.reach (Path.build odoc_support_path) ~from:(Path.build html_output)
  in
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "html-generate" ~flags_for:None
      [ A "-o"
      ; Path (Path.build (Paths.html_root ctx))
      ; A "--support-uri"
      ; A support_relative
      ; A "--theme-uri"
      ; A support_relative
      ; Dep (Path.build odoc_file.odocl_file)
      ; Hidden_targets [ odoc_file.html_file ]
      ]
  in
  add_rule sctx
    (Action_builder.progn
       (Action_builder.with_no_targets
          (Action_builder.return
             (Action.Full.make
                (Action.Progn
                   [ Action.Remove_tree to_remove
                   ; Action.Mkdir odoc_file.html_dir
                   ])))
       :: run_odoc :: dummy))

let setup_css_rule sctx =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx in
  let* run_odoc =
    let+ cmd =
      run_odoc sctx ~dir:(Path.build ctx.build_dir) "support-files"
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    cmd
    |> Action_builder.With_targets.add_directories ~directory_targets:[ dir ]
  in
  add_rule sctx run_odoc

let sp = Printf.sprintf

let setup_toplevel_index_rule sctx =
  let* list_items =
    let+ packages = Only_packages.get () in
    Package.Name.Map.to_list packages
    |> List.filter_map ~f:(fun (name, pkg) ->
           let name = Package.Name.to_string name in
           let link = sp {|<a href="%s/index.html">%s</a>|} name name in
           let version_suffix =
             match pkg.Package.version with
             | None -> ""
             | Some v -> sp {| <span class="version">%s</span>|} v
           in
           Some (sp "<li>%s%s</li>" link version_suffix))
    |> String.concat ~sep:"\n      "
  in
  let html =
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
      Paths.odoc_support_dirname list_items
  in
  let ctx = Super_context.context sctx in
  add_rule sctx (Action_builder.write_file (Paths.toplevel_index ctx) html)

let libs_of_pkg ctx ~pkg =
  let+ entries = Scope.DB.lib_entries_of_package ctx pkg in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map entries ~f:(fun (entry : Scope.DB.Lib_entry.t) ->
      match entry with
      | Library lib ->
        let is_impl =
          Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements
          |> Option.is_some
        in
        Option.some_if (not is_impl) lib
      | Deprecated_library_name _ -> None)

let entry_modules_by_lib sctx lib =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let name = Lib.name (Lib.Local.to_lib lib) in
  Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
  >>| Ml_sources.modules ~for_:(Library name)
  >>| Modules.entry_modules

let entry_modules sctx ~pkg =
  let* l =
    libs_of_pkg (Super_context.context sctx) ~pkg
    >>| List.filter ~f:(fun lib ->
            Lib.Local.info lib |> Lib_info.status |> Lib_info.Status.is_private
            |> not)
  in
  let+ l =
    Memo.parallel_map l ~f:(fun l ->
        let+ m = entry_modules_by_lib sctx l in
        (l, m))
  in
  Lib.Local.Map.of_list_exn l

let static_html ctx =
  let open Paths in
  [ odoc_support ctx; toplevel_index ctx ]

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld ->
        (Filename.chop_extension (Path.Build.basename mld), mld))
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted (Path.build p1))
          (Path.to_string_maybe_quoted (Path.build p2))
      ]

let odoc_artefacts sctx target =
  let ctx = Super_context.context sctx in
  match target with
  | Pkg pkg ->
    let+ mlds =
      let+ mlds = Packages.mlds sctx pkg in
      check_mlds_no_dupes ~pkg ~mlds
    in
    let odocl_base = Paths.odocl ctx target in
    String.Map.values mlds
    |> List.map ~f:(fun mld ->
           Mld.create (PkgPage pkg) mld
           |> Mld.odoc_file ctx
           |> create_odoc ctx ~odocl_base ~is_index:false ~target
                ~source:(Mld (Path.build mld)))
  | Lib lib ->
    let info = Lib.Local.info lib in
    let obj_dir = Lib_info.obj_dir info in
    let+ modules = entry_modules_by_lib sctx lib in
    List.map
      ~f:(fun m ->
        let odocl_base = Paths.odocl ctx target in
        let odoc_file = Obj_dir.Module.odoc obj_dir m in
        let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind:(Ocaml Cmi) m in
        create_odoc ctx ~target ~odocl_base ~is_index:false odoc_file
          ~source:(Module (Path.build cmti_file, true)))
      modules
  | _ -> Memo.return []

let setup_lib_odocl_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t * Lib.t list Resolve.t

    let equal (sc1, l1, r1) (sc2, l2, r2) =
      Super_context.equal sc1 sc2
      && Lib.Local.equal l1 l2
      && Resolve.equal (List.equal Lib.equal) r1 r2

    let hash (sc, l, r) =
      Poly.hash
        ( Super_context.hash sc
        , Lib.Local.hash l
        , Resolve.hash (List.hash Lib.hash) r )

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, lib, requires) =
    let* odocs = odoc_artefacts sctx (Lib lib) in
    let package = Lib_info.package (Lib.Local.info lib) in
    Memo.parallel_iter odocs ~f:(fun odoc ->
        link_odoc_rules sctx ~package ~requires odoc)
  in
  Memo.With_implicit_output.create "setup_library_odocls_rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f

let setup_lib_odocl_rules sctx lib ~requires =
  Memo.With_implicit_output.exec setup_lib_odocl_rules_def (sctx, lib, requires)

let setup_pkg_rules_def memo_name f =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Package.Name.t * Lib.Local.t list

    let equal (s1, p1, l1) (s2, p2, l2) =
      Package.Name.equal p1 p2
      && List.equal Lib.Local.equal l1 l2
      && Super_context.equal s1 s2

    let hash (sctx, p, ls) =
      Poly.hash
        ( Super_context.hash sctx
        , Package.Name.hash p
        , List.hash Lib.Local.hash ls )

    let to_dyn (_, package, libs) =
      let open Dyn in
      Tuple
        [ Package.Name.to_dyn package
        ; List (List.map ~f:Lib.Local.to_dyn libs)
        ]
  end in
  Memo.With_implicit_output.create memo_name
    ~input:(module Input)
    ~implicit_output:Rules.implicit_output f

let setup_pkg_odocl_rules_def =
  let f (sctx, pkg, (libs : Lib.Local.t list)) =
    let* requires =
      let libs = (libs :> Lib.t list) in
      Lib.closure libs ~linking:false
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_odocl_rules sctx ~requires)
    and* _ =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let package = Some pkg in
      let+ () =
        Memo.parallel_iter pkg_odocs ~f:(fun odoc ->
            link_odoc_rules sctx ~package ~requires odoc)
      in
      pkg_odocs
    and* _ =
      Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
    in
    Memo.return ()
  in
  setup_pkg_rules_def "setup-package-odocls-rules" f

let setup_pkg_odocl_rules sctx ~pkg ~libs : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_odocl_rules_def (sctx, pkg, libs)

let setup_lib_html_rules_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Lib.Local.t

    let equal (sc1, l1) (sc2, l2) =
      Super_context.equal sc1 sc2 && Lib.Local.equal l1 l2

    let hash (sc, l) = Poly.hash (Super_context.hash sc, Lib.Local.hash l)

    let to_dyn _ = Dyn.Opaque
  end in
  let f (sctx, lib) =
    let ctx = Super_context.context sctx in
    let* odocs = odoc_artefacts sctx (Lib lib) in
    let* () = Memo.parallel_iter odocs ~f:(fun odoc -> setup_html sctx odoc) in
    let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
    let static_html = List.map ~f:Path.build (static_html ctx) in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias ctx (Lib lib))
      (Action_builder.paths (List.rev_append static_html html_files))
  in
  Memo.With_implicit_output.create "setup-library-html-rules"
    ~implicit_output:Rules.implicit_output
    ~input:(module Input)
    f

let setup_lib_html_rules sctx lib =
  Memo.With_implicit_output.exec setup_lib_html_rules_def (sctx, lib)

let setup_pkg_html_rules_def =
  let f (sctx, pkg, (libs : Lib.Local.t list)) =
    let ctx = Super_context.context sctx in
    let index =
      let mld_path = Paths.package_index_mld ctx pkg in
      let index = Mld.create (PkgIndex pkg) mld_path in
      let odocl_base = Path.Build.parent_exn mld_path in
      Mld.odoc_file ctx index
      |> create_odoc ctx ~target:(Pkg pkg) ~is_index:true ~odocl_base
           ~source:(Mld (Path.build mld_path))
    in
    let* () = Memo.parallel_iter libs ~f:(setup_lib_html_rules sctx)
    and* pkg_odocs =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let+ () =
        Memo.parallel_iter (index :: pkg_odocs) ~f:(fun o -> setup_html sctx o)
      in
      index :: pkg_odocs
    and* lib_odocs =
      Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
    in
    let odocs = List.concat (pkg_odocs :: lib_odocs) in
    let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
    let static_html = List.map ~f:Path.build (static_html ctx) in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias ctx (Pkg pkg))
      (Action_builder.paths (List.rev_append static_html html_files))
  in
  setup_pkg_rules_def "setup-package-html-rules" f

let setup_pkg_html_rules sctx ~pkg ~libs : unit Memo.t =
  Memo.With_implicit_output.exec setup_pkg_html_rules_def (sctx, pkg, libs)

let setup_package_aliases sctx (pkg : Package.t) =
  let ctx = Super_context.context sctx in
  let name = Package.name pkg in
  let alias =
    let pkg_dir = Package.dir pkg in
    let dir = Path.Build.append_source ctx.build_dir pkg_dir in
    Alias.doc ~dir
  in
  let* libs =
    libs_of_pkg ctx ~pkg:name
    >>| List.map ~f:(fun lib -> Dep.html_alias ctx (Lib lib))
  in
  Dep.html_alias ctx (Pkg name) :: libs
  |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)
  |> Action_builder.deps
  |> Rules.Produce.Alias.add_deps alias

let default_index ~pkg entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
  entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
  |> List.iter ~f:(fun (lib, modules) ->
         Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib);
         Buffer.add_string b
           (match modules with
           | [ x ] ->
             sprintf
               "The entry point of this library is the module:\n{!module-%s}.\n"
               (Module_name.to_string x)
           | _ ->
             sprintf
               "This library exposes the following toplevel modules:\n\
                {!modules:%s}\n"
               (modules
               |> List.sort ~compare:(fun x y -> Module_name.compare x y)
               |> List.map ~f:Module_name.to_string
               |> String.concat ~sep:" ")));
  Buffer.contents b

let default_fallback_index local_path entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 Index for filesystem path %s}\n" local_path;
  Printf.bprintf b "{!modules:%s}\n"
    (entry_modules |> List.map ~f:fst
    |> List.sort ~compare:(fun x y -> Module_name.compare x y)
    |> List.map ~f:Module_name.to_string
    |> String.concat ~sep:" ");
  Buffer.contents b

let default_private_index l entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 %s index}\n"
    (Lib.Local.info l |> Lib_info.name |> Lib_name.to_string);
  Buffer.add_string b
    (match entry_modules with
    | [ x ] ->
      sprintf "The entry point of this library is the module:\n{!module-%s}.\n"
        (Module_name.to_string (Module.name x))
    | _ ->
      sprintf
        "This library exposes the following toplevel modules:\n{!modules:%s}\n"
        (entry_modules
        |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
        |> List.sort ~compare:(fun x y ->
               Module_name.compare (Module.name x) (Module.name y))
        |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
        |> String.concat ~sep:" "));
  Buffer.contents b

let package_mlds =
  let memo =
    Memo.create "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package)
      (fun (sctx, pkg) ->
        (* CR-someday jeremiedimino: it is weird that we drop the
           [Package.t] and go back to a package name here. Need to try and
           change that one day. *)
        let pkg = Package.name pkg in
        let* mlds = Packages.mlds sctx pkg in
        let mlds = check_mlds_no_dupes ~pkg ~mlds in
        Memo.return (String.Map.remove mlds "index"))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)

let setup_package_odoc_rules sctx ~pkg =
  let* mlds = package_mlds sctx ~pkg in
  let ctx = Super_context.context sctx in
  (* CR-someday jeremiedimino: it is weird that we drop the [Package.t] and go
     back to a package name here. Need to try and change that one day. *)
  let pkg = Package.name pkg in
  let index = Mld.create (PkgIndex pkg) (Paths.package_index_mld ctx pkg) in
  let* odocs =
    Memo.parallel_map (String.Map.values mlds) ~f:(fun mld ->
        compile_mld sctx
          (Mld.create (PkgPage pkg) mld)
          ~parent_opt:(Some index)
          ~doc_dir:(Paths.odocs ctx (Pkg pkg))
          ~children:[])
  in
  Dep.setup_deps ctx (Pkg pkg) (Path.set_of_build_paths_list odocs)

let gen_project_rules sctx project =
  let* packages = Only_packages.packages_of_project project in
  Package.Name.Map_traversals.parallel_iter packages
    ~f:(fun _ (pkg : Package.t) ->
      (* setup @doc to build the correct html for the package *)
      setup_package_aliases sctx pkg)

let setup_private_library_doc_alias sctx ~scope ~dir (l : Dune_file.Library.t) =
  match l.visibility with
  | Public _ -> Memo.return ()
  | Private _ ->
    let ctx = Super_context.context sctx in
    let* lib =
      Lib.DB.find_even_when_hidden (Scope.libs scope) (Library.best_name l)
      >>| Option.value_exn
    in
    let lib = Lib (Lib.Local.of_lib_exn lib) in
    Rules.Produce.Alias.add_deps (Alias.private_doc ~dir)
      (lib |> Dep.html_alias ctx |> Dune_engine.Dep.alias |> Action_builder.dep)

let setup_lnu_index_rules sctx lnu =
  let ctx = Super_context.context sctx in
  let index_path = Paths.package_index_mld_lnu ctx lnu in
  let* lib, lib_db = Scope_key.of_string ctx lnu in
  let* lib =
    let+ lib = Lib.DB.find lib_db lib in
    Option.bind ~f:Lib.Local.of_lib lib
  in
  match lib with
  | None -> Memo.return ()
  | Some l ->
    let* entry_modules = entry_modules_by_lib sctx l in
    let* () =
      add_rule sctx
        (Action_builder.write_file index_path
           (default_private_index l entry_modules))
    in
    let children =
      List.fold_left ~init:[] entry_modules ~f:(fun children m ->
          Module (Module.name m) :: children)
    in
    let* _ =
      compile_mld sctx
        (Mld.create (PrivateIndex lnu) index_path)
        ~doc_dir:(Path.Build.parent_exn index_path)
        ~parent_opt:None ~children
    in
    Memo.return ()

let setup_pkg_index_rules sctx pkg =
  let pkg = Package.name pkg in
  let* mlds = Packages.mlds sctx pkg in
  let mlds = check_mlds_no_dupes ~pkg ~mlds in
  let ctx = Super_context.context sctx in
  let index_path = Paths.package_index_mld ctx pkg in
  let* entry_modules = entry_modules sctx ~pkg in
  let entry_modules =
    Lib.Local.Map.foldi ~init:[] entry_modules ~f:(fun lib modules acc ->
        let info = Lib.Local.info lib in
        let modules =
          modules
          |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
          |> List.map ~f:Module.name
        in
        (Lib_info.name info, modules) :: acc)
  in
  let mld = Mld.create (PkgIndex pkg) index_path in

  (* Rule to create index pages - either symlinked from index.mld in a pacage or created by us. *)
  let* () =
    match String.Map.find mlds "index" with
    | Some mld ->
      add_rule sctx
        (Action_builder.symlink ~src:(Path.build mld) ~dst:index_path)
    | None ->
      add_rule sctx
        (Action_builder.write_file index_path
           (default_index ~pkg entry_modules))
  in

  let* _ =
    let pchildren =
      Import.String.Map.foldi ~init:[] mlds ~f:(fun name _ children ->
          Page name :: children)
    in
    let children =
      List.fold_left ~init:pchildren entry_modules
        ~f:(fun children (_lib, ms) ->
          let lchildren = List.map ~f:(fun m -> Module m) ms in
          lchildren @ children)
    in
    let children = Page "__dummy__" :: children in
    compile_mld sctx mld
      ~doc_dir:(Path.Build.parent_exn index_path)
      ~parent_opt:None ~children
  in

  let* _ =
    let* libs = libs_of_pkg ctx ~pkg in
    let* requires = Lib.closure (libs :> Lib.t list) ~linking:true in
    let index =
      let odocl_base =
        Paths.package_index_mld ctx pkg |> Path.Build.parent_exn
      in
      Mld.odoc_file ctx mld
      |> create_odoc ctx ~target:(Pkg pkg) ~is_index:true ~odocl_base
           ~source:(Mld (Path.build index_path))
    in
    link_odoc_rules sctx index ~package:(Some pkg) ~requires
  in

  Memo.return ()

(* Ugh, fix the recursive stuff *)
let rec modules_of_dir ~recursive d =
  let* dir_res = Fs_memo.dir_contents (Path.as_outside_build_dir_exn d) in
  match dir_res with
  | Error _ -> Memo.return []
  | Ok dc ->
    let list = Fs_cache.Dir_contents.to_list dc in
    let extensions = [ (".cmti", `Cmti); (".cmt", `Cmt); (".cmi", `Cmi) ] in
    let modules =
      List.filter_map
        ~f:(fun (x, ty) ->
          match (ty, List.assoc extensions (Filename.extension x)) with
          | Unix.S_REG, Some _ -> Some (Filename.chop_extension x)
          | _, _ -> None)
        list
      |> List.sort_uniq ~compare:String.compare
    in
    let* others =
      if recursive then
        Memo.List.map
          ~f:(fun (x, ty) ->
            match ty with
            | Unix.S_DIR ->
              let+ sub = modules_of_dir ~recursive:true (Path.relative d x) in
              List.map
                ~f:(fun (m, (reldir, path, ty)) ->
                  (m, (x ^ "/" ^ reldir, path, ty)))
                sub
            | _ -> Memo.return [])
          list
      else Memo.return []
    in
    let others = List.flatten others in
    let ms =
      List.map
        ~f:(fun m ->
          let ext, ty =
            List.find_exn
              ~f:(fun (ext, _ty) ->
                List.exists list ~f:(fun (n, _) -> n = m ^ ext))
              extensions
          in
          (Module_name.of_string m, ("", Path.relative d (m ^ ext), ty)))
        modules
    in
    Memo.return (ms @ others)

let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2 then false
    else if s.[i] = '_' && s.[i + 1] = '_' then true
    else aux (i + 1)
  in
  aux 0

(* type local_dir_libs =
   | Dune_style of (Lib_name.t * Dune_package.Lib.t) String.Map.t
   | Fallback of (Dune_package.Lib.t Lib_name.Map.t) String.Map.t *)

let libs_of_local_dir (ctx : Context.t) =
  let* findlib =
    Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
  in
  let* all_packages = Findlib.all_packages findlib in

  let map =
    List.fold_left all_packages ~init:String.Map.empty ~f:(fun map entry ->
        match entry with
        | Dune_package.Entry.Library l ->
          let obj_dir =
            Dune_package.Lib.info l |> Lib_info.obj_dir |> Obj_dir.dir
          in
          let local = Paths.local_path_of_findlib_path ctx obj_dir in
          let toplocal = String.split local ~on:'/' |> List.hd in
          let name = Dune_package.Lib.info l |> Lib_info.name in
          let update_fn = function
            | Some libs ->
              Some
                (String.Map.update libs local ~f:(function
                  | Some libs -> Some (Lib_name.Map.add_exn libs name l)
                  | None -> Some (Lib_name.Map.singleton name l)))
            | None ->
              Some (String.Map.singleton local (Lib_name.Map.singleton name l))
          in
          String.Map.update map toplocal ~f:update_fn
        | _ -> map)
  in
  Memo.return map

let setup_fallback_index_rules sctx dir =
  let ctx = Super_context.context sctx in
  let index_path = Paths.fallback_index_mld ctx dir in
  let mld = Mld.create (FallbackIndex dir) index_path in
  let* mods =
    Memo.List.map ctx.findlib_paths ~f:(fun p ->
        modules_of_dir ~recursive:true (Path.relative p dir))
  in
  let mods = List.flatten mods in
  let index_path = Paths.fallback_index_mld ctx dir in
  let entry_mods =
    List.filter
      ~f:(fun (m, _) ->
        Module_name.to_string m |> contains_double_underscore |> not)
      mods
  in
  let children =
    List.fold_left ~init:[] entry_mods ~f:(fun children (m, _) ->
        Module m :: children)
  in
  let* () =
    let action =
      let default_index = default_fallback_index dir entry_mods in
      Action_builder.write_file index_path default_index
    in
    add_rule sctx action
  in

  let* _ =
    compile_mld sctx mld
      ~doc_dir:(Path.Build.parent_exn index_path)
      ~parent_opt:None ~children
  in

  let* _ =
    let index =
      let odocl_base =
        Paths.fallback_index_mld ctx dir |> Path.Build.parent_exn
      in
      Mld.odoc_file ctx mld
      |> create_odoc ctx
           ~target:(Pkg (Package.Name.of_string "dummy"))
           ~is_index:true ~odocl_base
           ~source:(Mld (Path.build index_path))
    in
    link_odoc_rules sctx index ~package:None ~requires:(Resolve.return [])
  in

  Memo.return ()

let setup_external_index_rules sctx pkg =
  let ctx = Super_context.context sctx in
  let* findlib =
    Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
  in
  let* pkg_opt = Findlib.find_root_package findlib pkg in
  match pkg_opt with
  | Error _ -> Memo.return ()
  | Ok dpkg ->
    let index_path = Paths.ext_package_index_mld ctx pkg in
    let mld = Mld.create (ExtIndex pkg) index_path in
    let entry_modules =
      Lib_name.Map.fold dpkg.entries ~init:[] ~f:(fun entry acc ->
          match entry with
          | Dune_package.Entry.Library l -> (
            let info = Dune_package.Lib.info l in
            match Lib_info.entry_modules info with
            | External (Ok entry_modules) ->
              (Lib_info.name info, entry_modules) :: acc
            | _ -> acc)
          | _ -> acc)
    in
    let installed = dpkg.files in

    let mlds =
      List.filter_map installed ~f:(function
        | Dune_section.Doc, fs ->
          let doc_path = Section.Map.find_exn dpkg.sections Doc in
          Log.info [ Pp.textf "doc_path: %s" (Path.to_string doc_path) ];
          Some
            (List.filter_map
               ~f:(fun dst ->
                 let str = Install.Dst.to_string dst in
                 if Filename.check_suffix str ".mld" then
                   Some (Path.relative doc_path str)
                 else None)
               fs)
        | _ -> None)
      |> List.concat
    in

    let* () =
      let action =
        match List.find ~f:(fun p -> Path.basename p = "index.mld") mlds with
        | Some mld -> Action_builder.symlink ~src:mld ~dst:index_path
        | None ->
          let default_index = default_index ~pkg entry_modules in
          Action_builder.write_file index_path default_index
      in
      add_rule sctx action
    in

    let* _ =
      let pchildren =
        List.filter_map mlds ~f:(fun path ->
            let name = Path.basename path |> Filename.chop_extension in
            if name = "index" then None else Some (Page name))
      in

      let children =
        List.fold_left ~init:pchildren entry_modules ~f:(fun children (_, ms) ->
            let lchildren = List.map ~f:(fun m -> Module m) ms in
            lchildren @ children)
      in

      let children = Page "__dummy__" :: children in

      compile_mld sctx mld
        ~doc_dir:(Path.Build.parent_exn index_path)
        ~parent_opt:None ~children
    in

    let* _ =
      let* libs = libs_of_pkg ctx ~pkg in
      let* requires = Lib.closure (libs :> Lib.t list) ~linking:true in
      let index =
        let odocl_base =
          Paths.ext_package_index_mld ctx pkg |> Path.Build.parent_exn
        in
        Mld.odoc_file ctx mld
        |> create_odoc ctx ~target:(Pkg pkg) ~is_index:true ~odocl_base
             ~source:(Mld (Path.build index_path))
      in
      link_odoc_rules sctx index ~package:(Some pkg) ~requires
    in

    Memo.return ()

let external_odoc_artefact sctx local_path (m, cmti_file, visible) =
  let ctx = Super_context.context sctx in
  let output_dir = Paths.odocs ctx (ExtLib local_path) in
  let odoc_file_base = Module_name.to_string m in
  let odoc_file = output_dir ++ sprintf "%s.odoc" odoc_file_base in
  let artefact =
    create_odoc ctx ~target:(ExtLib local_path) ~source:(Module (cmti_file, visible))
      ~odocl_base:output_dir ~is_index:false odoc_file
  in
  artefact

let compile_external_odoc artefact sctx lib_module_names parent requires =
  let* deps_file = external_module_deps_rule sctx artefact in
  match deps_file with
  | None -> Memo.return ()
  | Some deps_file ->
    Log.info
      [ Pp.textf "Rule for: %s" (Path.Build.to_string artefact.odoc_file) ];
    let module_deps =
      let open Action_builder.O in
      let* l = Action_builder.lines_of (Path.build deps_file) in
      let deps = parse_odoc_deps l in
      let deps' =
        List.filter_map
          ~f:(fun (m', _) ->
            let muname =
              Module_name.Unique.of_name_assuming_needs_no_mangling m'
            in
            if
              Path.Build.basename artefact.odoc_file
              = Module_name.to_string m' ^ ".odoc"
            then None
            else
              match List.assoc lib_module_names muname with
              | None -> None
              | Some p ->
                Some (p ++ (Module_name.to_string m' ^ ".odoc") |> Path.build))
          deps
      in
      Log.info
        [ Pp.textf "NNN Got %d deps for odoc file %s (%s) (of %d)"
            (List.length deps')
            (Path.Build.to_string artefact.odoc_file)
            (String.concat ~sep:"," (List.map ~f:Path.to_string deps'))
            (List.length deps)
        ];
      Dune_engine.Dep.Set.of_files deps' |> Action_builder.deps
    in
    let* odoc_file =
      compile_module sctx ~artefact ~requires ~module_deps ~parent_opt:parent
        ~package:None
    in
    Log.info
      [ Pp.textf "About to add rule for %s" (Path.Build.to_string odoc_file) ];
    Memo.return ()

let fallback_artefacts sctx local_dir =
  let ctx = Super_context.context sctx in
  let cmti_paths =
    List.map ~f:(fun path -> Path.relative path local_dir) ctx.findlib_paths
  in
  let+ mods = Memo.List.map ~f:(modules_of_dir ~recursive:true) cmti_paths in
  let mods = List.flatten mods in
  List.fold_left mods ~init:[]
    ~f:(fun acc (mod_name, (subpath, cmti_file, _)) ->
      let artefact =
        external_odoc_artefact sctx
          (local_dir ^ "/" ^ subpath)
          (mod_name, cmti_file, not (contains_double_underscore (Module_name.to_string mod_name)))
      in
      artefact :: acc)

let fallback_external_rules sctx local_dir libs =
  if String.contains local_dir '/' then Memo.return ()
  else (
    Log.info
      [ Pp.textf "NFT: fallback_external_rules called for %s (libs=%s)"
          local_dir
          (String.concat ~sep:","
             (List.map ~f:(fun (x, _) -> Lib_name.to_string x) libs))
      ];
    let ctx = Super_context.context sctx in
    let index_path = Paths.fallback_index_mld ctx local_dir in
    let parent = Mld.create (FallbackIndex local_dir) index_path in
    let target = ExtLib local_dir in
    let cmti_paths =
      List.map ~f:(fun path -> Path.relative path local_dir) ctx.findlib_paths
    in
    let* mods = Memo.List.map ~f:(modules_of_dir ~recursive:true) cmti_paths in
    let mods = List.flatten mods in
    let* artefacts = fallback_artefacts sctx local_dir in

    let requires =
      List.fold_left libs ~init:[] ~f:(fun acc (_, lib) ->
          let info = Dune_package.Lib.info lib in
          let requires =
            Lib_info.requires info
            |> List.filter_map ~f:(function
                 | Lib_dep.Direct (_, x) -> Some (Loc.none, x)
                 | _ -> None)
          in
          requires @ acc)
    in
    let* public_libs = Scope.DB.public_libs ctx in
    let* all_requires =
      List.map
        ~f:(Lib.DB.resolve public_libs)
        ((Loc.none, Lib_name.of_string "stdlib") :: requires)
      |> Resolve.Memo.all
    in
    let requires =
      let open Resolve.O in
      let+ requires = all_requires in
      let cur_libs = List.map ~f:fst libs in
      List.filter
        ~f:(fun x -> not (List.mem cur_libs (Lib.name x) ~equal:Lib_name.equal))
        requires
    in

    let modules_names =
      let output_dir = Paths.odocs ctx (ExtLib local_dir) in
      List.map
        ~f:(fun (x, (subpath, _, _)) ->
          ( Module_name.Unique.of_name_assuming_needs_no_mangling x
          , output_dir ++ subpath ))
        mods
    in
    let* () = Memo.List.iter artefacts ~f:(fun artefact ->
      let parent_opt =
        match artefact.source with
        | Module (_, true) -> Some parent
        | _ -> None
      in    
      compile_external_odoc artefact sctx modules_names parent_opt
        requires)
    in
    Log.info
      [ Pp.textf "NFT: here we are, with %d artefacts" (List.length artefacts) ];
    let* _ =
      Memo.List.iter artefacts ~f:(fun artefact ->
          let+ () =
            link_odoc_rules sctx artefact ~package:None ~requires:all_requires
          in
          ())
    in
    Dep.setup_deps ctx target
      (Path.Set.of_list (List.map ~f:(fun a -> Path.build a.odoc_file) artefacts)))


type dune_with_modules = {
  local_dir : string;
  lib_name : Lib_name.t;
  lib : Dune_package.Lib.t;
  modules : Modules.t;
  entry_modules : Module_name.t list;
}

type fallback = {
    libs : Dune_package.Lib.t Lib_name.Map.t
}

type local_dir_type =
    | Nothing
    | Dune_with_modules of dune_with_modules list
    | Fallback of fallback

let singleton_artefacts sctx dwm =
  let info = Dune_package.Lib.info dwm.lib in
  let obj_dir = Lib_info.obj_dir info in
  let artefacts =
    Modules.fold_no_vlib dwm.modules ~init:[] ~f:(fun m acc ->
      let name = Module.obj_name m |> Module_name.Unique.to_name ~loc:Loc.none in
      let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind:(Ocaml Cmi) m in
      let visible = List.mem dwm.entry_modules (Module.name m) ~equal:(fun m1 m2 ->
        Module_name.equal m1 m2) in
      let artefact =
        external_odoc_artefact sctx dwm.local_dir
          (name, cmti_file, visible)
      in
      artefact :: acc)
  in
  artefacts

let singleton_external_rules sctx dwm =
  let ctx = Super_context.context sctx in
  let pkg = Lib_name.package_name dwm.lib_name in
  let index_path = Paths.ext_package_index_mld ctx pkg in
  let parent = Mld.create (ExtIndex pkg) index_path in
  let info = Dune_package.Lib.info dwm.lib in
  let obj_dir = info |> Lib_info.obj_dir |> Obj_dir.obj_dir in
  let local_path = Paths.local_path_of_findlib_path ctx obj_dir in
  let target = ExtLib local_path in
    let artefacts = singleton_artefacts sctx dwm in
    let requires =
      Lib_info.requires info
      |> List.filter_map ~f:(function
           | Lib_dep.Direct (_, x) -> Some (Loc.none, x)
           | _ -> None)
    in
    let* public_libs = Scope.DB.public_libs ctx in
    let* requires =
      List.map
        ~f:(Lib.DB.resolve public_libs)
        ((Loc.none, Lib_name.of_string "stdlib") :: requires)
      |> Resolve.Memo.all
    in
    let modules_names =
      let output_dir = Paths.odocs ctx (ExtLib dwm.local_dir) in
      Modules.fold_no_vlib dwm.modules ~init:[] ~f:(fun m acc ->
          (Module.obj_name m, output_dir) :: acc)
    in
    let* () = Memo.List.iter artefacts ~f:(fun artefact ->
      let parent_opt =
        match artefact.source with
        | Module (_, true) -> Some parent
        | _ -> None
      in
      compile_external_odoc artefact sctx modules_names parent_opt
        requires
      
      ) in
    let* _ =
      Memo.List.iter artefacts ~f:(fun artefact ->
          let+ () = link_odoc_rules sctx artefact ~package:None ~requires in
          ())
    in
    Dep.setup_deps ctx target
      (Path.Set.of_list (List.map ~f:(fun a -> Path.build a.odoc_file) artefacts))


exception Fallback

let classify_local_dir sctx local_dir =
  let+ map = libs_of_local_dir (Super_context.context sctx) in
  (* String.Map.iteri map ~f:(fun dir libs ->
      if Lib_name.Map.cardinal libs > 1 then
        Log.info [ Pp.textf "NFT: Dir %s contains more than one lib" dir ]); *)
  match String.Map.find map local_dir with
  | None ->
    Log.info [ Pp.textf "NFT: No lib at this path: %s" local_dir ];
    Nothing
  | Some libs ->
    try
      let f local_dir libs acc =
        match Lib_name.Map.to_list libs with
        | [(lib_name, lib)] -> begin
          let info = Dune_package.Lib.info lib in
          let mods_opt = Dune_package.Lib.modules lib in
          match (mods_opt, Lib_info.entry_modules info) with
          | Some modules, External (Ok entry_modules) ->
            { local_dir; lib_name; lib; modules; entry_modules } :: acc
          | _ -> raise Fallback
          end
        | _ -> raise Fallback
      in
      Dune_with_modules (String.Map.foldi libs ~f ~init:[])
    with _ ->
      Log.info [ Pp.textf "NFT: Multiple libs found at path: %s" local_dir ];
      let libs =
        String.Map.foldi libs ~init:Lib_name.Map.empty ~f:(fun p sublibs acc ->
            Log.info [ Pp.textf "NFT: Combining dir %s" p ];
            Lib_name.Map.merge acc sublibs ~f:(fun _ y z ->
                match (y, z) with
                | None, None -> None
                | Some x, _ -> Some x
                | _, Some x -> Some x))
      in
      Fallback {libs}

let setup_external_rules sctx local_dir =
  (* String.Map.iteri map ~f:(fun dir libs ->
      if Lib_name.Map.cardinal libs > 1 then
        Log.info [ Pp.textf "NFT: Dir %s contains more than one lib" dir ]); *)
  let* c = classify_local_dir sctx local_dir in
  match c with
  | Nothing ->
    Log.info [ Pp.textf "NFT: No lib at this path: %s" local_dir ];
    Memo.return ()
  | Dune_with_modules m ->
    let* _ = List.map m ~f:(fun m ->
      singleton_external_rules sctx m)
      |> Memo.all in
      Memo.return ()
  | Fallback {libs} ->
    fallback_external_rules sctx local_dir (Lib_name.Map.to_list libs)

let has_rules ?(directory_targets = Path.Build.Map.empty) m =
  let rules = Rules.collect_unit (fun () -> m) in
  Memo.return
    (Build_config.Rules
       { rules
       ; build_dir_only_sub_dirs = Build_config.Rules.Build_only_sub_dirs.empty
       ; directory_targets
       })

let with_package pkg ~f =
  let pkg = Package.Name.of_string pkg in
  let* packages = Only_packages.get () in
  match Package.Name.Map.find packages pkg with
  | Some pkg -> has_rules (f pkg)
  | None ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs =
             Build_config.Rules.Build_only_sub_dirs.empty
         ; directory_targets = Path.Build.Map.empty
         })

let gen_rules sctx ~dir rest =
  match rest with
  | [] ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs =
             Build_config.Rules.Build_only_sub_dirs.singleton ~dir
               Subdir_set.All
         ; directory_targets = Path.Build.Map.empty
         })
  | [ "_html" ] ->
    let ctx = Super_context.context sctx in
    let directory_targets =
      Path.Build.Map.singleton (Paths.odoc_support ctx) Loc.none
    in
    has_rules ~directory_targets
      (setup_css_rule sctx >>> setup_toplevel_index_rule sctx)
  | [ "_index_pages"; pkg ] ->
    with_package pkg ~f:(fun pkg -> setup_pkg_index_rules sctx pkg)
  | [ "_index_private"; lnu ] -> has_rules (setup_lnu_index_rules sctx lnu)
  | [ "_index_external"; pkg ] ->
    has_rules (setup_external_index_rules sctx (Package.Name.of_string pkg))
  | [ "_index_fallback"; dir ] ->
    has_rules (setup_fallback_index_rules sctx dir)
  | [ "_odoc"; "pkg"; pkg ] ->
    with_package pkg ~f:(fun pkg -> setup_package_odoc_rules sctx ~pkg)
  | [ "_odoc"; "external"; pkg ] -> has_rules (setup_external_rules sctx pkg)
  | [ "_odocls"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string ctx lib_unique_name_or_pkg in
       let setup_pkg_odocl_rules pkg =
         let* pkg_libs = libs_of_pkg ctx ~pkg in
         setup_pkg_odocl_rules sctx ~pkg ~libs:pkg_libs
       in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib -> (
           match Lib_info.package (Lib.Local.info lib) with
           | None ->
             let* requires =
               Lib.closure [ Lib.Local.to_lib lib ] ~linking:false
             in
             setup_lib_odocl_rules sctx lib ~requires
           | Some pkg -> setup_pkg_odocl_rules pkg)
       and+ () =
         let* packages = Only_packages.get () in
         match
           Package.Name.Map.find packages
             (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_odocl_rules name
       in
       ())
  | [ "_html"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string ctx lib_unique_name_or_pkg in
       let setup_pkg_html_rules pkg =
         let* pkg_libs = libs_of_pkg (Super_context.context sctx) ~pkg in
         setup_pkg_html_rules sctx ~pkg ~libs:pkg_libs
       in
       (* jeremiedimino: why isn't [None] some kind of error here? *)
       let* lib =
         let+ lib = Lib.DB.find lib_db lib in
         Option.bind ~f:Lib.Local.of_lib lib
       in
       let+ () =
         match lib with
         | None -> Memo.return ()
         | Some lib -> (
           match Lib_info.package (Lib.Local.info lib) with
           | None -> setup_lib_html_rules sctx lib
           | Some pkg -> setup_pkg_html_rules pkg)
       and+ () =
         let* packages = Only_packages.get () in
         match
           Package.Name.Map.find packages
             (Package.Name.of_string lib_unique_name_or_pkg)
         with
         | None -> Memo.return ()
         | Some pkg ->
           let name = Package.name pkg in
           setup_pkg_html_rules name
       in
       ())
  | _ -> Memo.return (Build_config.Redirect_to_parent Build_config.Rules.empty)
