open Import
open Dune_file
open Memo.O

(*

   layout:

   topdir:
   _build/default/_doc/

   external libraries:
   _external/<findlib dir>/deps - dependency information on a per-subdirectory basis
   _external/<findlib dir>/_odoc/<odocs> - odocs
   _external/<findlib dir>/_odocl/<odocls> - odocls

   _mlds/docs.mld - overall parent page
   _mlds/docs/p.mld - parent page for both findlib libs and packages

   _mlds/docs/l/<findlib name>.mld - parent mld
   _mlds/docs/l/<findlib name>/foo.mld - docs from odoc-pages
   _mlds/docs/p/<package name>.mld - parent mld

   _odoc/docs/l/page-<findlib name>.odoc 
   _odoc/docs/l/<findlib name>/page-foo.odoc
   _odoc/docs/p/page-<pkg name>.odoc

   _odocl/docs/l/page-<findlib name>.odocl
   _odoc/docs/l/<findlib name>/page-foo.odocl
   _odocl/docs/p/page-<pkg name>.odocl


   _html/docs/index.html
   _html/docs/<findlib name>/index.html
   _html/docs/<package name>/index.html



   Findlib has looser rules than dune, so should we distiguish between findlib libraries
   and dune packages?
*)
type mld_source =
  | MS_File
  | MS_Renamed_file of Path.Build.t
  | MS_String of string

type asource =
  | S_Module of (Module.t * Path.Build.t Obj_dir.t)
  | S_Mld of Path.Build.t * mld_source

type artefact_tree =
  { source : asource
  ; mld_children : string list (* e.g. "package" or "vendored" *)
  ; subdir : string
  ; odoc_dir : Path.Build.t
  ; odoc_file : string
  ; odocl_dir : Path.Build.t
  ; odocl_file : string
  ; html_dir : Path.Build.t
  ; html_file : string
  ; parent : artefact_tree option
  }

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
  | Findlib of Path.t

type source =
  | Module
  | Mld

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
  let root (context : Context.t) =
    Path.Build.relative context.Context.build_dir "_doc"

  let local_external_dir ctx obj_dir =
    let obj_dir_str = Path.to_string obj_dir in
    let findlib_paths =
      List.map ~f:(fun x -> Path.to_string x ^ "/") ctx.Context.findlib_paths
    in
    let paths =
      List.sort
        ~compare:(fun p1 p2 ->
          Int.compare (String.length p1) (String.length p2))
        findlib_paths
    in
    let prefix =
      List.find ~f:(fun prefix -> String.starts_with ~prefix obj_dir_str) paths
      |> Option.value_exn
    in
    let local =
      String.sub obj_dir_str ~pos:(String.length prefix)
        ~len:(String.length obj_dir_str - String.length prefix)
    in
    local

  let external_findlib_root ctx obj_dir =
    let local = local_external_dir ctx obj_dir in
    root ctx ++ "_external" ++ local

  let external_findlib_odoc ctx obj_dir =
    external_findlib_root ctx obj_dir ++ "_odoc"

  let external_findlib_odocl ctx obj_dir =
    external_findlib_root ctx obj_dir ++ "_odocl"

  let odocs ctx = function
    | Lib lib ->
      let obj_dir = Lib.Local.obj_dir lib in
      Obj_dir.odoc_dir obj_dir
    | Pkg pkg -> root ctx ++ sprintf "_odoc/pkg/%s" (Package.Name.to_string pkg)
    | Findlib p -> external_findlib_odoc ctx p

  let odocs_root ctx = root ctx ++ "_odoc"

  let html_root ctx = root ctx ++ "_html"

  let odocl_root ctx = root ctx ++ "_odocls"

  let mld_root ctx = root ctx ++ "_mlds"

  let deps_filename ctx obj_dir = external_findlib_root ctx obj_dir ++ "deps"

  let objinfo_filename ctx obj_dir cma_path =
    let cma_name = Path.basename cma_path |> Filename.chop_extension in
    external_findlib_root ctx obj_dir ++ sprintf "%s.objinfo" cma_name

  let add_pkg_lnu base m =
    base
    ++
    match m with
    | Pkg pkg -> Package.Name.to_string pkg
    | Lib lib -> pkg_or_lnu (Lib.Local.to_lib lib)
    | Findlib _p -> failwith "what to do here?"

  let html ctx m = add_pkg_lnu (html_root ctx) m

  let odocl ctx m = add_pkg_lnu (odocl_root ctx) m

  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg

  let css_file ctx = html_root ctx ++ "odoc.css"

  let highlight_pack_js ctx = html_root ctx ++ "highlight.pack.js"

  let toplevel_index ctx = html_root ctx ++ "index.html"

  let docs_mld_path ctx = mld_root ctx ++ "docs.mld"

  let packages_mld_path ctx = mld_root ctx ++ "docs/p.mld"

  let package_mld_path ctx pkg =
    mld_root ctx ++ "docs/p"
    ++
    let n = Package.Name.to_string pkg in
    sprintf "%s.mld" n

  let package_ext_mld_path ctx pkg =
    mld_root ctx ++ "docs/l"
    ++
    let n = Package.Name.to_string pkg in
    sprintf "%s.mld" n
end

let odoc_ext = ".odoc"

let odocl_ext = ".odoc"

let mknode ctx ~source ~mld_children ~parent =
  let odoc_dir, odoc_file =
    match source with
    | S_Module (m, obj_dir) ->
      let obj_name = Module.obj_name m in
      let basename =
        Module_name.Unique.artifact_filename obj_name ~ext:odoc_ext
      in
      (Obj_dir.odoc_dir obj_dir, basename)
    | S_Mld (f, _) ->
      let t = Filename.chop_extension (Path.Build.basename f) in
      let dir =
        Option.value ~default:(Paths.odocs_root ctx)
          (Option.map parent ~f:(fun p -> p.odoc_dir ++ p.subdir))
      in
      (dir, sprintf "page-%s%s" t odoc_ext)
  in
  let odocl_dir, odocl_file =
    let dir =
      Option.value ~default:(Paths.odocl_root ctx)
        (Option.map parent ~f:(fun p -> p.odocl_dir ++ p.subdir))
    in
    match source with
    | S_Module (m, _) ->
      let obj_name = Module.obj_name m in
      let basename =
        Module_name.Unique.artifact_filename obj_name ~ext:odocl_ext
      in
      (dir, basename)
    | S_Mld (f, _) ->
      let t = Filename.chop_extension (Path.Build.basename f) in
      (dir, sprintf "page-%s%s" t odocl_ext)
  in
  let html_dir, html_file =
    let dir =
      Option.value ~default:(Paths.html_root ctx)
        (Option.map parent ~f:(fun p -> p.html_dir ++ p.subdir))
    in
    match source with
    | S_Module (f, _) ->
      let basename = Module.name f |> Module_name.to_string in
      let subdir = Stdune.String.capitalize basename in
      (dir, sprintf "%s/index.html" subdir)
    | S_Mld (f, _) -> (
      let basename = Path.Build.basename f |> Filename.chop_extension in
      let dir =
        Option.value ~default:(Paths.html_root ctx)
          (Option.map parent ~f:(fun p -> p.html_dir ++ p.subdir))
      in
      match mld_children with
      | [] -> (dir, sprintf "%s.html" basename)
      | _ -> (dir, sprintf "%s/index.html" basename))
  in
  let subdir =
    match source with
    | S_Module (f, _) ->
      Module.name f |> Module_name.to_string |> String.capitalize
    | S_Mld (f, _) -> Path.Build.basename f |> Filename.chop_extension
  in
  { source
  ; mld_children
  ; subdir
  ; odoc_dir
  ; odoc_file
  ; odocl_dir
  ; odocl_file
  ; html_dir
  ; html_file
  ; parent
  }

module StdMlds = struct
  let docs ctx =
    let sp = sprintf in
    let* packages = Only_packages.get () in
    let list_items =
      Package.Name.Map.to_list packages
      |> List.filter_map ~f:(fun (name, pkg) ->
             let name = Package.Name.to_string name in
             let link = sp {|{!page-%s}%s}|} name name in
             let version_suffix =
               match pkg.Package.version with
               | None -> ""
               | Some v -> sp {| (version %s)|} v
             in
             Some (sp "- %s%s" link version_suffix))
      |> String.concat ~sep:"\n"
    in
    let doc_contents = sp {|{0 Docs}
      
      %s
  
      |} list_items in
    let source = S_Mld (Paths.docs_mld_path ctx, MS_String doc_contents) in
    let mld_children = [ "packages" ] in
    Memo.return (mknode ctx ~source ~mld_children ~parent:None)

  let packages ctx =
    Log.info [ Pp.textf "StdMlds.packages" ];
    let* packages = Only_packages.get () in
    let mld_children =
      Package.Name.Map.to_list_map packages ~f:(fun n _ ->
          Package.Name.to_string n)
    in
    let source =
      S_Mld (Paths.packages_mld_path ctx, MS_String "packages mld")
    in
    let+ docs = docs ctx in
    mknode ctx ~source ~mld_children ~parent:(Some docs)

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
    let obj_dir = Lib_info.obj_dir info in
    let dir = Lib_info.src_dir info in
    let name = Lib.name (Lib.Local.to_lib lib) in
    let+ x =
      Dir_contents.get sctx ~dir >>= Dir_contents.ocaml
      >>| Ml_sources.modules ~for_:(Library name)
      >>| Modules.entry_modules
    in
    (x, obj_dir)

  let entry_modules sctx ~pkg =
    let* l =
      libs_of_pkg (Super_context.context sctx) ~pkg
      >>| List.filter ~f:(fun lib ->
              Lib.Local.info lib |> Lib_info.status
              |> Lib_info.Status.is_private |> not)
    in
    let+ l =
      Memo.parallel_map l ~f:(fun l ->
          let+ m = entry_modules_by_lib sctx l in
          (l, m))
    in
    Lib.Local.Map.of_list_exn l

  let default_index ~pkg
      (entry_modules : (Module.t list * Path.Build.t Obj_dir.t) Lib.Local.Map.t)
      =
    let b = Buffer.create 512 in
    Printf.bprintf b "{0 %s index}\n" (Package.Name.to_string pkg);
    Lib.Local.Map.to_list entry_modules
    |> List.sort ~compare:(fun (x, _) (y, _) ->
           let name lib = Lib.name (Lib.Local.to_lib lib) in
           Lib_name.compare (name x) (name y))
    |> List.iter ~f:(fun (lib, (modules, _)) ->
           let lib = Lib.Local.to_lib lib in
           Printf.bprintf b "{1 Library %s}\n"
             (Lib_name.to_string (Lib.name lib));
           Buffer.add_string b
             (match modules with
             | [ x ] ->
               sprintf
                 "The entry point of this library is the module:\n\
                  {!module-%s}.\n"
                 (Module_name.to_string (Module.name x))
             | _ ->
               sprintf
                 "This library exposes the following toplevel modules:\n\
                  {!modules:%s}\n"
                 (modules
                 |> List.filter ~f:(fun m ->
                        Module.visibility m = Visibility.Public)
                 |> List.sort ~compare:(fun x y ->
                        Module_name.compare (Module.name x) (Module.name y))
                 |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
                 |> String.concat ~sep:" ")));
    Buffer.contents b

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

  let package =
    let memo =
      Memo.create "package-mlds"
        ~input:(module Super_context.As_memo_key.And_package)
        (fun (sctx, pkg) ->
          Log.info
            [ Pp.textf "StdMlds.package %s"
                (Package.Name.to_string (Package.name pkg))
            ];

          (* CR-someday jeremiedimino: it is weird that we drop the
              [Package.t] and go back to a package name here. Need to try and
              change that one day. *)
          let pkg = Package.name pkg in
          let* mlds = Packages.mlds sctx pkg in
          let mlds = check_mlds_no_dupes ~pkg ~mlds in
          let ctx = Super_context.context sctx in
          let index_exists = String.Map.mem mlds "index" in
          let pkg_mld = Paths.package_mld_path ctx pkg in
          let* entry_modules = entry_modules sctx ~pkg in
          let source =
            if index_exists then
              MS_Renamed_file (String.Map.find_exn mlds "index")
            else MS_String (default_index ~pkg entry_modules)
          in
          let mlds = String.Map.remove mlds "index" in
          let mld_children =
            String.Map.fold ~init:[]
              ~f:(fun mld list ->
                let name = Path.Build.basename mld |> Filename.chop_extension in
                name :: list)
              mlds
          in

          let+ parent = packages ctx in
          let pkg =
            mknode ctx
              ~source:(S_Mld (pkg_mld, source))
              ~mld_children ~parent:(Some parent)
          in
          let children =
            String.Map.fold ~init:[]
              ~f:(fun mld list ->
                let source = MS_File in
                mknode ctx
                  ~source:(S_Mld (mld, source))
                  ~mld_children:[] ~parent:(Some pkg)
                :: list)
              mlds
          in
          let module_children =
            Lib.Local.Map.fold ~init:[]
              ~f:(fun (modules, obj_dir) l ->
                List.fold_left ~init:l
                  ~f:(fun l m ->
                    let source = S_Module (m, obj_dir) in
                    mknode ctx ~source ~mld_children:[] ~parent:(Some pkg) :: l)
                  modules)
              entry_modules
          in
          (pkg, children @ module_children))
    in
    fun sctx ~pkg -> Memo.exec memo (sctx, pkg)
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
  >>> Command.run ~dir program Command.Args.[ A command; base_flags; S args ]

let compile_mld sctx (artefact : artefact_tree) =
  let open Memo.O in
  let odoc_input =
    match artefact.source with
    | S_Mld (f, _) -> f
    | _ -> failwith "Invalid artefact"
  in
  let odoc_reference (a : artefact_tree) =
    Filename.chop_extension a.odoc_file
  in
  let children =
    List.fold_left ~init:[]
      ~f:(fun args mld ->
        Command.Args.A "--child" :: A (sprintf "page-\"%s\"" mld) :: args)
      artefact.mld_children
  in
  let parent =
    Option.map artefact.parent ~f:(fun m ->
        [ Command.Args.A "-I"
        ; Path (Path.build m.odoc_dir)
        ; A "--parent"
        ; A (odoc_reference m)
        ; Hidden_deps
            (Import.Dep.Set.of_files [ Path.build (m.odoc_dir ++ m.odoc_file) ])
        ])
  in
  let parent_args = Option.value ~default:[] parent in
  Log.info
    [ Pp.textf "target: %s"
        (Path.Build.to_string (artefact.odoc_dir ++ artefact.odoc_file))
    ];
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.build artefact.odoc_dir)
      "compile" ~flags_for:(Some odoc_input)
      ([ Command.Args.A "-o"
       ; Target (artefact.odoc_dir ++ artefact.odoc_file)
       ; Dep (Path.build odoc_input)
       ]
      @ parent_args @ children)
  in
  add_rule sctx run_odoc

let mld_rules sctx (artefacts : artefact_tree list) =
  let test artefact =
    ( artefact.mld_children
    , artefact.html_file
    , artefact.html_file
    , artefact.odocl_file
    , artefact.odocl_dir
    , artefact.odoc_file
    , artefact.parent )
  in
  let mld_rule (a : artefact_tree) =
    ignore (test a);
    match a.source with
    | S_Mld (f, MS_String s) ->
      Log.info [ Pp.textf "write file - path: %s" (Path.Build.to_string f) ];
      Some (Action_builder.write_file f s)
    | S_Mld (f, MS_Renamed_file orig) ->
      Log.info [ Pp.textf "symlink - path: %s" (Path.Build.to_string f) ];
      Some (Action_builder.symlink ~src:(Path.build orig) ~dst:f)
    | S_Mld (_, _) -> None
    | S_Module _ -> None
  in
  List.filter_map ~f:mld_rule artefacts |> Memo.List.iter ~f:(add_rule sctx)

let odoc_rules sctx artefacts =
  let odoc_rule (a : artefact_tree) =
    match a.source with
    | S_Mld _ -> compile_mld sctx a
    | _ -> Memo.return ()
  in
  Memo.List.iter ~f:odoc_rule artefacts

module OdocDep : sig
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

  val alias : Context.t -> target -> Alias.t

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
           | None -> acc
           | Some lib ->
             let dir = Paths.odocs ctx (Lib lib) in
             let alias = alias ~dir in
             Dep.Set.add acc (Dep.alias alias)))

  let alias ctx m =
    match m with
    | Findlib p ->
      Log.info
        [ Pp.textf "Adding alias for path %s"
            (Paths.external_findlib_odoc ctx p |> Path.Build.to_string)
        ];
      alias ~dir:(Paths.external_findlib_odoc ctx p)
    | _ -> alias ~dir:(Paths.odocs ctx m)

  let setup_deps ctx m files =
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
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
    let t = Filename.chop_extension (Path.Build.basename t) in
    Path.Build.relative doc_dir (sprintf "page-%s%s" t odoc_ext)

  let odoc_input t = t
end

let odoc_base_flags sctx build_dir =
  let open Memo.O in
  let+ conf = Super_context.odoc sctx ~dir:build_dir in
  match conf.Env_node.Odoc.warnings with
  | Fatal -> Command.Args.A "--warn-error"
  | Nonfatal -> S []

let run_odoc sctx ~dir command ~flags_for args =
  let build_dir = (Super_context.context sctx).build_dir in
  let open Memo.O in
  let* program =
    Super_context.resolve_program sctx ~dir:build_dir "odoc" ~loc:None
      ~hint:"opam install odoc"
  in
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

let compile_module sctx ~obj_dir (m : Module.t) ~includes:(file_deps, iflags)
    ~dep_graphs ~pkg_or_lnu =
  let odoc_file = Obj_dir.Module.odoc obj_dir m in
  let open Memo.O in
  let+ () =
    let* action_with_targets =
      let doc_dir = Path.build (Obj_dir.odoc_dir obj_dir) in
      let+ run_odoc =
        run_odoc sctx ~dir:doc_dir "compile" ~flags_for:(Some odoc_file)
          [ A "-I"
          ; Path doc_dir
          ; iflags
          ; As [ "--pkg"; pkg_or_lnu ]
          ; A "-o"
          ; Target odoc_file
          ; Dep (Path.build (Obj_dir.Module.cmti_file obj_dir m))
          ]
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  (m, odoc_file)

let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let open Memo.O in
  let odoc_file = Mld.odoc_file m ~doc_dir in
  let odoc_input = Mld.odoc_input m in
  let* run_odoc =
    run_odoc sctx ~dir:(Path.build doc_dir) "compile"
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

let odoc_include_flags ctx pkg requires =
  Resolve.args
    (let open Resolve.O in
    let+ libs = requires in
    let paths =
      List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
          match Lib.Local.of_lib lib with
          | None -> paths
          | Some lib ->
            Path.Set.add paths (Path.build (Paths.odocs ctx (Lib lib))))
    in
    let paths =
      match pkg with
      | Some p -> Path.Set.add paths (Path.build (Paths.odocs ctx (Pkg p)))
      | None -> paths
    in
    Command.Args.S
      (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
           [ Command.Args.A "-I"; Path dir ])))

let link_odoc_rules sctx (odoc_file : odoc_artefact) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  let deps = OdocDep.deps ctx pkg requires in
  let open Memo.O in
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "link" ~flags_for:(Some odoc_file.odoc_file)
      [ odoc_include_flags ctx pkg requires
      ; A "-o"
      ; Target odoc_file.odocl_file
      ; Dep (Path.build odoc_file.odoc_file)
      ]
  in
  add_rule sctx
    (let open Action_builder.With_targets.O in
    Action_builder.with_no_targets deps >>> run_odoc)

let setup_library_odoc_rules cctx (local_lib : Lib.Local.t) =
  let open Memo.O in
  (* Using the proper package name doesn't actually work since odoc assumes that
     a package contains only 1 library *)
  let pkg_or_lnu = pkg_or_lnu (Lib.Local.to_lib local_lib) in
  let sctx = Compilation_context.super_context cctx in
  let ctx = Super_context.context sctx in
  let* requires = Compilation_context.requires_compile cctx in
  let info = Lib.Local.info local_lib in
  let package = Lib_info.package info in
  let odoc_include_flags =
    Command.Args.memo (odoc_include_flags ctx package requires)
  in
  let obj_dir = Compilation_context.obj_dir cctx in
  let modules = Compilation_context.modules cctx in
  let includes = (OdocDep.deps ctx package requires, odoc_include_flags) in
  let modules_and_odoc_files =
    Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
        let compiled =
          compile_module sctx ~includes
            ~dep_graphs:(Compilation_context.dep_graphs cctx)
            ~obj_dir ~pkg_or_lnu m
        in
        compiled :: acc)
  in
  let* modules_and_odoc_files = Memo.all_concurrently modules_and_odoc_files in
  OdocDep.setup_deps ctx (Lib local_lib)
    (Path.Set.of_list_map modules_and_odoc_files ~f:(fun (_, p) -> Path.build p))

let setup_html sctx (odoc_file : odoc_artefact) =
  let ctx = Super_context.context sctx in
  let to_remove, dummy =
    match odoc_file.source with
    | Mld -> (odoc_file.html_file, [])
    | Module ->
      (* Dummy target so that the bellow rule as at least one target. We do this
         because we don't know the targets of odoc in this case. The proper way
         to support this would be to have directory targets. *)
      let dummy = Action_builder.create_file (odoc_file.html_dir ++ ".dummy") in
      (odoc_file.html_dir, [ dummy ])
  in
  let open Memo.O in
  let* run_odoc =
    run_odoc sctx
      ~dir:(Path.build (Paths.html_root ctx))
      "html-generate" ~flags_for:None
      [ A "-o"
      ; Path (Path.build (Paths.html_root ctx))
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
                   ; Action.Mkdir (Path.build odoc_file.html_dir)
                   ])))
       :: run_odoc :: dummy))

let setup_css_rule sctx =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let* run_odoc =
    run_odoc sctx ~dir:(Path.build ctx.build_dir) "support-files"
      ~flags_for:None
      [ A "-o"
      ; Path (Path.build (Paths.html_root ctx))
      ; Hidden_targets [ Paths.css_file ctx; Paths.highlight_pack_js ctx ]
      ]
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
    <link rel="stylesheet" href="./odoc.css"/>
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
      list_items
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

let load_all_odoc_rules_pkg sctx ~pkg =
  let* pkg_libs = libs_of_pkg sctx ~pkg in
  let+ () =
    Memo.parallel_iter
      (Pkg pkg :: List.map pkg_libs ~f:(fun lib -> Lib lib))
      ~f:(fun _ -> Memo.return ())
  in
  pkg_libs

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

let create_odoc ctx ~target odoc_file =
  let html_base = Paths.html ctx target in
  let odocl_base = Paths.odocl ctx target in
  let basename = Path.Build.basename odoc_file |> Filename.chop_extension in
  let odocl_file = odocl_base ++ (basename ^ ".odocl") in
  match target with
  | Lib _ ->
    let html_dir = html_base ++ Stdune.String.capitalize basename in
    { odoc_file
    ; odocl_file
    ; html_dir
    ; html_file = html_dir ++ "index.html"
    ; source = Module
    }
  | Pkg _ ->
    { odoc_file
    ; odocl_file
    ; html_dir = html_base
    ; html_file =
        html_base
        ++ sprintf "%s.html"
             (basename |> String.drop_prefix ~prefix:"page-" |> Option.value_exn)
    ; source = Mld
    }
  | _ -> failwith "bad"

let static_html ctx =
  let open Paths in
  [ css_file ctx; highlight_pack_js ctx; toplevel_index ctx ]

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
  let dir = Paths.odocs ctx target in
  match target with
  | Pkg pkg ->
    let+ mlds =
      let+ mlds = Packages.mlds sctx pkg in
      let mlds = check_mlds_no_dupes ~pkg ~mlds in
      if String.Map.mem mlds "index" then mlds
      else
        let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
        String.Map.add_exn mlds "index" gen_mld
    in
    String.Map.values mlds
    |> List.map ~f:(fun mld ->
           Mld.create mld |> Mld.odoc_file ~doc_dir:dir
           |> create_odoc ctx ~target)
  | Lib lib ->
    let info = Lib.Local.info lib in
    let obj_dir = Lib_info.obj_dir info in
    let* modules = entry_modules_by_lib sctx lib in
    List.map
      ~f:(fun m ->
        let odoc_file = Obj_dir.Module.odoc obj_dir m in
        create_odoc ctx ~target odoc_file)
      modules
    |> Memo.return
  | _ -> failwith "bad"

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
    let pkg = Lib_info.package (Lib.Local.info lib) in
    Memo.parallel_iter odocs ~f:(fun odoc ->
        link_odoc_rules sctx ~pkg ~requires odoc)
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
      let pkg = Some pkg in
      let+ () =
        Memo.parallel_iter pkg_odocs ~f:(fun odoc ->
            link_odoc_rules sctx ~pkg ~requires odoc)
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
      (OdocDep.html_alias ctx (Lib lib))
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
    let* () = Memo.parallel_iter libs ~f:(setup_lib_html_rules sctx)
    and* pkg_odocs =
      let* pkg_odocs = odoc_artefacts sctx (Pkg pkg) in
      let+ () = Memo.parallel_iter pkg_odocs ~f:(fun o -> setup_html sctx o) in
      pkg_odocs
    and* lib_odocs =
      Memo.parallel_map libs ~f:(fun lib -> odoc_artefacts sctx (Lib lib))
    in
    let odocs = List.concat (pkg_odocs :: lib_odocs) in
    let html_files = List.map ~f:(fun o -> Path.build o.html_file) odocs in
    let static_html = List.map ~f:Path.build (static_html ctx) in
    Rules.Produce.Alias.add_deps
      (OdocDep.html_alias ctx (Pkg pkg))
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
    >>| List.map ~f:(fun lib -> OdocDep.html_alias ctx (Lib lib))
  in
  OdocDep.html_alias ctx (Pkg name) :: libs
  |> Dune_engine.Dep.Set.of_list_map ~f:(fun f -> Dune_engine.Dep.alias f)
  |> Action_builder.deps
  |> Rules.Produce.Alias.add_deps alias

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
         Buffer.add_string b
           (match modules with
           | [ x ] ->
             sprintf
               "The entry point of this library is the module:\n{!module-%s}.\n"
               (Module_name.to_string (Module.name x))
           | _ ->
             sprintf
               "This library exposes the following toplevel modules:\n\
                {!modules:%s}\n"
               (modules
               |> List.filter ~f:(fun m ->
                      Module.visibility m = Visibility.Public)
               |> List.sort ~compare:(fun x y ->
                      Module_name.compare (Module.name x) (Module.name y))
               |> List.map ~f:(fun m -> Module_name.to_string (Module.name m))
               |> String.concat ~sep:" ")));
  Buffer.contents b

let package_mlds =
  let memo =
    Memo.create "package-mlds"
      ~input:(module Super_context.As_memo_key.And_package)
      (fun (sctx, pkg) ->
        Rules.collect (fun () ->
            (* CR-someday jeremiedimino: it is weird that we drop the
               [Package.t] and go back to a package name here. Need to try and
               change that one day. *)
            let pkg = Package.name pkg in
            let* mlds = Packages.mlds sctx pkg in
            let mlds = check_mlds_no_dupes ~pkg ~mlds in
            let ctx = Super_context.context sctx in
            if String.Map.mem mlds "index" then Memo.return mlds
            else
              let gen_mld = Paths.gen_mld_dir ctx pkg ++ "index.mld" in
              let* entry_modules = entry_modules sctx ~pkg in
              let+ () =
                add_rule sctx
                  (Action_builder.write_file gen_mld
                     (default_index ~pkg entry_modules))
              in
              String.Map.set mlds "index" gen_mld))
  in
  fun sctx ~pkg -> Memo.exec memo (sctx, pkg)

let setup_package_odoc_rules sctx ~pkg =
  let* mlds = package_mlds sctx ~pkg >>| fst in
  let ctx = Super_context.context sctx in
  (* CR-someday jeremiedimino: it is weird that we drop the [Package.t] and go
     back to a package name here. Need to try and change that one day. *)
  let pkg = Package.name pkg in
  let* odocs =
    Memo.parallel_map (String.Map.values mlds) ~f:(fun mld ->
        compile_mld sctx (Mld.create mld) ~pkg
          ~doc_dir:(Paths.odocs ctx (Pkg pkg))
          ~includes:(Action_builder.return []))
  in
  OdocDep.setup_deps ctx (Pkg pkg) (Path.set_of_build_paths_list odocs)

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
      (lib |> OdocDep.html_alias ctx |> Dune_engine.Dep.alias
     |> Action_builder.dep)

let has_rules m =
  let rules = Rules.collect_unit (fun () -> m) in
  Memo.return
    (Build_config.Rules
       { rules
       ; build_dir_only_sub_dirs = Subdir_set.empty
       ; directory_targets = Path.Build.Map.empty
       })

let with_package pkg ~f =
  let pkg = Package.Name.of_string pkg in
  let* packages = Only_packages.get () in
  match Package.Name.Map.find packages pkg with
  | None ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs = Subdir_set.empty
         ; directory_targets = Path.Build.Map.empty
         })
  | Some pkg -> has_rules (f pkg)

module ExternalDeps = struct
  (* Find external dependencies of internal or external modules *)

  (* type dep =
     | Findlib of (Lib_name.t * Module_name.t)
     | Dune_packge
     | Local *)

  let parse_odoc_deps lines =
    let rec parse_stanza l =
      match l with
      | modname :: fname :: deps ->
        let rec getdeps cur = function
          | "" :: rest -> (cur, rest)
          | x :: rest -> (
            match String.split ~on:' ' x with
            | [ m; hash ] ->
              getdeps ((Module_name.of_string m, hash) :: cur) rest
            | _ -> getdeps cur rest)
          | [] -> (cur, [])
        in
        let moddeps, rest = getdeps [] deps in
        (Module_name.of_string modname, (Path.of_string fname, moddeps))
        :: parse_stanza rest
      | _ -> []
    in
    parse_stanza lines

  let _parse_ooi lines =
    let prefix = "Unit name: " in
    let pos = String.length prefix in
    let parse line =
      if String.starts_with ~prefix line then
        Some
          (String.sub line ~pos ~len:(String.length line - pos)
          |> Module_name.of_string)
      else None
    in
    List.filter_map ~f:parse lines

  let deps_graph sctx findlib_deps =
    let ctx = Super_context.context sctx in
    let open Action_builder.O in
    let+ all_deps =
      Action_builder.List.fold_left ~init:[] findlib_deps
        ~f:(fun list obj_dir ->
          let+ lines =
            Action_builder.lines_of
              (Path.build (Paths.deps_filename ctx obj_dir))
          in
          let dict = parse_odoc_deps lines in
          List.fold_left ~init:list
            ~f:(fun list (module_name, (filename, deps)) ->
              let odoc_filename =
                Path.basename filename |> Filename.chop_extension
              in
              let odoc_path = Paths.external_findlib_odoc ctx obj_dir in
              (module_name, (odoc_path ++ sprintf "%s.odoc" odoc_filename, deps))
              :: list)
            dict)
    in
    let graph =
      List.fold_left ~init:[] all_deps
        ~f:(fun g (module_name, (odoc_filename, deps)) ->
          let odoc_files =
            List.filter_map
              ~f:(fun (module_name, _) ->
                match List.assoc all_deps module_name with
                | Some (odoc_dep, _) -> Some odoc_dep
                | None -> None)
              deps
          in
          match List.assoc all_deps module_name with
          | Some (_cmti_file, _) -> (odoc_filename, odoc_files) :: g
          | None -> g)
    in
    Path.Build.Map.of_list graph |> Result.value ~default:Path.Build.Map.empty

  let deps_graph sctx findlib_deps =
    Action_builder.memoize "deps_graph" (deps_graph sctx findlib_deps)
end

let deps_of_findlib_dir (sctx : Super_context.t) (dir : Path.t) =
  let ctx = Super_context.context sctx in
  let deps_destination = Paths.deps_filename ctx dir in
  let* odoc = odoc_program sctx (Paths.root ctx) in
  Log.info
    [ Pp.textf "Adding rule for %s"
        (Path.Build.to_string_maybe_quoted deps_destination)
    ];
  Super_context.add_rule sctx ~dir:(Paths.root ctx)
    (Command.run odoc
       ~dir:(Path.build (Paths.root ctx))
       ~stdout_to:deps_destination
       [ A "compile-deps-dir"; A (Path.to_string dir) ])

let obj_info_of_cma (sctx : Super_context.t) obj_dir cma =
  let ctx = Super_context.context sctx in
  let destination = Paths.objinfo_filename ctx obj_dir cma in
  let ocamlobjinfo = ctx.ocamlobjinfo in
  Super_context.add_rule sctx ~dir:(Paths.root ctx)
    (Command.run ocamlobjinfo
       ~dir:(Path.build (Paths.root ctx))
       ~stdout_to:destination [ Dep cma ])

(* let cmti_of_odoc_file sctx findlib_package odoc_file =
   let ctx = Super_context.context sctx in
   let* findlib =
   Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
   in
   let* package = Findlib.find findlib findlib_package in
   match package with
   | Error _ -> Memo.return None
   | Ok p ->
     match p with
     | Dune_package.Entry.Library l ->
     let info = Dune_package.Lib.info l in
     let od = Lib_info.obj_dir info in
     let base = Path.Build.basename odoc_file |> Filename.chop_extension in
     let extensions = [".cmti"; ".cmt"; ".cmi"] in
     Memo.List.find_map extensions ~f:(fun ext ->
       let p = Path.relative (Obj_dir.dir od) (sprintf "%s%s" base ext) in
       let+ exists = Fs_memo.file_exists (Path.as_outside_build_dir_exn p) in
       if exists then Some p else None
       )

     | _ -> Memo.return None *)

let compile_external_odoc sctx dir findlib_deps cmti_file odoc_file =
  Log.info
    [ Pp.textf "Rule for: %s (depending on %s)"
        (Path.Build.to_string odoc_file)
        (Path.to_string cmti_file)
    ];
  let ctx = Super_context.context sctx in
  let output_dir = Paths.external_findlib_odoc ctx dir in
  let db = ExternalDeps.deps_graph sctx findlib_deps in
  let deps =
    let open Action_builder.O in
    let* db = db in
    match
      Path.Build.Map.find db odoc_file
      (*~f:(fun (odoc_file', _) -> Path.Build.basename odoc_file = Path.Build.basename odoc_file') *)
    with
    | None -> Dep.Set.of_list [] |> Action_builder.deps
    | Some deps ->
      Log.info
        [ Pp.textf "dependencies for %s" (Path.Build.to_string odoc_file) ];
      List.filter ~f:(fun x -> x <> odoc_file) deps
      |> List.map ~f:(fun p ->
             Log.info [ Pp.textf "dependency: %s" (Path.Build.to_string p) ];
             Path.build p)
      |> Dep.Set.of_files |> Action_builder.deps
  in
  let other_deps =
    List.filter_map
      ~f:(fun d ->
        if d = dir then None
        else (
          Log.info
            [ Pp.textf "also depending on findlib package %s" (Path.to_string d)
            ];
          Some (OdocDep.alias ctx (Findlib d) |> Dep.alias)))
      findlib_deps
    |> Dep.Set.of_list
  in

  let iflags =
    List.map findlib_deps ~f:(fun dir ->
        let odoc_path = Paths.external_findlib_odoc ctx dir in
        Command.Args.[ A "-I"; Path (Path.build odoc_path) ])
    |> List.concat
  in
  let doc_dir = Path.build output_dir in
  let* action =
    let+ run_odoc =
      run_odoc sctx ~dir:doc_dir "compile" ~flags_for:(Some odoc_file)
        (Command.Args.
           [ A "-I"
           ; Path doc_dir
           ; A "-o"
           ; Target odoc_file
           ; Dep cmti_file
           ; Hidden_deps other_deps
           ]
        @ iflags)
    in
    let open Action_builder.With_targets.O in
    Action_builder.with_no_targets deps >>> run_odoc
  in
  Log.info
    [ Pp.textf "About to add rule for %s" (Path.Build.to_string odoc_file) ];
  add_rule sctx action

let default_external_mld sctx pkg =
  let ctx = Super_context.context sctx in
  let gen_mld = Paths.package_ext_mld_path ctx pkg in
  Log.info [Pp.textf "external mld rule for %s" (Path.Build.to_string gen_mld)];
  let+ () =
    add_rule sctx
      (Action_builder.write_file gen_mld
         (default_index ~pkg Lib.Local.Map.empty))
  in
  ()

let external_pkg_mld_rules sctx pkg =
  let ctx = Super_context.context sctx in
  let pkg_str = Package.Name.to_string pkg in
  let odoc_mld_path p =
    let ( ++ ) = Path.relative in
    (Path.Outside_build_dir.parent p
    |> Option.value_exn |> Path.outside_build_dir)
    ++ "doc" ++ pkg_str ++ "odoc-pages"
  in
  let* result =
    Memo.List.find_map ctx.findlib_paths ~f:(fun p ->
        let path =
          odoc_mld_path (Path.as_outside_build_dir_exn p)
          |> Path.as_outside_build_dir_exn
        in
        let* result = Fs_memo.dir_exists path in
        if result then Memo.return (Some path) else Memo.return None)
  in
  match result with
  | None -> default_external_mld sctx pkg
  | Some _ -> default_external_mld sctx pkg

let compile_rules_of_findlib_dir sctx dir deps =
  let ctx = Super_context.context sctx in
  let* dir_res = Fs_memo.dir_contents (Path.as_outside_build_dir_exn dir) in
  match dir_res with
  | Error _ -> Memo.return []
  | Ok d ->
    let list = Fs_cache.Dir_contents.to_list d in
    let modules =
      List.filter_map
        ~f:(fun (x, ty) ->
          if
            (ty = Unix.S_REG && String.ends_with ~suffix:".cmti" x)
            || String.ends_with ~suffix:".cmt" x
            || String.ends_with ~suffix:".cmi" x
          then Some (Filename.chop_extension x)
          else None)
        list
      |> List.sort_uniq ~compare:String.compare
    in
    let modules =
      List.map
        ~f:(fun mod_name ->
          let best_extension =
            List.find [ ".cmti"; ".cmt"; ".cmi" ] ~f:(fun ext ->
                List.exists ~f:(fun (x, _) -> x = mod_name ^ ext) list)
          in
          mod_name ^ Option.value_exn best_extension)
        modules
    in
    Memo.parallel_map
      ~f:(fun m ->
        let odoc_file_base = Filename.chop_extension m in
        let odoc_file =
          Paths.external_findlib_odoc ctx dir
          ++ sprintf "%s.odoc" odoc_file_base
        in
        let+ () =
          compile_external_odoc sctx dir deps (Path.relative dir m) odoc_file
        in
        odoc_file)
      modules

let link_rules_of_findlib_dir sctx odocs dir findlib_deps =
  let ctx = Super_context.context sctx in
  let contains_double_underscore s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then false
      else if s.[i] = '_' && s.[i + 1] = '_' then true
      else aux (i + 1)
    in
    aux 0
  in
  let non_hidden =
    List.filter odocs ~f:(fun p ->
        not (contains_double_underscore (Path.Build.basename p)))
  in
  let iflags =
    List.map findlib_deps ~f:(fun dir ->
        let odoc_path = Paths.external_findlib_odoc ctx dir in
        Command.Args.[ A "-I"; Path (Path.build odoc_path) ])
    |> List.concat
  in
  Memo.List.iter non_hidden ~f:(fun odoc_file ->
      let odoc_file_base =
        Path.Build.basename odoc_file |> Filename.chop_extension
      in
      let odocl_file =
        Paths.external_findlib_odocl ctx dir
        ++ sprintf "%s.odocl" odoc_file_base
      in
      let build_dir = Paths.external_findlib_odocl ctx dir in
      let* action =
        let+ run_odoc =
          run_odoc sctx ~dir:(Path.build build_dir) "link"
            ~flags_for:(Some odocl_file)
            (Command.Args.
               [ A "-I"
               ; Path dir
               ; A "-o"
               ; Target odocl_file
               ; Dep (Path.build odoc_file)
               ; Hidden_deps (Dep.Set.of_files (List.map ~f:Path.build odocs))
               ]
            @ iflags)
        in
        run_odoc
      in
      Log.info
        [ Pp.textf "About to add rule for %s" (Path.Build.to_string odocl_file)
        ];
      add_rule sctx action)

let packages_of_local_dir sctx =
  let ctx = Super_context.context sctx in
  let* findlib =
    Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
  in
  let* all_packages = Findlib.all_packages findlib in
  let findlib_paths_unsorted =
    List.map ~f:(fun x -> Path.to_string x ^ "/") ctx.Context.findlib_paths
  in
  let findlib_paths =
    List.sort
      ~compare:(fun p1 p2 -> Int.compare (String.length p1) (String.length p2))
      findlib_paths_unsorted
  in
  let map =
    List.fold_left all_packages ~init:String.Map.empty ~f:(fun map entry ->
        match entry with
        | Dune_package.Entry.Library l ->
          let obj_dir =
            Dune_package.Lib.info l |> Lib_info.obj_dir |> Obj_dir.dir
          in
          let obj_dir_str = Path.to_string obj_dir in
          let prefix =
            List.find
              ~f:(fun prefix -> String.starts_with ~prefix obj_dir_str)
              findlib_paths
            |> Option.value_exn
          in
          let local =
            String.sub obj_dir_str ~pos:(String.length prefix)
              ~len:(String.length obj_dir_str - String.length prefix)
          in
          let local = String.split ~on:'/' local |> List.hd in
          let name = Dune_package.Lib.info l |> Lib_info.name in
          String.Map.update map local ~f:(function
            | Some (l, ods) -> Some (name :: l, Path.Set.add ods obj_dir)
            | None -> Some ([ name ], Path.Set.add Path.Set.empty obj_dir))
        | _ -> map)
  in
  Memo.return map

let _external_rules2 sctx dir =
  let ctx = Super_context.context sctx in
  let* map = packages_of_local_dir sctx in
  let* findlib =
    Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
  in
  let packages_opt = String.Map.find map dir in
  match packages_opt with
  | None -> Memo.return ()
  | Some (pkgs, obj_dirs) ->
    (* Log.info [Pp.textf "XXX Build rules for packages %s" (String.concat ~sep:"," (List.map pkgs ~f:(Lib_name.to_string))) ];
       Log.info [Pp.textf "XXX Build rules for dirs %s" (String.concat ~sep:"," (List.map ~f:(Path.to_string) (Path.Set.to_list obj_dirs)))]; *)
    let* packages =
      Memo.List.filter_map pkgs ~f:(fun pkg ->
          let+ entry = Findlib.find findlib pkg in
          match entry with
          | Ok (Dune_package.Entry.Library l) ->
            (* Log.info [Pp.textf "XXX Build rules for library: %s" (Lib_name.to_string (Dune_package.Lib.info l |> Lib_info.name))]; *)
            Some l
          | _ -> None)
    in

    let cmas =
      List.fold_left ~init:[] packages ~f:(fun acc pkg ->
          let info = Dune_package.Lib.info pkg in
          let obj_dir = Lib_info.obj_dir info |> Obj_dir.dir in
          let cmas = Mode.Dict.get (Lib_info.archives info) Byte in
          let cmas = List.map cmas ~f:(fun cma -> (obj_dir, cma)) in
          acc @ cmas)
    in

    let* () =
      Memo.List.iter cmas ~f:(fun (obj_dir, cma) ->
          obj_info_of_cma sctx obj_dir cma)
    in

    let package_deps pkg =
      let info = Dune_package.Lib.info pkg in
      let deps = Lib_info.requires info in
      Memo.List.filter_map
        ~f:(fun dep ->
          match dep with
          | Lib_dep.Direct (_, l) -> (
            let+ l' = Findlib.find findlib l in
            match l' with
            | Ok (Dune_package.Entry.Library l') ->
              if
                List.exists packages ~f:(fun p ->
                    Dune_package.Lib.info p |> Lib_info.name = l)
              then None
              else
                (* Avoid self-dependencies *)
                Some
                  (l, Obj_dir.dir (Lib_info.obj_dir (Dune_package.Lib.info l')))
            | _ -> None)
          | Re_export _ -> Memo.return None
          | Select _ -> Memo.return None)
        deps
    in
    let* deps =
      Memo.List.fold_left packages ~init:Path.Set.empty ~f:(fun set pkg ->
          let+ deps = package_deps pkg in
          List.fold_left ~init:set deps ~f:(fun set (_name, dir) ->
              Path.Set.add set dir))
    in
    let obj_dirs_list = Path.Set.to_list obj_dirs in
    Memo.List.iter obj_dirs_list ~f:(fun obj_dir ->
        let* () = deps_of_findlib_dir sctx obj_dir in
        let* odocs =
          compile_rules_of_findlib_dir sctx obj_dir
            (ctx.stdlib_dir :: Path.Set.to_list deps)
        in
        let* _odocls =
          link_rules_of_findlib_dir sctx odocs obj_dir
            (ctx.stdlib_dir :: Path.Set.to_list deps)
        in
        OdocDep.setup_deps ctx (Findlib obj_dir)
          (Path.Set.of_list_map ~f:Path.build odocs))

let _external_rules sctx =
  (* Log.info [Pp.textf "here we are..."]; *)
  let ctx = Super_context.context sctx in
  let* findlib =
    Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
  in
  let* all_packages = Findlib.all_packages findlib in
  let stdlib_dir = ctx.stdlib_dir in
  (* let* () = deps_of_findlib_dir sctx stdlib_dir in *)
  (* let* odocs = compile_rules_of_findlib_dir sctx stdlib_dir [] in *)
  (* let* () = OdocDep.setup_deps ctx (Findlib stdlib_dir) (Path.Set.of_list_map ~f:Path.build odocs) in *)
  let paths_handled = ref [ stdlib_dir ] in
  let* () =
    Memo.parallel_iter
      ~f:(fun entry ->
        List.iter
          ~f:(fun p ->
            Log.info [ Pp.textf "Findlib path: %s" (Path.to_string p) ])
          ctx.findlib_paths;
        (* Log.info [Pp.textf "Got entry: %s" (Dune_package.Entry.name entry |> Lib_name.to_string)]; *)
        match entry with
        | Dune_package.Entry.Library l -> (
          (* let modules_opt = Dune_package.Lib.modules l in *)
          (* match modules_opt with
             | Some modules ->
               let ms = Modules.fold_no_vlib modules ~init:[] ~f:(fun m l -> m::l) in
               List.iter ~f:(fun m ->
                 Log.info [Pp.textf "Got module: %s" (Module.obj_name m |> Module_name.Unique.artifact_filename ~ext:"moo" )]) ms;
               Memo.return ()
             | None -> ( *)
          let info = Dune_package.Lib.info l in
          let deps = Lib_info.requires info in
          let od = Lib_info.obj_dir info in
          let cma = Mode.Dict.get (Lib_info.archives info) Byte in
          match cma with
          | [] -> Memo.return ()
          | cmas ->
            let _cma = List.hd cmas in
            (* Log.info [Pp.textf "obj_dir: %s" (Obj_dir.dir od |> Path.to_string)]; *)
            let* _package_deps =
              Memo.List.filter_map
                ~f:(fun dep ->
                  match dep with
                  | Lib_dep.Direct (_, l) -> (
                    let+ l' = Findlib.find findlib l in
                    match l' with
                    | Ok (Dune_package.Entry.Library l') ->
                      Some
                        (Obj_dir.dir
                           (Lib_info.obj_dir (Dune_package.Lib.info l')))
                    | _ -> None)
                  | Re_export _ -> Memo.return None
                  | Select _ -> Memo.return None)
                deps
            in
            let dir = Obj_dir.dir od in
            (* let* () = obj_info_of_cma sctx dir cma in *)
            let* _odocs =
              if List.mem ~equal:Path.equal !paths_handled dir then
                Memo.return []
              else (
                paths_handled := dir :: !paths_handled;
                let* () = deps_of_findlib_dir sctx (Obj_dir.dir od) in
                (*compile_rules_of_findlib_dir sctx dir (dir :: stdlib_dir :: package_deps) *)
                Memo.return [])
            in
            (* OdocDep.setup_deps ctx (Findlib dir) (Path.Set.of_list_map ~f:Path.build odocs) *)
            Memo.return ())
        | _ ->
          Log.info [ Pp.textf "Not a library" ];
          Memo.return ())
      all_packages
  in
  Memo.return ()

(* let action =
   let open Action_builder.O in *)

(*
  let* installed_libs = Lib.DB.installed ctx in
  let open Memo.O in
  Lib.DB.find installed_libs (Lib_name.of_string pkg) >>= function
  | None -> Memo.return ()
  | Some pkg ->
    let info = Lib.info pkg in
    let lib_name = Lib_name.to_string (Lib.name pkg) in
    let od = Lib_info.obj_dir info in
    Log.info [Pp.textf "lib_name: %s obj_dir: %s" lib_name (Obj_dir.dir od |> Path.to_string)];
    let package = Lib_info.package info in

    Memo.return ()
*)

let gen_rules sctx ~dir:_ rest =
  Log.info [ Pp.textf "rules for: %s" (String.concat ~sep:"/" rest) ];
  match rest with
  | [] ->
    Memo.return
      (Build_config.Rules
         { rules = Memo.return Rules.empty
         ; build_dir_only_sub_dirs = Subdir_set.All
         ; directory_targets = Path.Build.Map.empty
         })
  | [ "_html" ] ->
    has_rules (setup_css_rule sctx >>> setup_toplevel_index_rule sctx)
  | [ "_mlds"; "docs" ] ->
    let* doc = StdMlds.packages (Super_context.context sctx) in
    has_rules (mld_rules sctx [ doc ])
  | [ "_mlds"; "docs"; "p" ] ->
    let* packages = Only_packages.get () in
    let packages = Package.Name.Map.to_list packages in
    let rules =
      Memo.parallel_iter packages ~f:(fun (_, pkg) ->
          let* r1, rs = StdMlds.package sctx ~pkg in
          mld_rules sctx (r1 :: rs))
    in
    has_rules rules
  | [ "_mlds"; "docs"; "l" ] ->
    let ctx = Super_context.context sctx in
    let* findlib =
      Findlib.create ~paths:ctx.findlib_paths ~lib_config:ctx.lib_config
    in
    let* all_packages = Findlib.all_packages findlib in
    let package_names =
      List.filter_map all_packages ~f:(fun entry ->
          match entry with
          | Dune_package.Entry.Library l ->
            let info = Dune_package.Lib.info l in
            let name = Lib_info.name info in
            let package_name =
              Package.Name.of_string (Lib_name.to_string name)
            in
            Some package_name
          | _ -> None)
    in
    let rules =
      Memo.parallel_iter package_names ~f:(fun p ->
          external_pkg_mld_rules sctx p)
    in
    has_rules rules
  | [ "_mlds"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
        let* _mlds, rules = package_mlds sctx ~pkg in
        Rules.produce rules)
  | [ "_mlds" ] ->
    let* doc = StdMlds.docs (Super_context.context sctx) in
    has_rules (mld_rules sctx [ doc ])
  | [ "_odoc"; "docs" ] ->
    let* doc = StdMlds.packages (Super_context.context sctx) in
    has_rules (odoc_rules sctx [ doc ])
  | [ "_odoc"; "docs"; "packages" ] ->
    let* packages = Only_packages.get () in
    let packages = Package.Name.Map.to_list packages in
    let rules =
      Memo.parallel_iter packages ~f:(fun (_, pkg) ->
          let* r1, rs = StdMlds.package sctx ~pkg in
          odoc_rules sctx (r1 :: rs))
    in
    has_rules rules
  (* | [ "_external"] ->
     Log.info [Pp.textf "in _external rules"];
      has_rules (_external_rules sctx) *)
  | [ "_odoc"; pkg ] ->
    with_package pkg ~f:(fun pkg ->
        let* _mlds, rules = package_mlds sctx ~pkg in
        Rules.produce rules)
  | [ "_odoc" ] ->
    let* doc = StdMlds.docs (Super_context.context sctx) in
    has_rules (odoc_rules sctx [ doc ])
  | [ "_odoc"; "pkg"; pkg ] ->
    with_package pkg ~f:(fun pkg -> setup_package_odoc_rules sctx ~pkg)
  | [ "_external"; pkg ] -> has_rules (_external_rules2 sctx pkg)
  | [ "_odocls"; lib_unique_name_or_pkg ] ->
    has_rules
      ((* TODO we can be a better with the error handling in the case where
          lib_unique_name_or_pkg is neither a valid pkg or lnu *)
       let ctx = Super_context.context sctx in
       let* lib, lib_db = Scope_key.of_string ctx lib_unique_name_or_pkg in
       let setup_pkg_odocl_rules pkg =
         let* pkg_libs = load_all_odoc_rules_pkg ctx ~pkg in
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
         let* pkg_libs =
           load_all_odoc_rules_pkg (Super_context.context sctx) ~pkg
         in
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
  | _ -> Memo.return Build_config.Redirect_to_parent
