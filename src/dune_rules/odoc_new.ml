open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

let lib_equal l1 l2 = Lib.compare l1 l2 |> Ordering.is_eq

let find_project_by_key =
  let memo =
    let make_map projects =
      Dune_project.File_key.Map.of_list_map_exn projects ~f:(fun project ->
        Dune_project.file_key project, project)
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
    let* { projects; _ } = Dune_load.load () in
    let+ map = Memo.exec memo projects in
    Dune_project.File_key.Map.find_exn map key
;;

module Scope_key : sig
  val of_string : Context.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      Lib_name.parse_string_exn (Loc.none, s), public_libs
    | Some (lib, key) ->
      let+ scope =
        let key = Dune_project.File_key.of_string key in
        find_project_by_key key >>= Scope.DB.find_by_project context
      in
      Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope
  ;;

  let to_string lib project =
    let key = Dune_project.file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib) (Dune_project.File_key.to_string key)
  ;;
end

let is_public lib =
  let lib = Lib.Local.to_lib lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private -> false
  | Installed -> true
  | Public _ -> true
  | Private (_project, _) -> false
;;

let lib_unique_name lib =
  let lib = Lib.Local.to_lib lib in
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project
;;

module Paths = struct
  let odoc_support_dirname = "docs/odoc.support"

  let root (context : Context.t) _all =
    let sub = "_doc_new" in
    Path.Build.relative context.Context.build_dir sub
  ;;

  let local_path_of_findlib_path ctx obj_dir =
    List.find_map ctx.Context.findlib_paths ~f:(fun p ->
      if Path.is_descendant ~of_:p obj_dir
      then Some (Path.reach obj_dir ~from:p)
      else None)
    |> Option.value_exn
  ;;

  let html_root ctx all = root ctx all ++ "html"
  let odoc_support ctx all = html_root ctx all ++ odoc_support_dirname
end

module Index = struct
  type ty =
    | LocalPackage of Package.Name.t
    | PrivateLib of string
    | External of string
    | Subdir of string

  type t = ty list

  let ty_name = function
    | LocalPackage pkg -> Package.Name.to_string pkg
    | PrivateLib lnu -> lnu
    | External s -> s
    | Subdir str -> str
  ;;

  let name : t -> string = function
    | [] -> "toplevel"
    | xs -> String.concat ~sep:"." (List.map xs ~f:ty_name)
  ;;

  let rec is_external = function
    | [] -> false
    | External _ :: _ -> true
    | Subdir _ :: xs -> is_external xs
    | PrivateLib _ :: _ -> false
    | LocalPackage _ :: _ -> false

  let ty_to_dyn x =
    let open Dyn in
    match x with
    | LocalPackage pkg -> variant "LocalPackage" [ Package.Name.to_dyn pkg ]
    | PrivateLib lnu -> variant "PrivateLib" [ String lnu ]
    | External str -> variant "External" [ String str ]
    | Subdir str -> variant "LocalSubLib" [ String str ]
  ;;

  let to_dyn x = Dyn.list ty_to_dyn x
  let to_string x = Dyn.to_string (to_dyn x)
  let compare x y = Dyn.compare (to_dyn x) (to_dyn y)
  let compare_ty x y = Dyn.compare (ty_to_dyn x) (ty_to_dyn y)

  let top_dir_of_external_fallback = function
    | s ->
      assert(not (String.contains s '/'));
      String.split ~on:'/' s |> List.hd
  ;;

  let subdir = function
    | LocalPackage pkg -> Package.Name.to_string pkg
    | External str -> str
    | PrivateLib lnu -> lnu
    | Subdir str -> str

  let obj_dir ctx all : t -> Path.Build.t =
    let root = Paths.root ctx all ++ "index" in
    List.fold_right ~f:(fun x acc -> acc ++ subdir x) ~init:root
  ;;

  let html_dir ctx all (m : t) =
    let init = Paths.html_root ctx all ++ "docs" in
    List.fold_right ~f:(fun x acc -> acc ++ subdir x) ~init m
  ;;

  let odoc_dir ctx all (m : t) =
    let init = Paths.root ctx all ++ "odoc" in
    List.fold_right ~f:(fun x acc -> acc ++ subdir x) ~init m

  let mld_name_ty : ty -> string = subdir

  let mld_name : t -> string = function
    | [] -> "docs"
    | x :: _ -> mld_name_ty x
  ;;

  let mld_filename index = mld_name index ^ ".mld"
  let mld_path ctx all index = obj_dir ctx all index ++ mld_filename index
end

module IndexSet = Set.Make (Index) (Map.Make (Index))

module Target = struct
  type module_source = Index.t * Path.Build.t * bool
  type mld_source = Index.t * Path.Build.t

  type source =
    | Module : (Index.t * Path.t * bool) -> source
    | Mld : Index.t * Path.t -> source

  (** The target tells us the locations of the artifacts - input cmtis/mlds,
      odoc files, odocl files and html file. It is parameterised over a type
      related to where the input files will be found. *)
  type _ t =
    | Lib : Lib.Local.t -> module_source t
        (** [Lib lib] represents a library in the source tree. The only inputs
            associated with libraries are modules as mld files are always
            associated with the whole package *)
    | Pkg : Package.Name.t -> mld_source t
        (** [Pkg pkg_name] represents a package defined in the source tree.
            There are no modules, only mld files associated with the package. *)
    | ExtLib : string -> source t
        (** An external library. We don't distinguish between libraries and
            packages for external libs as all the files (ie, odoc files from
            both modules and mlds) end up in the same directory. *)
    | Index : Index.t -> unit t
        (** The indices for each type of tree go into separate dirs. *)

end

let add_rule sctx =
  let dir = (Super_context.context sctx).build_dir in
  Super_context.add_rule sctx ~dir
;;

(* Returns a map from a 'local dir' - defined as a directory in
   your switch's lib dir, e.g. 'ocaml' or 'dune' - to a map from
   subdir to a map from library name to Dune_package.Lib.t. For
   example, on 4.14.1, looking up "ocaml" in the map gives a map
   containing the following:

   [ ocaml ->
     [ bigarray -> <lib>, bytes -> <lib>, dynlink -> <lib> ... ]
   [ ocaml/compiler-libs ->
     [ compiler-libs -> <lib>, compiler-libs.bytecomp -> <lib> ... ]
*)
let libs_of_local_dir_def =
  let f (ctx : Context.t) =
    let* findlib = Findlib.create ctx.name in
    let* all_packages = Findlib.all_packages findlib in
    let* db = Scope.DB.public_libs ctx in
    let map =
      List.fold_left
        all_packages
        ~init:(Memo.return String.Map.empty)
        ~f:(fun map entry ->
          let* map = map in
          match entry with
          | Dune_package.Entry.Library l ->
            let obj_dir = Dune_package.Lib.info l |> Lib_info.obj_dir |> Obj_dir.dir in
            let local = Paths.local_path_of_findlib_path ctx obj_dir in
            let toplocal = String.split local ~on:'/' |> List.hd in
            let name = Dune_package.Lib.info l |> Lib_info.name in
            let+ lib_opt = Lib.DB.find db name in
            (match lib_opt with
             | None -> map
             | Some lib ->
               let update_fn = function
                 | Some libs ->
                   Some
                     (String.Map.update libs local ~f:(function
                       | Some libs -> Some (Lib_name.Map.add_exn libs name (l, lib))
                       | None -> Some (Lib_name.Map.singleton name (l, lib))))
                 | None ->
                   Some
                     (String.Map.singleton local (Lib_name.Map.singleton name (l, lib)))
               in
               String.Map.update map toplocal ~f:update_fn)
          | _ -> Memo.return map)
    in
    map
  in
  let module Input = Context in
  Memo.create "libs_of_local_dir" ~input:(module Input) f
;;

let libs_of_local_dir ctx = Memo.exec libs_of_local_dir_def ctx

(* The following function gives information about how we can document a
   particular external directory. Call with the a top-level path, e.g. "base".
   If 'base' is a dune package, where we know all the info (e.g. a wrapped
   library), and in addition all other libraries under the [base] path are the
   same, this will return some extracted info from all of the libraries in the
   package. If not, this will return a fallback with everything found under
   the [base] path.

   Note that we can't just call [Findlib.find_root_package] as that won't
   work for paths like [ocaml] *)

exception Fallback

type dune_with_modules =
  { local_dir : string
  ; lib : Lib.t
  ; modules : Modules.t
  ; entry_modules : Module_name.t list
  }

type fallback = { libs : (Dune_package.Lib.t * Lib.t) Lib_name.Map.t Import.String.Map.t }

type local_dir_type =
  | Nothing
  | DuneWithModules of (Package.Name.t * dune_with_modules list)
  | Fallback of fallback

let classify_local_dir_memo =
  let run (ctx, local_dir) =
    (match String.index_opt local_dir '/' with
     | Some _ -> assert false
     | None -> ());
    let pkg = local_dir in
    let* map = libs_of_local_dir ctx in
    match String.Map.find map pkg with
    | None ->
      Log.info [ Pp.textf "classify_local_dir: No lib at this path: %s" local_dir ];
      Memo.return Nothing
    | Some libs ->
      let* public_libs = Lib.DB.installed ctx in
      (try
         let f local_dir libs acc =
           match Lib_name.Map.values libs with
           | [ (lib, _) ] ->
             let info = Dune_package.Lib.info lib in
             let mods_opt = Lib_info.modules (Dune_package.Lib.info lib) in
             (match mods_opt, Lib_info.entry_modules info with
              | External (Some modules), External (Ok entry_modules) ->
                (local_dir, lib, modules, entry_modules) :: acc
              | _ -> raise Fallback)
           | _ -> raise Fallback
         in
         let ms = String.Map.foldi libs ~f ~init:[] in
         let* ms =
           Memo.List.map
             ms
             ~f:(fun (local_dir, dune_package_lib, modules, entry_modules) ->
               let info = Dune_package.Lib.info dune_package_lib in
               (* let requires = Resolve.return requires in *)
               let* resolved_lib =
                 Lib.DB.resolve public_libs (Loc.none, Lib_info.name info)
               in
               let+ lib = Resolve.read_memo resolved_lib in
               let package = Lib_info.package info |> Option.value_exn in
               package, { local_dir; lib; modules; entry_modules })
         in
         let pkg =
           List.fold_left ~init:None ms ~f:(fun acc m ->
             match acc with
             | None -> Some (fst m)
             | Some p -> if p <> fst m then raise Fallback else acc)
           |> Option.value_exn
         in
         Memo.return (DuneWithModules (pkg, List.map ~f:snd ms))
       with
       | Fallback -> Memo.return (Fallback { libs }))
  in
  let module Input = struct
    type t = Context.t * string

    let equal (c1, s1) (c2, s2) = Context.equal c1 c2 && String.equal s1 s2
    let hash (c, s) = Poly.hash (Context.hash c, String.hash s)
    let to_dyn = Dyn.pair Context.to_dyn Dyn.string
  end
  in
  Memo.create "libs_and_packages" ~input:(module Input) run
;;

let classify_local_dir ctx dir = Memo.exec classify_local_dir_memo (ctx, dir)

let index_of_external_lib ctx lib =
  let open Index in
  let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.dir in
  let local = Paths.local_path_of_findlib_path ctx obj_dir |> String.split ~on:'/' in
  let init = [External (List.hd local)]in
  List.fold_left ~init ~f:(fun acc s -> Subdir s :: acc) (List.tl local) 
;;

let index_of_local_dir local_dir =
  let open Index in
  match String.split ~on:'/' local_dir with
  | [] -> assert false
  | x :: xs ->
    List.fold_left
      ~f:(fun acc s -> Subdir s :: acc)
      ~init:[ External x ]
      xs
;;

let index_of_local_lib (lib : Lib.Local.t) =
  let open Index in
  let info = Lib.Local.info lib in
  let package = Lib_info.package info in
  match package with
  | Some _pkg ->
    (match Lib_name.analyze (Lib.name (lib :> Lib.t)) with
     | Public (pkg, rest) ->
       Log.info
         [ Pp.textf
             "local: pkg=%s rest=[%s]"
             (Package.Name.to_string pkg)
             (String.concat ~sep:"," rest)
         ];
       List.fold_left
         ~f:(fun acc s -> Subdir s :: acc)
         rest
         ~init:[ LocalPackage pkg ]
     | Private (_, _) -> [ PrivateLib (lib_unique_name lib) ])
  | None -> [ PrivateLib (lib_unique_name lib) ]
;;

module Valid = struct
  (* These functions return a whitelist of libraries and packages that
     should be documented. There is one single function that performs this
     task because there needs to be an exact correspondance at various points
     in the process - e.g. the indexes need to know exactly which libraries will
     be documented and where. *)
  let valid_libs_and_packages =
    let run (ctx, all, projects) =
      let* mask = Only_packages.get_mask () in
      let mask = Option.map ~f:Package.Name.Map.keys mask in
      let* libs_and_pkgs =
        Scope.DB.with_all ctx ~f:(fun find ->
          Memo.List.fold_left
            ~init:([], [])
            ~f:(fun (libs_acc, pkg_acc) proj ->
              let* vendored = Source_tree.is_vendored (Dune_project.root proj) in
              if vendored
              then Memo.return (libs_acc, pkg_acc)
              else (
                let scope = find proj in
                let lib_db = Scope.libs scope in
                let+ libs = Lib.DB.all lib_db in
                let libs =
                  match mask with
                  | None -> libs
                  | Some mask ->
                    Lib.Set.filter
                      ~f:(fun lib ->
                        let info = Lib.info lib in
                        match Lib_info.package info with
                        | Some p -> List.mem ~equal:Package.Name.equal mask p
                        | None -> false)
                      libs
                in
                let libs_acc = (proj, lib_db, libs) :: libs_acc in
                let pkgs =
                  let proj_pkgs = Dune_project.packages proj |> Package.Name.Map.keys in
                  match mask with
                  | Some m ->
                    List.filter ~f:(List.mem ~equal:Package.Name.equal m) proj_pkgs
                  | None -> proj_pkgs
                in
                let pkg_acc = pkgs @ pkg_acc in
                libs_acc, pkg_acc))
            projects)
      in
      let* libs, packages = libs_and_pkgs in
      let* stdlib = stdlib_lib ctx in
      let+ libs_list =
        Memo.all
          (List.map libs ~f:(fun (_, _lib_db, libs) ->
             Lib.Set.fold ~init:(Memo.return []) libs ~f:(fun lib acc ->
               let* acc = acc in
               let* libs = Lib.closure (lib :: Option.to_list stdlib) ~linking:false in
               let+ libs = Resolve.read_memo libs in
               libs :: acc)))
      in
      let libs_list =
        List.concat (List.concat libs_list) |> Lib.Set.of_list |> Lib.Set.to_list
      in
      let libs_list =
        List.filter libs_list ~f:(fun lib ->
          let is_impl = Lib.info lib |> Lib_info.implements |> Option.is_some in
          not is_impl)
      in
      let libs_list =
        if all
        then libs_list
        else
          List.filter libs_list ~f:(fun lib ->
            match Lib.Local.of_lib lib with
            | None -> false
            | Some l -> is_public l)
      in
      libs_list, packages
    in
    let module Input = struct
      type t = Context.t * bool * Dune_project.t list

      let equal (c1, b1, ps1) (c2, b2, ps2) =
        Context.equal c1 c2 && List.equal Dune_project.equal ps1 ps2 && b1 = b2
      ;;

      let hash (c, b, ps) = Poly.hash (Context.hash c, b, List.hash Dune_project.hash ps)
      let to_dyn _ = Dyn.Opaque
    end
    in
    Memo.create "libs_and_packages" ~input:(module Input) run
  ;;

  let get ctx all =
    let* { projects; _ } = Dune_load.load () in
    Memo.exec valid_libs_and_packages (ctx, all, projects)
  ;;

  let filter_libs ctx all libs =
    let+ valid_libs, _ = get ctx all in
    List.filter libs ~f:(fun l -> List.mem valid_libs l ~equal:lib_equal)
  ;;

  let filter_dwms ctx all dwms =
    let+ valid_libs, _ = get ctx all in
    List.filter dwms ~f:(fun dwm -> List.mem valid_libs dwm.lib ~equal:lib_equal)
  ;;

  let filter_fallback_libs ctx all libs =
    let+ valid_libs, _ = get ctx all in
    Import.String.Map.filter_map libs ~f:(fun libs ->
      let filtered =
        Lib_name.Map.filter libs ~f:(fun (_, l) -> List.mem valid_libs l ~equal:lib_equal)
      in
      if Lib_name.Map.cardinal filtered > 0 then Some filtered else None)
  ;;

  type categorized =
    { packages : Package.Name.Set.t
    ; local : Lib.Local.t Lib_name.Map.t
    ; localprivate : Lib.Local.t Import.String.Map.t
    ; external_dirs : local_dir_type Import.String.Map.t
    }

  let empty_categorized =
    { packages = Package.Name.Set.empty
    ; local = Lib_name.Map.empty
    ; localprivate = Import.String.Map.empty
    ; external_dirs = Import.String.Map.empty
    }
  ;;

  let get_categorized_memo =
    let run (ctx, all) =
      let* libs, packages = get ctx all in
      let init =
        Memo.return { empty_categorized with packages = Package.Name.Set.of_list packages }
      in
      List.fold_left libs ~init ~f:(fun cats lib ->
        let* cats = cats in
        match Lib.Local.of_lib lib with
        | Some llib ->
          (match Lib_info.package (Lib.Local.info llib) with
          | Some _pkg ->
            let local =
              match Lib_name.Map.add cats.local (Lib.name (llib :> Lib.t)) llib with
              | Ok l -> l
              | Error _ ->
                Log.info
                  [ Pp.textf
                      "Error adding local library %s to categorized map"
                      (Lib.name (llib :> Lib.t) |> Lib_name.to_string)
                  ];
                cats.local
            in
            Memo.return { cats with local }
          | None ->
            let lnu = lib_unique_name llib in
            let localprivate =
              match Import.String.Map.add cats.localprivate lnu llib with
              | Ok l -> l
              | Error _ ->
                Log.info
                  [ Pp.textf
                      "Error adding local private library %s to categorized map"
                      (Lib.name (llib :> Lib.t) |> Lib_name.to_string)
                  ];
                cats.localprivate
            in
            Memo.return { cats with localprivate })
        | None ->
          let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.dir in
          let local = Paths.local_path_of_findlib_path ctx obj_dir in
          let top_dir = local |> String.split ~on:'/' |> List.hd in
          if String.Map.mem cats.external_dirs top_dir
          then Memo.return cats
          else
            let* c = classify_local_dir ctx top_dir in
            let external_dirs =
              match Import.String.Map.add cats.external_dirs top_dir c with
              | Ok l -> l
              | Error _ ->
                Log.info
                  [ Pp.textf "Error adding external dir %s to categorized map" top_dir ];
                cats.external_dirs
            in
            Memo.return { cats with external_dirs })
    in
    let module Input = struct
      type t = Context.t * bool

      let equal (c1, b1) (c2, b2) =
        Context.equal c1 c2 && b1 = b2
      ;;

      let hash (c, b) = Poly.hash (Context.hash c, b)
      let to_dyn _ = Dyn.Opaque
    end
    in
    Memo.create "categorized" ~input:(module Input) run

    let get_categorized ctx all = Memo.exec get_categorized_memo (ctx, all)
  ;;
end

module Dep : sig
  (** [html_alias ctx target] returns the alias that depends on all html targets
      produced by odoc for [target] *)
  val html_alias : Path.Build.t -> Alias.t

  (** [deps ctx pkg libraries] returns all odoc dependencies of [libraries]. If
      [libraries] are all part of a package [pkg], then the odoc dependencies of
      the package are also returned*)
  val deps
    :  Context.t
    -> bool
    -> Lib.t list
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx target odocs] Adds [odocs] as dependencies for [target].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> bool -> Index.t -> Path.Set.t -> unit Memo.t
end = struct
  let html_alias dir =
    Alias.make Alias0.doc_new ~dir  ;;

  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx all valid_libs pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p ->
          let index = [ Index.LocalPackage p ] in
           Dep.Set.singleton (Dep.alias (alias ~dir:(Index.odoc_dir ctx all index)))
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         if not (List.mem ~equal:lib_equal valid_libs lib)
         then acc
         else (
           match Lib.Local.of_lib lib with
           | None ->
            let index = index_of_external_lib ctx lib in
             let dir = Index.odoc_dir ctx all index in
             let alias = alias ~dir in
             Dep.Set.add acc (Dep.alias alias)
           | Some lib ->
              let index = index_of_local_lib lib in 

            let dir = Index.odoc_dir ctx all index in
             let alias = alias ~dir in
             Dep.Set.add acc (Dep.alias alias))))
  ;;

  let alias ctx all index = alias ~dir:(Index.odoc_dir ctx all index)

  let setup_deps ctx all m files =
    Rules.Produce.Alias.add_deps (alias ctx all m) (Action_builder.path_set files)
  ;;
end

module MiniArtifact : sig
  type artifact_ty =
    | Module of bool
    | Mld

  type t

  val odoc_file : t -> Path.Build.t
  val odocl_file : t -> Path.Build.t
  val html_file : t -> Path.Build.t
  val html_dir : t -> Path.Build.t
  val source_file : t -> Path.t
  val is_visible : t -> bool
  val artifact_ty : t -> artifact_ty
  val reference : t -> string
  val module_name : t -> Module_name.t option

  val make : 'a. Context.t -> bool -> 'a Target.t -> 'a -> t
end = struct
  type artifact_ty =
    | Module of bool
    | Mld

  type t =
    { source : Path.t
    ; odoc : Path.Build.t
    ; html_dir : Path.Build.t
    ; html_file : Path.Build.t
    ; ty : artifact_ty
    }

  let odoc_file v = v.odoc
  let odocl_file v = Path.Build.set_extension v.odoc ~ext:".odocl"
  let source_file v = v.source
  let html_file v = v.html_file
  let html_dir v = v.html_dir

  let is_visible v =
    match v.ty with
    | Module x -> x
    | Mld -> true
  ;;

  let artifact_ty v = v.ty

  let reference v =
    match v.ty with
    | Mld ->
      let basename = Path.basename v.source |> Filename.chop_extension in
      sprintf "page-\"%s\"" basename
    | Module _ ->
      let basename =
        Path.basename v.source |> Filename.chop_extension |> Stdune.String.capitalize
      in
      sprintf "module-%s" basename
  ;;

  let module_name v =
    match v.ty with
    | Module _ ->
      let basename =
        Path.basename v.source |> Filename.chop_extension |> Stdune.String.capitalize
      in
      Some (Module_name.of_string_allow_invalid (Loc.none, basename))
    | _ -> None
  ;;

  let v ~source ~odoc ~html_dir ~html_file ~ty = { source; odoc; html_dir; html_file; ty }

  let make : type a. Context.t -> bool -> a Target.t -> a -> t =
    fun ctx all target source ->
    let module_files index source ty =
      let basename =
        Path.basename source |> Filename.chop_extension |> Stdune.String.uncapitalize
      in
      let odoc = Index.odoc_dir ctx all index ++ (basename ^ ".odoc") in
      Log.info [ Pp.textf "odoc=%s" (Path.Build.to_string odoc)];
      let html_dir = Index.html_dir ctx all index ++ Stdune.String.capitalize basename in
      let html = html_dir ++ "index.html" in
      v ~source ~odoc ~html_dir ~html_file:html ~ty
    in
    let mld_files index source ty is_index =
      let basename = Path.basename source |> Filename.chop_extension in
      let odoc = (if is_index then Index.obj_dir ctx all index else Index.odoc_dir ctx all index) ++ ("page-" ^ basename ^ ".odoc") in
      let html_dir = Index.html_dir ctx all index in
      let html = html_dir ++ if is_index then "index.html" else sprintf "%s.html" basename in
      Log.info [ Pp.textf "odoc=%s" (Path.Build.to_string odoc)];
      v ~source ~odoc ~html_dir ~html_file:html ~ty
    in
    match target with
    | Lib _ ->
      let index, path, visibility = source in
      module_files index (Path.build path) (Module visibility)
    | Pkg _ ->
      let index, path = source in
      mld_files index (Path.build path) Mld false
    | ExtLib _ ->
      (match (source : Target.source) with
       | Target.Module (index, path, visibility) ->
         module_files index path (Module visibility)
       | Target.Mld (index, path) -> mld_files index path Mld false)
    | Index index ->
      let filename = Index.mld_filename index in
      let dir = Index.obj_dir ctx all index in
      let source = Path.build (dir ++ filename) in
      mld_files index source Mld true
  ;;

end

let odoc_base_flags sctx quiet build_dir =
  let open Memo.O in
  let+ conf = Super_context.env_node sctx ~dir:build_dir >>= Env_node.odoc in
  match conf.Env_node.Odoc.warnings with
  | Fatal ->
    (* if quiet has been passed, we're running odoc on an external
       artifact (e.g. stdlib.cmti) - so no point in warn-error *)
    if quiet then Command.Args.S [] else A "--warn-error"
  | Nonfatal -> S []
;;

let odoc_program sctx dir =
  Super_context.resolve_program sctx ~dir "odoc" ~loc:None ~hint:"opam install odoc"
;;

let run_odoc sctx ~dir command ~quiet ~flags_for args =
  let build_dir = (Super_context.context sctx).build_dir in
  let open Memo.O in
  let* program = odoc_program sctx build_dir in
  let+ base_flags =
    match flags_for with
    | None -> Memo.return Command.Args.empty
    | Some path -> odoc_base_flags sctx quiet path
  in
  let deps = Action_builder.env_var "ODOC_SYNTAX" in
  let open Action_builder.With_targets.O in
  Action_builder.with_no_targets deps
  >>> Command.run ~dir program [ A command; base_flags; S args ]
;;

let parse_odoc_deps lines =
  let rec getdeps cur = function
    | x :: rest ->
      (match String.split ~on:' ' x with
       | [ m; hash ] -> getdeps ((Module_name.of_string m, hash) :: cur) rest
       | _ -> getdeps cur rest)
    | [] -> cur
  in
  getdeps [] lines
;;

let parent_args parent_opt =
  match parent_opt with
  | None -> []
  | Some mld ->
    let dir = MiniArtifact.odoc_file mld |> Path.Build.parent_exn in
    let reference = MiniArtifact.reference mld in
    let odoc_file =
      MiniArtifact.odoc_file mld
      |> Path.build
      |> Dune_engine.Dep.file
      |> Dune_engine.Dep.Set.singleton
    in
    Command.Args.
      [ A "-I"; Path (Path.build dir); A "--parent"; A reference; Hidden_deps odoc_file ]
;;

let odoc_include_flags ctx all pkg requires indices =
  Resolve.args
    (let open Resolve.O in
     let+ libs = requires in
     let paths =
       List.fold_left libs ~init:Path.Set.empty ~f:(fun paths lib ->
         Log.info
           [ Pp.textf "odoc_include_flags: lib=%s" (Lib.name lib |> Lib_name.to_string) ];
         match Lib.Local.of_lib lib with
         | None ->
          let index = index_of_external_lib ctx lib in
           Path.Set.add paths (Path.build (Index.odoc_dir ctx all index))
         | Some lib ->
          let index = index_of_local_lib lib in
           Path.Set.add paths (Path.build (Index.odoc_dir ctx all index)))
     in
     let paths =
       match pkg with
       | Some p -> Path.Set.add paths (Path.build (Index.odoc_dir ctx all [LocalPackage p]))
       | None -> paths
     in
     let paths =
       List.fold_left indices ~init:paths ~f:(fun p index ->
         let odoc_dir = MiniArtifact.odoc_file index |> Path.Build.parent_exn in
         Path.Set.add p (Path.build odoc_dir))
     in
     Command.Args.S
       (List.concat_map (Path.Set.to_list paths) ~f:(fun dir ->
          [ Command.Args.A "-I"; Path dir ])))
;;

let create_index_artifact ctx all index =
  MiniArtifact.make ctx all (Index index) ()
;;

let index_dep index =
  MiniArtifact.odoc_file index
  |> Path.build
  |> Dune_engine.Dep.file
  |> Dune_engine.Dep.Set.singleton
;;

let compile_module
  sctx
  all
  ~artifact:a
  ~quiet
  ~requires
  ~package
  ~module_deps
  ~parent_opt
  ~indices
  =
  let odoc_file = MiniArtifact.odoc_file a in
  let open Memo.O in
  let cmti = MiniArtifact.source_file a in
  let ctx = Super_context.context sctx in
  let iflags = Command.Args.memo (odoc_include_flags ctx all package requires indices) in
  let quiet_arg =
    if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
  in
  let* valid_libs, _ = Valid.get ctx all in
  let file_deps = Dep.deps ctx all valid_libs package requires in
  let parent_args = parent_args parent_opt in
  let+ () =
    let* action_with_targets =
      let doc_dir = Path.parent_exn (Path.build (MiniArtifact.odoc_file a)) in
      let+ run_odoc =
        run_odoc
          sctx
          ~dir:doc_dir
          "compile"
          ~flags_for:(Some odoc_file)
          ~quiet
          ([ Command.Args.A "-I"
           ; Path doc_dir
           ; iflags
           ; A "-o"
           ; Target odoc_file
           ; Dep cmti
           ]
           @ parent_args
           @ [ quiet_arg ])
      in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets module_deps
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  odoc_file
;;

let compile_requires libs =
  let+ requires = Memo.List.map ~f:Lib.requires libs in
  let requires = Resolve.all requires |> Resolve.map ~f:List.flatten in
  Resolve.map
    requires
    ~f:(List.filter ~f:(fun l -> not (List.mem libs l ~equal:lib_equal)))
;;

let link_requires libs = Lib.closure libs ~linking:false

let compile_mld sctx a ~parent_opt ~quiet ~is_index ~children =
  assert (MiniArtifact.artifact_ty a = MiniArtifact.Mld);
  let doc_dir = Path.Build.parent_exn (MiniArtifact.odoc_file a) in
  let odoc_file = MiniArtifact.odoc_file a in
  let odoc_input = MiniArtifact.source_file a in
  let parent_args =
    match parent_opt with
    | None -> []
    | _ -> parent_args parent_opt
  in
  let child_args =
    List.fold_left children ~init:[] ~f:(fun args child ->
      match MiniArtifact.artifact_ty child with
      | Module true | Mld -> "--child" :: MiniArtifact.reference child :: args
      | Module false -> args)
  in
  let child_args =
    if is_index && List.is_empty child_args then [ "--child"; "dummy" ] else child_args
  in
  let quiet_arg =
    if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
  in
  let* run_odoc =
    run_odoc
      sctx
      ~dir:(Path.build doc_dir)
      "compile"
      ~flags_for:(Some odoc_file)
      ~quiet
      (A "-o"
       :: Target odoc_file
       :: Dep odoc_input
       :: As child_args
       :: quiet_arg
       :: parent_args)
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file
;;

let link_odoc_rules
  sctx
  all
  (artifacts : MiniArtifact.t list)
  ~quiet
  ~package
  ~libs
  ~indices
  =
  let ctx = Super_context.context sctx in
  let* requires = link_requires libs in
  let* valid_libs, _ = Valid.get ctx all in
  let deps = Dep.deps ctx all valid_libs package requires in
  let index_deps =
    List.map ~f:(fun x -> Command.Args.Hidden_deps (index_dep x)) indices
  in
  let quiet_arg =
    if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
  in
  let open Memo.O in
  Memo.List.iter artifacts ~f:(fun a ->
    let* run_odoc =
      run_odoc
        sctx
        ~dir:(Path.parent_exn (Path.build (MiniArtifact.odocl_file a)))
        "link"
        ~quiet
        ~flags_for:(Some (MiniArtifact.odoc_file a))
        (index_deps
         @ [ odoc_include_flags ctx all package requires indices
           ; A "-o"
           ; Target (MiniArtifact.odocl_file a)
           ; Dep (Path.build (MiniArtifact.odoc_file a))
           ]
         @ [ quiet_arg ])
    in
    add_rule
      sctx
      (let open Action_builder.With_targets.O in
       Action_builder.with_no_targets deps >>> run_odoc))
;;

let html_generate sctx all (a : MiniArtifact.t) =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let odoc_support_path = Paths.odoc_support ctx all in
  let html_output = Paths.html_root ctx all in
  let support_relative =
    Path.reach (Path.build odoc_support_path) ~from:(Path.build html_output)
  in
  let* run_odoc =
    run_odoc
      sctx
      ~quiet:false
      ~dir:(Path.build html_output)
      "html-generate"
      ~flags_for:None
      [ A "-o"
      ; Path (Path.build html_output)
      ; A "--support-uri"
      ; A support_relative
      ; A "--theme-uri"
      ; A support_relative
      ; Dep (Path.build (MiniArtifact.odocl_file a))
      ]
  in
  let rule, result =
    match MiniArtifact.artifact_ty a with
    | Mld ->
      ( Action_builder.With_targets.add ~file_targets:[ MiniArtifact.html_file a ] run_odoc
      , None )
    | Module _ ->
      let dir = MiniArtifact.html_dir a in
      ( Action_builder.With_targets.add_directories ~directory_targets:[ dir ] run_odoc
      , Some dir )
  in
  let+ () = add_rule sctx rule in
  result
;;

let setup_css_rule sctx all =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let dir = Paths.odoc_support ctx all in
  let* run_odoc =
    let+ cmd =
      run_odoc
        sctx
        ~quiet:false
        ~dir:(Path.build ctx.build_dir)
        "support-files"
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    cmd |> Action_builder.With_targets.add_directories ~directory_targets:[ dir ]
  in
  add_rule sctx run_odoc
;;

let libs_of_pkg ctx ~pkg =
  let+ entries = Scope.DB.lib_entries_of_package ctx pkg in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map entries ~f:(fun (entry : Scope.DB.Lib_entry.t) ->
    match entry with
    | Library lib ->
      let is_impl =
        Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements |> Option.is_some
      in
      Option.some_if (not is_impl) lib
    | Deprecated_library_name _ -> None)
;;

let modules_by_lib sctx lib =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let name = Lib.name (Lib.Local.to_lib lib) in
  Dir_contents.get sctx ~dir
  >>= Dir_contents.ocaml
  >>| Ml_sources.modules ~for_:(Library name)
;;

let entry_modules_by_lib sctx lib = modules_by_lib sctx lib >>| Modules.entry_modules

let entry_modules sctx ~pkg =
  let* l =
    libs_of_pkg (Super_context.context sctx) ~pkg
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

let static_html ctx all =
  let open Paths in
  [ odoc_support ctx all ]
;;

let modules_of_dir d
  : (Module_name.t * (string * Path.t * [ `Cmti | `Cmt | `Cmi ])) list Memo.t
  =
  let extensions = [ ".cmti", `Cmti; ".cmt", `Cmt; ".cmi", `Cmi ] in
  let* dir_res = Fs_memo.dir_contents (Path.as_outside_build_dir_exn d) in
  match dir_res with
  | Error _ -> Memo.return []
  | Ok dc ->
    let list = Fs_cache.Dir_contents.to_list dc in
    let modules =
      List.filter_map
        ~f:(fun (x, ty) ->
          match ty, List.assoc extensions (Filename.extension x) with
          | Unix.S_REG, Some _ -> Some (Filename.chop_extension x)
          | _, _ -> None)
        list
      |> List.sort_uniq ~compare:String.compare
    in
    let ms =
      List.map
        ~f:(fun m ->
          let ext, ty =
            List.find_exn
              ~f:(fun (ext, _ty) -> List.exists list ~f:(fun (n, _) -> n = m ^ ext))
              extensions
          in
          Module_name.of_string m, ("", Path.relative d (m ^ ext), ty))
        modules
    in
    Memo.return ms
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.map mlds ~f:(fun mld -> Filename.chop_extension (Path.Build.basename mld), mld)
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld files with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.Build.to_string_maybe_quoted p1)
          (Path.Build.to_string_maybe_quoted p2)
      ]
;;

let contains_double_underscore s =
  let len = String.length s in
  let rec aux i =
    if i > len - 2
    then false
    else if s.[i] = '_' && s.[i + 1] = '_'
    then true
    else aux (i + 1)
  in
  aux 0
;;

let check_fallback_artifacts_uniqueness
  (artifacts : (string * MiniArtifact.t list * Lib.t list) list)
  =
  let ok, _ =
    List.fold_left
      artifacts
      ~init:([], [])
      ~f:(fun (ok, artifacts_so_far) (dir, artifacts, libs) ->
        let equal a1 a2 =
          Path.Build.equal (MiniArtifact.html_dir a1) (MiniArtifact.html_dir a2)
        in
        let artifacts =
          List.filter
            ~f:(fun artifact -> not (List.mem artifacts_so_far artifact ~equal))
            artifacts
        in
        (dir, artifacts, libs) :: ok, artifacts @ artifacts_so_far)
  in
  ok
;;

let fallback_artifacts
  sctx
  (libs : (Dune_package.Lib.t * Lib.t) Lib_name.Map.t String.Map.t)
  =
  let ctx = Super_context.context sctx in
  let* public_libs = Scope.DB.public_libs ctx in
  let result =
    String.Map.foldi libs ~init:[] ~f:(fun local_dir libs acc ->
      let lib_names = Lib_name.Map.keys libs in
      let cmti_paths =
        List.map ~f:(fun path -> Path.relative path local_dir) ctx.findlib_paths
      in
      let result =
        let* mods = Memo.List.map ~f:modules_of_dir cmti_paths in
        let mods = List.flatten mods in
        let mods =
          List.fold_left mods ~init:[] ~f:(fun acc (mod_name, (subpath, cmti_file, _)) ->
            let dir = if subpath = "" then local_dir else local_dir ^ "/" ^ subpath in
            let target = Target.ExtLib dir in
            let index = index_of_local_dir dir in
            Log.info
              [ Pp.textf
                  "cmti: %s (%s) index: %s"
                  (Path.to_string cmti_file)
                  dir
                  (Index.to_string index)
              ];
            let artifact =
              MiniArtifact.make
                ctx
                true
                target
                (Module
                   ( index
                   , cmti_file
                   , not (contains_double_underscore (Module_name.to_string mod_name)) ))
            in
            artifact :: acc)
        in
        let+ libs =
          Memo.List.fold_left ~init:[] lib_names ~f:(fun acc lib_name ->
            let+ lib_opt = Lib.DB.find public_libs lib_name in
            match lib_opt with
            | None -> acc
            | Some l -> l :: acc)
        in
        local_dir, mods, libs
      in
      result :: acc)
  in
  let+ result = Memo.all_concurrently result in
  check_fallback_artifacts_uniqueness result
;;

let package_mlds =
  let memo =
    Memo.create
      "package-mlds"
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
;;

let ext_package_mlds (ctx : Context.t) (pkg : Package.Name.t) =
  let* findlib = Findlib.create ctx.name in
  let* result = Findlib.find_root_package findlib pkg in
  match result with
  | Error _ -> Memo.return []
  | Ok dpkg ->
    let installed = dpkg.files in
    List.filter_map installed ~f:(function
      | Dune_section.Doc, fs ->
        let doc_path = Section.Map.find_exn dpkg.sections Doc in
        Some
          (List.filter_map
             ~f:(fun dst ->
               let str = Install.Entry.Dst.to_string dst in
               if Filename.check_suffix str ".mld"
               then Some (Path.relative doc_path str)
               else None)
             fs)
      | _ -> None)
    |> List.concat
    |> Memo.return
;;

let singleton_artifacts ctx index dwm =
  let info = Lib.info dwm.lib in
  let obj_dir = Lib_info.obj_dir info in
  let target = Target.ExtLib dwm.local_dir in
  let artifacts =
    Modules.fold_no_vlib dwm.modules ~init:[] ~f:(fun m acc ->
      let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind:(Ocaml Cmi) m in
      let visible =
        List.mem dwm.entry_modules (Module.name m) ~equal:(fun m1 m2 ->
          Module_name.equal m1 m2)
      in
      let artifact =
        MiniArtifact.make ctx true target (Module (index, cmti_file, visible))
      in
      artifact :: acc)
  in
  artifacts
;;

let ext_pkg_mld_artifacts ctx dir pkg =
  let t = Target.ExtLib dir in
  let+ mlds = ext_package_mlds ctx pkg in
  let index_file, mlds =
    List.partition ~f:(fun a -> Path.basename a = "index.mld") mlds
  in
  let index_file =
    match index_file with
    | [ x ] -> Some x
    | _ -> None
  in
  let artifacts =
    List.map mlds ~f:(fun m ->
      MiniArtifact.make ctx true t (Mld ([ Index.External (Package.Name.to_string pkg) ], m)))
  in
  index_file, artifacts
;;

let pkg_artifacts sctx all pkg =
  let ctx = Super_context.context sctx in
  let+ mlds = Packages.mlds sctx pkg in
  let index_file, mlds =
    List.partition ~f:(fun a -> Path.Build.basename a = "index.mld") mlds
  in
  let index_file =
    match index_file with
    | [ x ] -> Some (Path.build x)
    | _ -> None
  in
  let mlds = check_mlds_no_dupes ~pkg ~mlds in
  let mlds =
    String.Map.values mlds
    |> List.map ~f:(fun mld ->
      MiniArtifact.make ctx all (Pkg pkg) ([ Index.LocalPackage pkg ], mld))
  in
  index_file, mlds
;;

let local_lib_artifacts sctx all index lib =
  let ctx = Super_context.context sctx in
  let info = Lib.Local.info lib in
  let modes = Lib_info.modes info in
  let mode = Lib_mode.Map.Set.for_merlin modes in
  let cm_kind =
    let open Lib_mode in
    match mode with
    | Ocaml _ -> Cm_kind.Ocaml Cmi
    | Melange -> Cm_kind.Melange Cmi
  in
  let obj_dir = Lib_info.obj_dir info in
  let+ modules = modules_by_lib sctx lib in
  let entry_modules = Modules.entry_modules modules in
  let modules =
    Modules.fold_no_vlib
      ~init:[]
      ~f:(fun m acc ->
        let visible =
          List.mem entry_modules m ~equal:(fun m1 m2 ->
            Module_name.equal (Module.name m1) (Module.name m2))
        in
        let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind m in
        (MiniArtifact.make ctx all (Lib lib) (index, cmti_file, visible))
        :: acc)
      modules
  in
  modules
;;

module Index_info = struct
  type index_info =
    { artifacts : MiniArtifact.t list
    ; predefined_index : Path.t option
    ; lib : (Lib.t * Module_name.t list) list
    }
end

module IndexTree = struct
  type 'a t = Br of Index.ty * 'a * 'a t list

  let of_index_list :
        'a. empty:'a -> combine:('a -> 'a -> 'a) -> (Index.t * 'a) list -> 'a t list
    =
    fun ~empty ~combine indexes ->
    Log.info [ Pp.textf "of_index_info: %d indexes" (List.length indexes) ];
    let cmp x y =
      match y with
      | Br (y, _, _) -> Index.compare_ty x y = Eq
    in
    let add_one (cur : 'a t list) (index, index_info) =
      let list = List.rev index in
      let rec inner (tree : 'a t list) index_list =
        match tree, index_list with
        | x, [] -> x
        | ys, [ x ] ->
          (match List.partition ys ~f:(cmp x) with
           | [ Br (x, ii, children) ], others ->
             Br (x, combine ii index_info, children) :: others
           | [], others -> Br (x, index_info, []) :: others
           | _ -> assert false)
        | ys, x :: xs ->
          (match List.partition ys ~f:(cmp x) with
           | [ Br (v, ii, children) ], others -> Br (v, ii, inner children xs) :: others
           | [], others -> Br (x, empty, inner [] xs) :: others
           | _ -> assert false)
      in
      inner cur list
    in
    List.fold_left indexes ~init:[] ~f:add_one
  ;;

  let of_index_info =
    let empty = { Index_info.artifacts = []; predefined_index = None; lib = []; } in
    let combine x y =
      let predefined_index =
        match x.Index_info.predefined_index, y.Index_info.predefined_index with
        | None, None -> None
        | Some x, None | None, Some x -> Some x
        | Some x, Some y ->
          Log.info
            [ Pp.textf
                "of_index_info: duplicate predefined index: %s %s"
                (Path.to_string x)
                (Path.to_string y)
            ];
          Some x
      in
      { Index_info.artifacts = x.Index_info.artifacts @ y.Index_info.artifacts
      ; predefined_index
      ; lib = x.lib @ y.lib
      }
    in
    fun x -> of_index_list ~empty ~combine x
  ;;
end

let index_info_of_local_pkg sctx all pkg_name =
  Log.info [ Pp.textf "index_info_of_local_pkg: %s" (Package.Name.to_string pkg_name) ];
  let* entry_modules = entry_modules sctx ~pkg:pkg_name in
  let* index_infos =
    Lib.Local.Map.foldi ~init:(Memo.return []) entry_modules ~f:(fun lib modules acc ->
      let* acc = acc in
      let index = index_of_local_lib lib in
      let* artifacts = local_lib_artifacts sctx all index lib in
      let entry_modules =
        modules
        |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
        |> List.map ~f:Module.name
      in
      let lib = [ (lib :> Lib.t), entry_modules ] in
      Memo.return ((index, { Index_info.lib; artifacts; predefined_index = None; }) :: acc))
  in
  let+ main_index_path, main_artifacts = pkg_artifacts sctx all pkg_name in
  let pkg_index_info =
    ( [ Index.LocalPackage pkg_name ]
    , { Index_info.lib = []
      ; artifacts = main_artifacts
      ; predefined_index = main_index_path
      } )
  in
  IndexTree.of_index_info (pkg_index_info :: index_infos)
;;

let index_info_of_private_lib sctx all lnu lib =
  let index = [ Index.PrivateLib lnu ] in
  let* artifacts = local_lib_artifacts sctx all index lib in
  let+ modules = entry_modules_by_lib sctx lib in
  let entry_modules =
    modules
    |> List.filter ~f:(fun m -> Module.visibility m = Visibility.Public)
    |> List.map ~f:Module.name
  in
  let lib = [ (lib :> Lib.t), entry_modules ] in
  IndexTree.of_index_info [(index, { Index_info.lib; artifacts; predefined_index = None;})]
;;

let index_info_of_dune_with_modules ctx dir pkg_name dwms =
  let* dwms = Valid.filter_dwms ctx true dwms in
  let+ main_index_path, main_artifacts = ext_pkg_mld_artifacts ctx dir pkg_name in
  let index_infos =
    List.fold_left ~init:[] dwms ~f:(fun acc dwm ->
      let info = Lib.info dwm.lib in
      let index = index_of_external_lib ctx dwm.lib in
      let artifacts = singleton_artifacts ctx index dwm in
      let entry_modules =
        match Lib_info.entry_modules info with
        | External (Ok modules) -> modules
        | _ -> []
      in
      let lib = [ dwm.lib, entry_modules ] in

      (index, { Index_info.lib; artifacts; predefined_index = None }) :: acc)
  in
  let pkg_index_info =
    ( [ Index.External (Package.Name.to_string pkg_name) ]
    , { Index_info.lib = []
      ; artifacts = main_artifacts
      ; predefined_index = main_index_path
      } )
  in
  IndexTree.of_index_info (pkg_index_info :: index_infos)
;;

let index_info_of_external_fallback sctx (fallback : fallback) =
  let ctx = Super_context.context sctx in
  let* libs = Valid.filter_fallback_libs ctx true fallback.libs in
  let* artifacts = fallback_artifacts sctx libs in
  let+ index_infos = List.fold_left ~init:(Memo.return []) artifacts ~f:(fun acc (dir, artifacts, libs) ->
    let* acc = acc in
    Log.info [ Pp.textf "index_info_of_external_fallback: %s" dir ];
    let+ libs = Valid.filter_libs ctx true libs in
    let index = index_of_local_dir dir in
    let libs =
      List.map libs ~f:(fun lib ->
        let info = Lib.info lib in
        let entry_modules =
          match Lib_info.entry_modules info with
          | External (Ok modules) -> modules
          | _ -> []
        in
        lib, entry_modules)
    in
  (index, { Index_info.artifacts; lib = libs; predefined_index = None; }) :: acc) in
  IndexTree.of_index_info index_infos
;;

(* Index generation

   The following are the functions for generating the indexes for
   the four different types of level-1 indexes. There are:

   1. Local packages
   2. Local private libraries
   3. External Dune packages for which we have module info
   4. External opam-lib directories

   These should all be disjoint as local packages override external
   packages, local private libs have unique names generated for them
   and 'other' opam lib directories share the same namespace in the
   opam dir as external packages (as dune enforces that the package
   name is equal to the dir name in the opam lib directory).
*)

let default_index ~main_name ~pkg_opt ~subindexes entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf
    b
    "{0 %s %s}\n"
    main_name
    (match pkg_opt with
     | None -> "index"
     | Some pkg ->
       (match pkg.Package.synopsis with
        | None -> "index"
        | Some s -> " : " ^ s));
  (match pkg_opt with
   | None -> ()
   | Some pkg ->
     (match pkg.Package.description with
      | None -> ()
      | Some s -> Printf.bprintf b "%s\n" s));
  if List.length subindexes > 0
  then (
    Printf.bprintf b "{1 Sub-packages}\n%!";
    subindexes
    |> List.sort ~compare:(fun x y -> Dyn.compare (Index.to_dyn x) (Index.to_dyn y))
    |> List.iter ~f:(fun i ->
      Printf.bprintf b "- {{!page-\"%s\"}%s}\n" (Index.mld_name i) (Index.name i)));
  entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
  |> List.iter ~f:(fun (lib, modules) ->
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib);
    Buffer.add_string
      b
      (match modules with
       | [ x ] ->
         sprintf
           "The entry point of this library is the module:\n{!module-%s}.\n"
           (Module_name.to_string x)
       | _ ->
         sprintf
           "This library exposes the following toplevel modules:\n{!modules:%s}\n"
           (modules
            |> List.sort ~compare:(fun x y -> Module_name.compare x y)
            |> List.map ~f:Module_name.to_string
            |> String.concat ~sep:" ")));
  Buffer.contents b
;;

(* let default_fallback_index local_path artifacts =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 Index for filesystem path %s}\n" local_path;
  Printf.bprintf
    b
    "{!modules:%s}\n"
    (artifacts
     |> List.filter ~f:MiniArtifact.is_visible
     |> List.filter_map ~f:MiniArtifact.module_name
     |> List.sort ~compare:(fun x y -> Module_name.compare x y)
     |> List.map ~f:Module_name.to_string
     |> String.concat ~sep:" ");
  Buffer.contents b
;; *)

(* let default_private_index l artifacts =
  let b = Buffer.create 512 in
  Printf.bprintf
    b
    "{0 %s index}\n"
    (Lib.Local.info l |> Lib_info.name |> Lib_name.to_string);
  let mods =
    List.filter artifacts ~f:MiniArtifact.is_visible
    |> List.filter_map ~f:MiniArtifact.module_name
  in
  Buffer.add_string
    b
    (match mods with
     | [ x ] ->
       sprintf
         "The entry point of this library is the module:\n{!module-%s}.\n"
         (Module_name.to_string x)
     | _ ->
       sprintf
         "This library exposes the following toplevel modules:\n{!modules:%s}\n"
         (mods
          |> List.sort ~compare:(fun x y -> Module_name.compare x y)
          |> List.map ~f:(fun m -> Module_name.to_string m)
          |> String.concat ~sep:" "));
  Buffer.contents b
;; *)

let toplevel_index_contents _sctx tree =
  let top = List.map tree ~f:(function | IndexTree.Br (i, _, _) -> i) in

  let b = Buffer.create 1024 in
  Printf.bprintf b "{0 Docs}\n\n";
  let output_indices label = function
    | [] -> ()
    | indices ->
      Printf.bprintf b "{1 %s}\n" label;
      List.iter
        ~f:(fun i -> Printf.bprintf b "- {!page-\"%s\"}\n" (Index.mld_name_ty i))
        indices
  in
  output_indices
    "Local packages"
    (List.filter top ~f:(function
      | Index.LocalPackage _ -> true
      | _ -> false));
  output_indices
    "Switch-installed packages"
    (List.filter top ~f:(function
      | Index.External _ -> true
      | _ -> false));
  (* output_indices
    "Other switch library directories"
    (List.filter top ~f:(function
      | Index.ExternalFallback _ -> true
      | _ -> false)); *)
  output_indices
    "Private libraries"
    (List.filter top ~f:(function
      | Index.PrivateLib _  -> true
      | _ -> false));
  Buffer.contents b
;;

let hierarchical_index_rules sctx all tree =
  Log.info [ Pp.textf "hierarchical_index_rules" ];
  let ctx = Super_context.context sctx in
  let main_name =
    match tree with
    | [ IndexTree.Br (i, _, _) ] -> Index.ty_name i
    | _ -> "index"
  in
  let rec gather_mlds idx acc tree =
    List.fold_left tree ~init:acc ~f:(fun acc (IndexTree.Br (i, _ii, children)) ->
      let index = i::idx in
      let mld = create_index_artifact ctx all index in
      gather_mlds index (mld :: acc) children)
  in
  let all_mlds = gather_mlds [] [] tree in
  let rec inner idx tree =
    Memo.List.iter tree ~f:(fun (IndexTree.Br (i, ii, children)) ->
      let index = i :: idx in
      let index_path = Index.mld_path ctx all index in
      let mld = create_index_artifact ctx all index in
      let parent_opt = Some (create_index_artifact ctx all idx) in
      let subindexes =
        List.map
          ~f:(function
            | IndexTree.Br (x, _, _) -> x :: index)
          children
      in
      let extra_children = List.map ~f:(create_index_artifact ctx true) subindexes in
      let liblist = List.map ~f:(fun (l, x) -> Lib.name l, x) ii.Index_info.lib in
      let* () =
        match ii.predefined_index with
        | Some p -> add_rule sctx (Action_builder.symlink ~src:p ~dst:index_path)
        | None -> add_rule sctx (Action_builder.write_file index_path (default_index ~main_name ~pkg_opt:None ~subindexes liblist))
      in
      let libs = List.map ~f:fst ii.lib in
      let* _ =
        compile_mld
        sctx
        mld
        ~quiet:false
        ~parent_opt
        ~is_index:true
        ~children:(extra_children @ ii.artifacts)
      in
      Log.info [Pp.textf "rules for %s" (Path.Build.to_string (MiniArtifact.odocl_file mld))];
      let* () = link_odoc_rules sctx all [mld] ~package:None ~libs ~indices:all_mlds ~quiet:false in

      inner index children)
  in
  inner [] tree

;;

let hierarchical_html_rules sctx all tree =
  let ctx = Super_context.context sctx in
  let rec inner idx dirs tree =
    Memo.List.fold_left
      tree
      ~f:(fun dirs (IndexTree.Br (i, ii, children)) ->
        let index = i :: idx in
        let index_artifact = create_index_artifact ctx true index in
        Log.info
          [ Pp.textf
              "hierarchical_index_rules: %s (%d children)"
              (Index.name index)
              (List.length children)
          ];
        let artifacts =
          List.filter
            ~f:(fun a -> MiniArtifact.is_visible a)
            (index_artifact :: ii.Index_info.artifacts)
        in
        let* new_dirs =
          Memo.List.filter_map artifacts ~f:(fun a -> html_generate sctx true a)
        in
        let html_files =
          List.map artifacts ~f:(fun a -> Path.build (MiniArtifact.html_file a))
        in
        let html_dir = Index.html_dir ctx all index in
        let html_alias = Dep.html_alias html_dir in
        let* () =
          Rules.Produce.Alias.add_deps html_alias (Action_builder.paths html_files)
        in
        inner index (new_dirs @ dirs) children)
      ~init:dirs
  in
  inner [] [] tree
;;


let all_indexes sctx all =
  let ctx = Super_context.context sctx in
  let* categorized = Valid.get_categorized ctx all in
  let indexes = Package.Name.Set.fold ~init:(Memo.return []) categorized.Valid.packages ~f:(fun pkg acc ->
    let* acc = acc in
    let+ ii = index_info_of_local_pkg sctx all pkg in
    List.rev_append ii acc) in
  let indexes = String.Map.foldi ~init:indexes categorized.localprivate ~f:(fun name lib acc ->
    let* acc = acc in
    let+ ii = index_info_of_private_lib sctx all name lib in
    List.rev_append ii acc) in
  String.Map.foldi ~init:indexes categorized.external_dirs ~f:(fun dir ty acc ->
    let* acc = acc in
    match ty with
    | DuneWithModules (pkg_name, dwms) ->
      let+ ii = index_info_of_dune_with_modules (Super_context.context sctx) dir pkg_name dwms in
      List.rev_append ii acc
    | Fallback fallback ->
      let+ ii = index_info_of_external_fallback sctx fallback in
      List.rev_append ii acc
    | Nothing -> Memo.return acc) 

let setup_all_index_rules sctx all =
  let ctx = Super_context.context sctx in
  let* tree = all_indexes sctx all in
  let* () = hierarchical_index_rules sctx all tree in
  let artifacts =
    List.map ~f:(fun (IndexTree.Br (i, _, _)) -> create_index_artifact ctx all [ i ]) tree
  in
  let contents = toplevel_index_contents sctx tree in
  let f = Index.mld_path ctx all [] in
  let mld = create_index_artifact ctx all [] in
  let* () = add_rule sctx (Action_builder.write_file f contents) in
  let* _ =
    compile_mld
      sctx
      mld
      ~quiet:false
      ~parent_opt:None
      ~is_index:true
      ~children:artifacts
  in
  let artifact = create_index_artifact ctx all [] in
  let* _ =
    link_odoc_rules sctx all [ artifact ] ~package:None ~libs:[] ~indices:artifacts ~quiet:false
  in
  Memo.return []


let setup_all_html_rules sctx all =
  let ctx = Super_context.context sctx in
  let* tree = all_indexes sctx all in
  let artifact = create_index_artifact ctx all [] in
  let rec get_indexes idx tree =
    List.map tree ~f:(fun (IndexTree.Br (i, _ii, children)) ->
      let index = i :: idx in
      let indexes = get_indexes index children in
      index :: indexes) |> List.concat in
  let indexes = get_indexes [] tree in
  let deps =
    List.map ~f:(fun x -> x |> Index.html_dir ctx all |> Dep.html_alias |> Dune_engine.Dep.alias) indexes in
  let deps = Dune_engine.Dep.Set.of_list deps in
  let html = List.map ~f:(fun b -> Path.build b) (MiniArtifact.html_file artifact :: static_html ctx all) in
  let* () =
    Rules.Produce.Alias.add_deps
        (Dep.html_alias (Index.html_dir ctx all []))
        (Action_builder.paths html) in
  let* () =
    Rules.Produce.Alias.add_deps
      (Dep.html_alias (Index.html_dir ctx all []))
      (Action_builder.deps deps)
  in
  let* dirs = hierarchical_html_rules sctx all tree in
  let* () = setup_css_rule sctx all in
  let* _ = html_generate sctx all artifact in
  Memo.return (Paths.odoc_support ctx all :: dirs)

  
(* End of index rules *)

(* External rules *)

(* Intra-library module dependencies have to be found out for
   external libraries, we do this by running [odoc compile-deps]
   per module. *)
let external_module_deps_rule sctx all a =
  match MiniArtifact.artifact_ty a with
  | Module _ ->
    let ctx = Super_context.context sctx in
    let* odoc = odoc_program sctx (Paths.root ctx all) in
    let deps_file = Path.Build.set_extension (MiniArtifact.odoc_file a) ~ext:".deps" in
    let* () =
      Super_context.add_rule
        sctx
        ~dir:(Paths.root ctx all)
        (Command.run
           odoc
           ~dir:(Path.parent_exn (Path.build deps_file))
           ~stdout_to:deps_file
           [ A "compile-deps"; Dep (MiniArtifact.source_file a) ])
    in
    Memo.return (Some deps_file)
  | _ -> Memo.return None
;;

let compile_odocs sctx all ~quiet artifacts parent libs =
  let requires = compile_requires libs in
  let ctx = Super_context.context sctx in
  let* requires =
    Resolve.Memo.bind requires ~f:(fun libs ->
      let+ libs = Valid.filter_libs ctx all libs in
      Resolve.return libs)
  in
  Memo.parallel_iter artifacts ~f:(fun a ->
    let* deps_file = external_module_deps_rule sctx all a in
    match deps_file with
    | None -> 
      (* mld file *)
      let* _ = compile_mld
        sctx
        a
        ~quiet:false
        ~parent_opt:(Some parent)
        ~is_index:false
        ~children:[] in
      Memo.return ()
   | Some deps_file ->
      let module_deps =
        let open Action_builder.O in
        let* l = Action_builder.lines_of (Path.build deps_file) in
        let deps = parse_odoc_deps l in
        let deps' =
          List.filter_map
            ~f:(fun (m', _) ->
              if MiniArtifact.module_name a = Some m'
              then None
              else (
                match
                  List.find_opt artifacts ~f:(fun a ->
                    MiniArtifact.module_name a = Some m')
                with
                | None -> None
                | Some a' -> Some (MiniArtifact.odoc_file a' |> Path.build)))
            deps
        in
        Dune_engine.Dep.Set.of_files deps' |> Action_builder.deps
      in
      let parent_opt =
        match MiniArtifact.artifact_ty a with
        | Module true -> Some parent
        | _ -> None
      in
      let* _odoc_file =
        compile_module
          sctx
          all
          ~artifact:a
          ~requires
          ~module_deps
          ~quiet
          ~parent_opt
          ~package:None
          ~indices:[]
      in
      Memo.return ())
;;


let hierarchical_odoc_rules sctx all tree =
  let ctx = Super_context.context sctx in
  let rec inner idx tree =
    Memo.List.iter tree ~f:(fun (IndexTree.Br (i, ii, children)) ->
      let index = i :: idx in
      let parent = create_index_artifact ctx all index in
      let artifacts = ii.Index_info.artifacts in
      let libs = List.map ~f:fst ii.lib in
      let ctx = Super_context.context sctx in
      let quiet = Index.is_external index in
      let* () = compile_odocs sctx all ~quiet artifacts parent libs in
      let* () = link_odoc_rules sctx all artifacts ~package:None ~libs ~indices:[] ~quiet in
      let all_deps =
        List.map ~f:(fun a -> MiniArtifact.odoc_file a |> Path.build) artifacts
        |> Path.Set.of_list
      in
      let* () =
        Memo.List.iter [index] ~f:(fun alias -> Dep.setup_deps ctx all alias all_deps)
      in
      inner index children)
  in inner [] tree


let setup_odoc_rules sctx all =
  let* tree = all_indexes sctx all in
  let+ () = hierarchical_odoc_rules sctx all tree in
  []
(* End of external rules *)

let gen_project_rules sctx project =
  let* packages = Only_packages.packages_of_project project in
  let ctx = Super_context.context sctx in
  Package.Name.Map_traversals.parallel_iter packages ~f:(fun _ (pkg : Package.t) ->
    let dir =
      let pkg_dir = Package.dir pkg in
      Path.Build.append_source ctx.build_dir pkg_dir
    in
    let register alias all =
      let top_alias = Dep.html_alias (Index.html_dir ctx all []) in
      Dune_engine.Dep.alias top_alias
      |> Dune_engine.Dep.Set.singleton
      |> Action_builder.deps
      |> Rules.Produce.Alias.add_deps alias
    in
    let* () = register (Alias.make Alias0.doc_new ~dir) false in
    register (Alias.make Alias0.doc_new ~dir) true)
;;

let has_rules m =
  let* dirs, rules = Rules.collect (fun () -> m) in
  let directory_targets =
    Path.Build.Map.of_list_exn (List.map ~f:(fun dir -> dir, Loc.none) dirs)
  in
  Memo.return (Gen_rules.make ~directory_targets (Memo.return rules))
;;

let no_rules = Gen_rules.make (Memo.return Rules.empty)


let gen_rules sctx ~dir rest =
  let all = true in
  match rest with
  | [] ->
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ "odoc"; ] -> has_rules (setup_odoc_rules sctx all)
  | [ "index"; ] -> has_rules (setup_all_index_rules sctx all)
  | [ "html"; "docs"; ] -> has_rules (setup_all_html_rules sctx all)
  | _ -> Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
;;
