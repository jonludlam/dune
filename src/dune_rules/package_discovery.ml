[@@@warning "-32"] (* unused functions *)
[@@@warning "-69"] (* unused fields *)

open Import
open Memo.O

(* Simplified type - removed unused package_info complexity *)
type t = {
  package_of_lib : Package.Name.t Lib_name.Map.t;
  libs_of_package : Lib.t list Package.Name.Map.t;
  mlds_of_package : Path.t list Package.Name.Map.t;
  config_of_package : Odoc_config.t Package.Name.Map.t;
  installed_files : string list Package.Name.Map.t; (* Files from .changes files *)
  opam_prefix : Path.t option; (* Root of opam installation *)
}

let empty = {
  package_of_lib = Lib_name.Map.empty;
  libs_of_package = Package.Name.Map.empty;
  mlds_of_package = Package.Name.Map.empty;
  config_of_package = Package.Name.Map.empty;
  installed_files = Package.Name.Map.empty;
  opam_prefix = None;
}

(* Parse an opam changes file using the opam-format library *)
let parse_changes_file ~changes_file_path contents =
  let filename = OpamFile.make (OpamFilename.of_string (Path.to_string changes_file_path)) in
  try
    let changed = 
      (* Remove opam-version line if present, as it's invalid in changes files *)
      let clean_contents = 
        if String.is_prefix contents ~prefix:"opam-version" then
          match String.index contents '\n' with
          | Some pos -> 
            let len = String.length contents in
            String.sub contents ~pos:(pos + 1) ~len:(len - pos - 1)
          | None -> ""
        else contents
      in
      OpamFile.Changes.read_from_string ~filename clean_contents
    in
    let added =
      OpamStd.String.Map.fold
        (fun file track acc ->
          match track with
          | OpamDirTrack.Added _ -> file :: acc
          | _ -> acc)
        changed []
    in
    added
  with
  | OpamPp.Bad_version _ | OpamPp.Bad_format _ ->
    (* Failed to parse, return empty list *)
    []

let read_opam_changes_file ~opam_prefix ~package_name =
  let changes_file = 
    Path.relative opam_prefix 
      (sprintf ".opam-switch/install/%s.changes" 
        (Package.Name.to_string package_name))
  in
  (* Convert to Outside_build_dir.t as required by Fs_memo *)
  let changes_file_external = Path.as_outside_build_dir_exn changes_file in
  let* exists = Fs_memo.file_exists changes_file_external in
  if exists then
    let+ contents = Fs_memo.file_contents changes_file_external in
    let files = parse_changes_file ~changes_file_path:changes_file contents in
    Some files
  else
    (* Changes file doesn't exist *)
    Memo.return None

let discover_opam_packages ~opam_prefix =
  (* List all *.changes files in .opam-switch/install/ *)
  let install_dir = Path.relative opam_prefix ".opam-switch/install" in
  let install_dir_external = Path.as_outside_build_dir_exn install_dir in
  let* dir_result = Fs_memo.dir_contents ~force_update:false install_dir_external in
  match dir_result with
  | Error _ -> 
    (* Directory doesn't exist *)
    Memo.return []
  | Ok contents ->
    let changes_files = 
      Fs_cache.Dir_contents.to_list contents
      |> List.filter_map ~f:(fun (filename, _kind) ->
        if String.is_suffix filename ~suffix:".changes" then
          match String.drop_suffix filename ~suffix:".changes" with
          | Some pkg_name -> Some (Package.Name.of_string pkg_name)
          | None -> None
        else
          None)
    in
    let+ packages_with_files = 
      Memo.parallel_map changes_files ~f:(fun pkg_name ->
        let+ files = read_opam_changes_file ~opam_prefix ~package_name:pkg_name in
        (pkg_name, files))
    in
    List.filter_map packages_with_files ~f:(fun (pkg_name, files) ->
      match files with
      | None -> None
      | Some files -> Some (pkg_name, files))

let find_package_for_library ~file_to_package_map lib =
  let lib_info = Lib.info lib in
  let lib_name = Lib.name lib in

  (* Get the actual archive files for this library *)
  let archives = Lib_info.archives lib_info in
  let byte_archives = Mode.Dict.get archives Byte in
  let native_archives = Mode.Dict.get archives Native in
  let all_archives = byte_archives @ native_archives in

  (* Special case for stdlib which has no archives - construct the expected path *)
  let all_archives =
    if List.is_empty all_archives && Lib_name.equal lib_name (Lib_name.of_string "stdlib") then
      (* stdlib archive should be at lib/ocaml/stdlib.cma relative to opam prefix *)
      let src_dir = Lib_info.src_dir lib_info in
      [ Path.relative src_dir "stdlib.cma" ]
    else
      all_archives
  in

  (* Look up each archive file in the opam changes map to see which package installed it *)
  List.find_map all_archives ~f:(fun archive_path ->
    Path.Map.find file_to_package_map archive_path)

(* Simplified: inline the logic *)

let build_mappings_from_changes_data ~file_to_package_map libs =
  List.fold_left libs ~init:empty ~f:(fun acc lib ->
    let lib_name = Lib.name lib in
    let pkg_name_opt = find_package_for_library ~file_to_package_map lib in

    match pkg_name_opt with
    | None -> acc  (* No package found for this library, skip it *)
    | Some pkg_name ->
      { package_of_lib = Lib_name.Map.set acc.package_of_lib lib_name pkg_name;
        libs_of_package =
          Package.Name.Map.update acc.libs_of_package pkg_name ~f:(function
            | None -> Some [lib]
            | Some libs -> Some (lib :: libs));
        mlds_of_package = acc.mlds_of_package;
        config_of_package = acc.config_of_package;
        installed_files = acc.installed_files;
        opam_prefix = acc.opam_prefix;
      })

let build_file_to_package_map packages_with_files ~opam_prefix =
  List.fold_left packages_with_files ~init:Path.Map.empty
    ~f:(fun acc (pkg_name, files) ->
      List.fold_left files ~init:acc ~f:(fun acc file_str ->
        let file_path = Path.relative opam_prefix file_str in
        Path.Map.set acc file_path pkg_name))

let build_installed_files_map packages_with_files =
  List.fold_left packages_with_files ~init:Package.Name.Map.empty
    ~f:(fun acc (pkg_name, files) ->
      Package.Name.Map.set acc pkg_name files)

let build_mlds_map packages_with_files ~opam_prefix =
  (* Extract mld files from the changes data
     Pattern: doc/{package}/odoc-pages/**/*.mld *)
  List.fold_left packages_with_files ~init:Package.Name.Map.empty
    ~f:(fun acc (pkg_name, files) ->
      let mld_files =
        List.filter_map files ~f:(fun file_str ->
          (* Check if this is an mld file in the doc/{pkg}/odoc-pages/ directory *)
          let parts = String.split file_str ~on:'/' in
          match parts with
          | "doc" :: pkg :: "odoc-pages" :: _ when String.equal pkg (Package.Name.to_string pkg_name) ->
            if String.is_suffix file_str ~suffix:".mld" then
              Some (Path.relative opam_prefix file_str)
            else
              None
          | _ -> None)
      in
      if List.is_empty mld_files then
        acc
      else
        Package.Name.Map.set acc pkg_name mld_files)

let build_config_map packages_with_files ~opam_prefix =
  (* Extract config files from the changes data
     Pattern: doc/{package}/odoc-config.sexp *)
  List.fold_left packages_with_files ~init:Package.Name.Map.empty
    ~f:(fun acc (pkg_name, files) ->
      let config_file_opt =
        List.find_opt files ~f:(fun file_str ->
          let parts = String.split file_str ~on:'/' in
          match parts with
          | ["doc"; pkg; "odoc-config.sexp"] when String.equal pkg (Package.Name.to_string pkg_name) ->
            true
          | _ -> false)
      in
      match config_file_opt with
      | None -> acc
      | Some config_file_str ->
        let config_path = Path.relative opam_prefix config_file_str in
        let config = Odoc_config.load config_path in
        Package.Name.Map.set acc pkg_name config)

let get_opam_prefix ~context =
  let kind = Context.kind context in
  match kind with
  | Opam { root = _; switch = _ } | Default | Lock _ ->
    (* Get opam prefix from environment variable for all context types *)
    let* env = Context.installed_env context in
    (match Env.get env Opam_switch.opam_switch_prefix_var_name with
    | Some prefix -> Memo.return (Some (Path.of_string prefix))
    | None -> Memo.return None)

let discover_package_ownership ~context =
  let* opam_prefix = get_opam_prefix ~context in
  match opam_prefix with
  | None -> Memo.return Path.Map.empty
  | Some prefix ->
    let+ packages_with_files = discover_opam_packages ~opam_prefix:prefix in
    build_file_to_package_map packages_with_files ~opam_prefix:prefix

(* Removed unused function - was always returning empty list *)

let create_impl context =
  let* opam_prefix = get_opam_prefix ~context in
  match opam_prefix with
  | None -> Memo.return empty
  | Some prefix ->
    let* packages_with_files = discover_opam_packages ~opam_prefix:prefix in
    let file_to_package = build_file_to_package_map packages_with_files ~opam_prefix:prefix in
    let mlds_map = build_mlds_map packages_with_files ~opam_prefix:prefix in
    let config_map = build_config_map packages_with_files ~opam_prefix:prefix in
    let installed_files_map = build_installed_files_map packages_with_files in

    (* Debug: Log discovered mld files *)
    Package.Name.Map.iteri mlds_map ~f:(fun pkg mlds ->
      if not (List.is_empty mlds) then
        Log.info [ Pp.textf "Package_discovery: Package %s has %d mld files"
          (Package.Name.to_string pkg) (List.length mlds) ]);

    (* Debug: Log discovered config files *)
    Package.Name.Map.iteri config_map ~f:(fun pkg config ->
      if not (Odoc_config.(config.deps.libraries = [] && config.deps.packages = [])) then
        Log.info [ Pp.textf "Package_discovery: Package %s has odoc-config with %d libs, %d pkgs"
          (Package.Name.to_string pkg)
          (List.length config.Odoc_config.deps.libraries)
          (List.length config.Odoc_config.deps.packages) ]);

    (* Get all installed libraries to map them to packages *)
    let* installed_libs = Lib.DB.installed context in
    let* all_libs_set = Lib.DB.all installed_libs in
    let all_libs = Lib.Set.to_list all_libs_set in

    let lib_mappings = build_mappings_from_changes_data ~file_to_package_map:file_to_package all_libs in

    Memo.return { lib_mappings with
                  mlds_of_package = mlds_map;
                  config_of_package = config_map;
                  installed_files = installed_files_map;
                  opam_prefix = Some prefix }

let create =
  let memo = Memo.create "package-discovery" ~input:(module Context) create_impl in
  fun ~context -> Memo.exec memo context

let package_of_library t lib =
  let lib_name = Lib.name lib in
  Lib_name.Map.find t.package_of_lib lib_name

let libraries_of_package t pkg =
  Package.Name.Map.find t.libs_of_package pkg |> Option.value ~default:[]

let mlds_of_package t pkg =
  Package.Name.Map.find t.mlds_of_package pkg |> Option.value ~default:[]

let module_source_file t ~lib ~module_name =
  (* For installed libraries, query the installed files from .changes data *)
  match t.opam_prefix with
  | None -> None
  | Some prefix ->
    let info = Lib.info lib in
    let src_dir = Lib_info.src_dir info in
    let module_name_lower = String.uncapitalize_ascii module_name in

    (* Get the package this library belongs to *)
    let lib_name = Lib.name lib in
    match Lib_name.Map.find t.package_of_lib lib_name with
    | None -> None
    | Some pkg ->
      (* Get installed files for this package *)
      match Package.Name.Map.find t.installed_files pkg with
      | None -> None
      | Some files ->
        (* Build the relative path from opam prefix to the source directory *)
        let src_dir_str = Path.to_string src_dir in
        let prefix_str = Path.to_string prefix in

        (* Remove the prefix to get the relative path *)
        let rel_dir =
          if String.is_prefix src_dir_str ~prefix:prefix_str then
            let prefix_len = String.length prefix_str in
            let src_len = String.length src_dir_str in
            if prefix_len < src_len && src_dir_str.[prefix_len] = '/' then
              String.sub src_dir_str ~pos:(prefix_len + 1) ~len:(src_len - prefix_len - 1)
            else if prefix_len = src_len then
              ""
            else
              src_dir_str
          else
            src_dir_str
        in

        (* Check if .cmti exists, otherwise use .cmt *)
        let cmti_rel =
          if String.is_empty rel_dir then
            module_name_lower ^ ".cmti"
          else
            rel_dir ^ "/" ^ module_name_lower ^ ".cmti"
        in
        let cmt_rel =
          if String.is_empty rel_dir then
            module_name_lower ^ ".cmt"
          else
            rel_dir ^ "/" ^ module_name_lower ^ ".cmt"
        in

        if List.mem files cmti_rel ~equal:String.equal then
          Some (Path.relative src_dir (module_name_lower ^ ".cmti"))
        else if List.mem files cmt_rel ~equal:String.equal then
          Some (Path.relative src_dir (module_name_lower ^ ".cmt"))
        else
          None

let config_of_package t pkg =
  Package.Name.Map.find t.config_of_package pkg |> Option.value ~default:Odoc_config.empty

(* Removed doc_dir_of_package - was using unused package_info fields *)

let is_package_available t pkg =
  Package.Name.Map.mem t.libs_of_package pkg

module For_tests = struct
  let parse_changes_file = parse_changes_file
  
  let empty = empty
  
  (* Helper to avoid rebuilding file_to_package map multiple times *)
  let with_file_to_package_map ~packages_with_files ~opam_prefix ~f =
    let file_to_package = build_file_to_package_map packages_with_files ~opam_prefix in
    f file_to_package
    
  let build_from_changes_data ~packages_with_files ~opam_prefix ~libs =
    with_file_to_package_map ~packages_with_files ~opam_prefix ~f:(fun file_to_package ->
      build_mappings_from_changes_data ~file_to_package_map:file_to_package libs
    )
  
  let package_of_archive_file ~packages_with_files ~opam_prefix ~archive_file =
    with_file_to_package_map ~packages_with_files ~opam_prefix ~f:(fun file_to_package ->
      Path.Map.find file_to_package archive_file
    )
  
  (* Real filesystem functions *)
  
  let find_opam_root () =
    (* Only detect opam from OPAM_SWITCH_PREFIX environment variable *)
    match Env.get Env.initial Opam_switch.opam_switch_prefix_var_name with
    | None -> None
    | Some candidate ->
        let path = Path.of_string candidate in
        let install_dir = Path.relative path ".opam-switch/install" in
        if Sys.file_exists (Path.to_string install_dir) then Some path else None
  
  let find_ocaml_packages ~opam_root =
    let install_dir = Path.relative opam_root ".opam-switch/install" in
    if not (Sys.file_exists (Path.to_string install_dir)) then []
    else
      let changes_files = 
        match Sys.readdir (Path.to_string install_dir) with
        | files ->
            Array.to_list files
            |> List.filter_map ~f:(fun filename ->
                if String.is_suffix filename ~suffix:".changes" then
                  match String.drop_suffix filename ~suffix:".changes" with
                  | Some pkg_name -> 
                      if String.is_prefix pkg_name ~prefix:"ocaml" then
                        Some (Package.Name.of_string pkg_name)
                      else None
                  | None -> None
                else None)
        | exception _ -> []
      in
      changes_files
  
  let read_real_changes_file ~opam_root ~package_name =
    let changes_file = 
      Path.relative opam_root 
        (sprintf ".opam-switch/install/%s.changes" 
          (Package.Name.to_string package_name))
    in
    if not (Sys.file_exists (Path.to_string changes_file)) then None
    else
      try
        let content = Io.read_file changes_file in
        let files = parse_changes_file ~changes_file_path:changes_file content in
        Some files
      with
      | _ -> None
  
  let find_archive_in_opam ~opam_root ~filename =
    (* Search for the file in the opam installation *)
    let rec search_dir dir =
      if not (Sys.file_exists (Path.to_string dir)) then None
      else
        try
          let entries = Sys.readdir (Path.to_string dir) in
          Array.fold_left entries ~init:None ~f:(fun acc entry ->
            match acc with
            | Some found -> Some found
            | None ->
                let entry_path = Path.relative dir entry in
                if String.equal entry filename then
                  Some entry_path
                else if Sys.is_directory (Path.to_string entry_path) then
                  search_dir entry_path
                else
                  None
          )
        with
        | _ -> None
    in
    search_dir opam_root
  
  let package_of_real_archive ~archive_path =
    match find_opam_root () with
    | None -> None
    | Some opam_root ->
        let ocaml_packages = find_ocaml_packages ~opam_root in
        List.find_map ocaml_packages ~f:(fun pkg ->
          match read_real_changes_file ~opam_root ~package_name:pkg with
          | None -> None
          | Some files ->
              let archive_str = Path.to_string archive_path in
              let matches = List.exists files ~f:(fun file ->
                String.is_suffix archive_str ~suffix:file ||
                String.is_suffix file ~suffix:(Path.basename archive_path)
              ) in
              if matches then Some pkg else None
        )
end