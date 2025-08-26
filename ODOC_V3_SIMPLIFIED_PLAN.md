# Simplified Implementation Plan: Dune odoc v3 Migration
## Assuming odoc v3 Always Available

This document provides a commit-by-commit plan for migrating Dune to odoc v3, assuming odoc v3 is always available (no feature detection or compatibility layers needed).

---

## Phase 0: Foundation (Independent Components)

### Commit: Implement package discovery using opam changes files
**Branch**: `odoc-v3-package-discovery`  
**Files**: `src/dune_rules/package_discovery.ml`, `src/dune_rules/package_discovery.mli`, `src/dune_rules/dune`

**Why first**: This module is completely independent and will be needed by all subsequent v3 changes. It can be merged immediately without affecting any existing functionality.

**Changes**:
```ocaml
(* package_discovery.mli *)
(** Discovers which files belong to which packages using opam changes files.
    This is essential for odoc v3's package-centric directory structure. *)

type t
(** Package discovery context *)

val create : unit -> t
(** Initialize package discovery by reading all changes files *)

val package_of_library : t -> Lib.t -> Package.Name.t option
(** Returns the package that owns a given library *)

val libraries_of_package : t -> Package.Name.t -> Lib.t list
(** Returns all libraries belonging to a package *)

(* package_discovery.ml *)
open Stdune

type t = {
  file_to_package : Package.Name.t Path.Map.t;
  lib_to_package : Package.Name.t Lib_name.Map.t;
  package_to_libs : Lib_name.Set.t Package.Name.Map.t;
}

let read_changes_file pkg_name =
  let opam_switch_prefix = 
    match Env.get Env.initial "OPAM_SWITCH_PREFIX" with
    | Some prefix -> Path.of_string prefix
    | None -> 
        (* Fallback to finding switch from opam *)
        match Sys.getenv_opt "HOME" with
        | Some home -> Path.relative (Path.of_string home) ".opam/default"
        | None -> Path.root
  in
  let changes_file = 
    Path.relative opam_switch_prefix 
      (sprintf ".opam-switch/install/%s.changes" 
        (Package.Name.to_string pkg_name))
  in
  
  match Path.exists changes_file with
  | false -> []
  | true ->
      let content = Io.read_file changes_file in
      (* Parse using vendored opam-format *)
      let changes = OpamFile.Changes.read_from_string content in
      OpamFile.Changes.files changes
      |> OpamStd.String.Map.bindings
      |> List.filter_map (fun (file, change) ->
          match change with
          | OpamDirTrack.Added _ -> Some (Path.of_string file)
          | _ -> None)

let identify_library_from_path path =
  (* Check if this is a .cmi file in lib/ directory *)
  match Path.split_extension path with
  | base, ".cmi" -> (
      match Path.explode path with
      | "lib" :: lib_name :: _ -> Some (Lib_name.of_string lib_name)
      | _ -> None)
  | _ -> None

let create () =
  (* Discover all installed packages *)
  let all_packages = 
    (* Get from opam list or dune project *)
    Dune_project.packages (Dune_project.load Path.Source.root)
    |> Package.Name.Map.keys
  in
  
  (* Build maps *)
  let file_to_package, lib_to_package, package_to_libs =
    List.fold_left all_packages 
      ~init:(Path.Map.empty, Lib_name.Map.empty, Package.Name.Map.empty)
      ~f:(fun (f2p, l2p, p2l) pkg ->
        let files = read_changes_file pkg in
        let f2p = 
          List.fold_left files ~init:f2p ~f:(fun acc file ->
            Path.Map.add_exn acc file pkg)
        in
        let libs_in_pkg = 
          List.filter_map files ~f:identify_library_from_path
        in
        let l2p =
          List.fold_left libs_in_pkg ~init:l2p ~f:(fun acc lib ->
            Lib_name.Map.add_exn acc lib pkg)
        in
        let p2l =
          Package.Name.Map.add_exn p2l pkg (Lib_name.Set.of_list libs_in_pkg)
        in
        (f2p, l2p, p2l))
  in
  { file_to_package; lib_to_package; package_to_libs }

let package_of_library t lib =
  let lib_name = Lib.name lib in
  Lib_name.Map.find t.lib_to_package lib_name

let libraries_of_package t pkg =
  match Package.Name.Map.find t.package_to_libs pkg with
  | None -> []
  | Some set -> 
      Lib_name.Set.to_list set
      |> List.filter_map (fun name ->
          (* Convert lib names back to Lib.t - may need adjustment *)
          Lib.DB.find (Scope.libs scope) name)
```

**Tests**: 
- Test with workspace containing multiple packages
- Test with external opam packages
- Test with missing changes files (fallback behavior)
- Test library-to-package mapping accuracy

**Acceptance**: 
- Can determine package ownership for any library
- Works with both local and external packages
- Handles missing changes files gracefully

---

## Phase 1: Infrastructure Setup

### Commit: Create odoc v3 module interface
**Branch**: `odoc-v3-interface`  
**Files**: `src/dune_rules/odoc_v3.mli`, `src/dune_rules/dune`

**Changes**:
```ocaml
(* odoc_v3.mli *)
module Id : sig
  type t
  val of_string : string -> t
  val of_path : Path.Build.t -> t
  val to_string : t -> string
  val root : t
  val ( / ) : t -> string -> t
end

type compile_kind = 
  | Intf of { hidden : bool }
  | Impl 
  | Mld
  | Asset

val compile :
  output_dir:Path.Build.t ->
  input_file:Path.Build.t ->
  includes:Path.Build.Set.t ->
  parent_id:Id.t ->
  ?warnings_tag:string ->
  kind:compile_kind ->
  unit -> unit

val link :
  input_file:Path.Build.t ->
  output_file:Path.Build.t ->
  includes:Path.Build.t list ->
  docs:(string * Path.Build.t) list ->
  libs:(string * Path.Build.t) list ->
  unit -> unit

val html_generate :
  output_dir:Path.Build.t ->
  input_file:Path.Build.t ->
  ?search_uris:Path.Build.t list ->
  ?as_json:bool ->
  unit -> unit
```

**Tests**: Interface compiles, basic type checking  
**Acceptance**: Interface is complete and well-typed

---

### Commit: Implement odoc v3 Id module and basic functions
**Branch**: `odoc-v3-implementation`  
**Files**: `src/dune_rules/odoc_v3.ml`

**Changes**:
```ocaml
(* odoc_v3.ml *)
open Stdune

module Id = struct
  type t = string
  
  let of_string s = s
  let of_path p = Path.Build.to_string p
  let to_string t = t
  let root = ""
  let ( / ) parent child = 
    match parent with
    | "" -> child
    | p -> p ^ "/" ^ child
end

let run_odoc sctx ~dir cmd =
  let context = Super_context.context sctx in
  Command.run ~dir:(Path.build dir) cmd

let compile ~output_dir ~input_file ~includes ~parent_id ?warnings_tag ~kind () =
  let open Command.Args in
  let cmd = 
    S [ A "odoc"; A "compile" 
      ; A "--output-dir"; Path (Path.build output_dir)
      ; A "--parent-id"; A (Id.to_string parent_id)
      ; S (Path.Build.Set.to_list_map includes ~f:(fun p -> 
          S [A "-I"; Path (Path.build p)]))
      ; (match warnings_tag with Some t -> A ("--warnings-tag=" ^ t) | None -> empty)
      ; (match kind with 
          | Intf {hidden = true} -> A "--hidden"
          | Intf {hidden = false} -> empty  
          | Impl -> empty
          | Mld -> empty
          | Asset -> empty)
      ; Path (Path.build input_file)
      ]
  in
  run_odoc cmd

let link ~input_file ~output_file ~includes ~docs ~libs () =
  let open Command.Args in
  let include_args = 
    List.concat_map includes ~f:(fun p -> [A "-I"; Path (Path.build p)])
  in
  let doc_args =
    List.concat_map docs ~f:(fun (name, path) -> 
      [A "--docs"; A name; Path (Path.build path)])
  in
  let lib_args = 
    List.concat_map libs ~f:(fun (name, path) ->
      [A "--libs"; A name; Path (Path.build path)])
  in
  let cmd =
    S ([ A "odoc"; A "link" 
       ; A "--output-file"; Path (Path.build output_file)
       ; Path (Path.build input_file)
       ] @ include_args @ doc_args @ lib_args)
  in
  run_odoc cmd

let html_generate ~output_dir ~input_file ?search_uris ?as_json () =
  let open Command.Args in
  let search_args = 
    Option.value search_uris ~default:[] 
    |> List.concat_map ~f:(fun uri -> [A "--search-uri"; Path (Path.build uri)])
  in
  let json_arg = if Option.value as_json ~default:false then [A "--as-json"] else [] in
  let cmd =
    S ([ A "odoc"; A "html-generate" 
       ; A "--output-dir"; Path (Path.build output_dir)
       ; Path (Path.build input_file)
       ] @ search_args @ json_arg)
  in
  run_odoc cmd
```

**Tests**: Basic compilation, linking, and HTML generation work  
**Acceptance**: All odoc v3 functions work correctly

---

### Commit: Create package-centric path system
**Branch**: `odoc-v3-paths`  
**Files**: `src/dune_rules/odoc_paths_v3.ml`, `src/dune_rules/odoc_paths_v3.mli`

**Changes**:
```ocaml
(* odoc_paths_v3.mli *)
val doc_root : Context.t -> Path.Build.t
val package_dir : Context.t -> Package.Name.t -> Path.Build.t
val library_dir : Context.t -> Package.Name.t -> Lib_name.t -> Path.Build.t

val odoc_file : Context.t -> Package.Name.t -> Lib_name.t -> Module.t -> Path.Build.t
val odocl_file : Context.t -> Package.Name.t -> Lib_name.t -> Module.t -> Path.Build.t
val html_file : Context.t -> Package.Name.t -> Lib_name.t -> Module.t -> Path.Build.t

val parent_id_of_module : Package.Name.t -> Lib_name.t -> Odoc_v3.Id.t
val parent_id_of_library : Package.Name.t -> Odoc_v3.Id.t
val parent_id_of_package : Odoc_v3.Id.t

(* odoc_paths_v3.ml *)
let doc_root ctx = Path.Build.relative (Context.build_dir ctx) "_doc"

let package_dir ctx pkg = 
  Path.Build.relative (doc_root ctx) (Package.Name.to_string pkg)

let library_dir ctx pkg lib =
  Path.Build.relative (package_dir ctx pkg) (Lib_name.to_string lib)

let odoc_file ctx pkg lib module_ =
  let lib_dir = library_dir ctx pkg lib in
  let basename = Module.name module_ |> Module_name.to_string |> String.uncapitalize_ascii in
  Path.Build.relative lib_dir (basename ^ ".odoc")

let odocl_file ctx pkg lib module_ =
  Path.Build.set_extension (odoc_file ctx pkg lib module_) ~ext:".odocl"

let html_file ctx pkg lib module_ =
  let lib_dir = library_dir ctx pkg lib in
  let basename = Module.name module_ |> Module_name.to_string in
  Path.Build.relative lib_dir (basename ^ ".html")

let parent_id_of_module pkg lib =
  Odoc_v3.Id.(of_string (Package.Name.to_string pkg) / Lib_name.to_string lib)

let parent_id_of_library pkg =
  Odoc_v3.Id.of_string (Package.Name.to_string pkg)

let parent_id_of_package = Odoc_v3.Id.root
```

**Tests**: Path generation tests, parent ID computation tests  
**Acceptance**: All paths generate correctly in package/library structure

---

## Phase 2: Replace odoc.ml Implementation

### Commit: Replace module compilation in odoc.ml
**Branch**: `odoc-v3-replace-modules`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Replace compile_module function *)
let compile_module sctx ~obj_dir (m : Module.t) ~includes:(file_deps, iflags) ~dep_graphs ~pkg_or_lnu ~mode =
  let ctx = Super_context.context sctx in
  
  (* Extract package and library names - critical for correct directory structure *)
  let lib_name = extract_library_name_from_obj_dir obj_dir in
  let pkg_name = match pkg_or_lnu with 
    | `Pkg pkg -> Package.name pkg
    | `Lnu lnu -> 
        (* Use package discovery to find the correct package *)
        match Package_discovery.library_to_package_map () |> Lib_name.Map.find lib_name with
        | Some pkg -> pkg
        | None -> 
            (* Fallback: extract from LNU if no changes file *)
            extract_package_from_lnu lnu
  
  (* Use v3 paths and parent ID *)
  let parent_id = Odoc_paths_v3.parent_id_of_module pkg_name lib_name in
  let output_dir = Odoc_paths_v3.library_dir ctx pkg_name lib_name in
  let input_file = Obj_dir.Module.cmti_file 
    ~cm_kind:(match mode with
      | Lib_mode.Ocaml _ -> Ocaml Cmi
      | Melange -> Melange Cmi)
    obj_dir m in
  
  (* Convert includes *)
  let includes = extract_includes_from_iflags iflags in
  
  (* Use v3 compilation *)
  let odoc_file = Odoc_paths_v3.odoc_file ctx pkg_name lib_name m in
  let+ () =
    let action_with_targets =
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
      >>> Action_builder.with_targets ~targets:[odoc_file] (
        Action_builder.return (
          Odoc_v3.compile ~output_dir ~input_file ~includes ~parent_id
            ~warnings_tag:(Some (extract_warnings_tag pkg_or_lnu))
            ~kind:(Intf {hidden = Module.visibility m <> Public}) ()
        )
      )
    in
    Super_context.add_rule sctx action_with_targets
  in
  m, odoc_file
```

**Tests**: Module compilation works with v3, parent IDs are correct  
**Acceptance**: Modules compile to package/library structure

---

### Commit: Replace .mld compilation in odoc.ml  
**Branch**: `odoc-v3-replace-mld`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Replace compile_mld function *)
let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  let ctx = Super_context.context sctx in
  let pkg_name = Package.name pkg in
  let parent_id = Odoc_paths_v3.parent_id_of_library pkg_name in
  let output_dir = Odoc_paths_v3.package_dir ctx pkg_name in
  let input_file = Mld.odoc_input m in
  let includes = extract_includes_set includes in
  
  let odoc_file = 
    let basename = Mld.odoc_input m |> Path.Build.basename |> Filename.remove_extension in
    Path.Build.relative output_dir ("page-" ^ basename ^ ".odoc")
  in
  
  let+ () = 
    let action = Action_builder.return (
      Odoc_v3.compile ~output_dir ~input_file ~includes ~parent_id ~kind:Mld ()
    ) in
    Super_context.add_rule sctx (Action_builder.with_targets ~targets:[odoc_file] action)
  in
  odoc_file
```

**Tests**: .mld files compile with correct parent IDs  
**Acceptance**: Package documentation generates correctly

---

### Commit: Update alias system for v3 paths
**Branch**: `odoc-v3-update-aliases`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Update Dep module for v3 paths *)
module Dep = struct
  let format_alias f ctx m = 
    let dir = match m with
      | Lib lib -> 
          let pkg = Lib_info.package (Lib.Local.info lib) in
          let lib_name = Lib.name (Lib.Local.to_lib lib) in
          Odoc_paths_v3.library_dir ctx pkg lib_name
      | Pkg pkg ->
          Odoc_paths_v3.package_dir ctx (Package.name pkg)
    in
    Output_format.alias f ~dir

  let alias ctx m = 
    let dir = match m with
      | Lib lib -> 
          let pkg = Lib_info.package (Lib.Local.info lib) in
          let lib_name = Lib.name (Lib.Local.to_lib lib) in
          Odoc_paths_v3.library_dir ctx pkg lib_name
      | Pkg pkg ->
          Odoc_paths_v3.package_dir ctx (Package.name pkg)
    in
    Alias.make (Alias.Name.of_string ".odoc-all") ~dir

  let deps ctx pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | Some p -> 
             let alias = alias ctx (Pkg { Package.name = p; dir = Path.Source.root; }) in
             Dep.Set.singleton (Dep.alias alias)
         | None -> Dep.Set.empty
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match Lib.Local.of_lib lib with
         | None -> acc
         | Some lib ->
             let alias = alias ctx (Lib lib) in
             Dep.Set.add acc (Dep.alias alias)))

  let setup_deps ctx m files =
    Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
end
```

**Tests**: Aliases work with v3 directory structure  
**Acceptance**: Library dependencies still enforced correctly via .odoc-all

---

### Commit: Update linking in odoc.ml
**Branch**: `odoc-v3-replace-linking`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Replace link_odoc_rules function *)
let link_odoc_rules sctx (odoc_file : odoc_artefact) ~pkg ~requires =
  let ctx = Super_context.context sctx in
  let deps = Dep.deps ctx pkg requires in
  
  (* Use v3 linking *)
  let libs = compute_lib_paths_v3 ctx requires in
  let docs = compute_doc_paths_v3 ctx pkg in
  let includes = compute_include_paths_v3 ctx requires in
  
  let link_action = Action_builder.return (
    Odoc_v3.link 
      ~input_file:odoc_file.odoc_file
      ~output_file:odoc_file.odocl_file  
      ~includes ~docs ~libs ()
  ) in
  
  Super_context.add_rule sctx
    (let open Action_builder.With_targets.O in
     Action_builder.with_no_targets deps >>> 
     Action_builder.with_targets ~targets:[odoc_file.odocl_file] link_action)
```

**Tests**: Linking works with v3 API  
**Acceptance**: Cross-references resolve correctly

---

## Phase 3: Replace odoc_new.ml Implementation  

### Commit: Simplify odoc_new.ml to package-centric structure
**Branch**: `odoc-v3-simplify-new`  
**Files**: `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Remove hierarchical categories, use package structure *)

(* Replace complex Index system with simple package-based one *)
module Package_index = struct
  type t = Package.Name.t
  
  let of_package pkg = pkg
  let odoc_dir ctx ~all pkg = Odoc_paths_v3.package_dir ctx pkg
  let parent_id pkg = Odoc_paths_v3.parent_id_of_library pkg
end

(* Simplify compile_odocs function *)
let compile_odocs sctx ~all ~quiet artifacts parent libs =
  let* requires = compute_requires_v3 libs in
  
  Memo.parallel_iter artifacts ~f:(fun a ->
    match Artifact.artifact_ty a with
    | Module _ ->
        let pkg_name, lib_name = extract_package_lib_from_artifact a in
        let parent_id = Odoc_paths_v3.parent_id_of_module pkg_name lib_name in
        let output_dir = Odoc_paths_v3.library_dir (Super_context.context sctx) pkg_name lib_name in
        
        (* Use odoc compile-deps for module dependencies *)
        let* deps_file = run_odoc_compile_deps a in  
        let module_deps = parse_and_resolve_deps deps_file artifacts in
        
        Action_builder.with_no_targets module_deps >>>
        Action_builder.return (
          Odoc_v3.compile ~output_dir ~input_file:(Artifact.source_file a) 
            ~includes:Fpath.Set.empty ~parent_id ~kind:(determine_kind a) ()
        )
    | Mld ->
        let pkg_name = extract_package_from_artifact a in
        let parent_id = Odoc_paths_v3.parent_id_of_library pkg_name in
        let output_dir = Odoc_paths_v3.package_dir (Super_context.context sctx) pkg_name in
        
        Action_builder.return (
          Odoc_v3.compile ~output_dir ~input_file:(Artifact.source_file a)
            ~includes:Fpath.Set.empty ~parent_id ~kind:Mld ()
        ))
```

**Tests**: Package-centric structure works  
**Acceptance**: External libraries organize under package structure, no hierarchical categories

---

### Commit: Remove hierarchical compilation dependencies
**Branch**: `odoc-v3-remove-hierarchical-deps`  
**Files**: `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Remove all Hidden_deps on parent files *)
(* Remove parent_args and child registration *)
(* Simplify to direct compilation with parent_id *)

let compile_mld sctx a ~package_name =
  let ctx = Super_context.context sctx in
  let parent_id = Odoc_paths_v3.parent_id_of_library package_name in
  let output_dir = Odoc_paths_v3.package_dir ctx package_name in
  
  (* No parent deps - just compile directly *)
  let compile_action = Action_builder.return (
    Odoc_v3.compile ~output_dir ~input_file:(Artifact.source_file a) 
      ~includes:Fpath.Set.empty ~parent_id ~kind:Mld ()
  ) in
  
  let odoc_file = compute_odoc_file_path a in
  Super_context.add_rule sctx (Action_builder.with_targets ~targets:[odoc_file] compile_action);
  odoc_file

(* Remove hierarchical index generation - replace with simple package indexes *)
let generate_package_index sctx pkg_name libs =
  let ctx = Super_context.context sctx in
  let parent_id = Odoc_paths_v3.parent_id_of_package in
  let output_dir = Odoc_paths_v3.package_dir ctx pkg_name in
  
  (* Generate simple package index listing libraries *)
  let index_content = generate_simple_package_index pkg_name libs in
  let index_mld = Path.Build.relative output_dir "index.mld" in
  
  Super_context.add_rule sctx (Action_builder.write_file index_mld index_content);
  
  (* Compile the index *)
  Odoc_v3.compile ~output_dir ~input_file:index_mld 
    ~includes:Fpath.Set.empty ~parent_id ~kind:Mld ()
```

**Tests**: No compilation dependencies on parent index files  
**Acceptance**: Hierarchical compilation constraints removed, performance improved

---

## Phase 4: HTML Generation Migration

### Commit: Update HTML generation for package structure
**Branch**: `odoc-v3-html-generation`  
**Files**: `src/dune_rules/odoc.ml`, `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Update HTML file path generation *)
let setup_generate sctx ~search_db odoc_file out =
  let ctx = Super_context.context sctx in
  
  (* Use v3 HTML generation *)
  let output_dir = match out with
    | Html -> determine_html_output_dir_v3 ctx odoc_file
    | Json -> determine_json_output_dir_v3 ctx odoc_file
  in
  
  let search_uris = if search_db then [compute_search_uri ctx] else [] in
  let as_json = match out with Json -> true | Html -> false in
  
  let generate_action = Action_builder.return (
    Odoc_v3.html_generate ~output_dir ~input_file:odoc_file.odocl_file 
      ~search_uris ~as_json ()
  ) in
  
  let html_targets = compute_html_targets_v3 odoc_file out in
  Super_context.add_rule sctx (Action_builder.with_targets ~targets:html_targets generate_action)

(* Update CSS and support files *)
let setup_css_rule sctx =
  let ctx = Super_context.context sctx in
  let support_dir = Path.Build.relative (Odoc_paths_v3.doc_root ctx) "odoc.support" in
  
  (* Copy support files to doc root *)
  copy_support_files_to support_dir
```

**Tests**: HTML generates in correct package/library structure  
**Acceptance**: HTML files appear in package/library directories with correct navigation

---

### Commit: Update search integration  
**Branch**: `odoc-v3-search`  
**Files**: `src/dune_rules/sherlodoc.ml`

**Changes**:
```ocaml
(* Update search database generation for v3 structure *)
let search_db sctx ~dir ~external_odocls odocls =
  let ctx = Super_context.context sctx in
  let search_dir = Odoc_paths_v3.doc_root ctx in
  
  (* Generate search database with package-centric paths *)
  let all_odocls = external_odocls @ odocls in
  let search_file = Path.Build.relative search_dir "search-data.js" in
  
  let generate_search = Action_builder.return (
    generate_search_database_v3 all_odocls search_file
  ) in
  
  Super_context.add_rule sctx (Action_builder.with_targets ~targets:[search_file] generate_search);
  search_file

(* Update search URI generation *)
let odoc_args sctx ~search_db ~dir_sherlodoc_dot_js =
  let ctx = Super_context.context sctx in
  let search_uri = Path.Build.relative (Odoc_paths_v3.doc_root ctx) "search-data.js" in
  [ "--search-uri"; Path.Build.to_string search_uri ]
```

**Tests**: Search works with new directory structure  
**Acceptance**: Search functionality preserved and works across package boundaries

---

## Phase 5: Integration and Polish

### Commit: Add comprehensive test suite for v3
**Branch**: `odoc-v3-tests`  
**Files**: `test/blackbox-tests/test-cases/odoc-v3/`, multiple test files

**Changes**:
```bash
# Add comprehensive test cases covering:
test/blackbox-tests/test-cases/odoc-v3/
├── basic-package/           # Single package with libraries
│   ├── dune-project
│   ├── lib1/dune
│   ├── lib2/dune  
│   └── doc/package.mld
├── multi-package/          # Multiple packages in workspace
├── virtual-libraries/      # Virtual library handling
├── external-deps/          # External library integration  
├── private-libs/           # Private library documentation
├── json-output/            # JSON format testing
├── search-integration/     # Search functionality
├── cross-references/       # Inter-package references
└── complex-hierarchy/      # Nested package structures
```

**Tests**: All test cases pass, cover edge cases  
**Acceptance**: Comprehensive test coverage validates all v3 functionality

---

### Commit: Add performance benchmarks and validation
**Branch**: `odoc-v3-benchmarks`  
**Files**: `bench/odoc-v3-performance.ml`

**Changes**:
```ocaml
(* Performance validation *)
let benchmark_compilation_performance () =
  let large_project = setup_large_test_project () in
  
  (* Measure compilation time *)
  let start_time = Unix.gettimeofday () in
  let compiled_units = compile_all_units large_project in
  let compile_time = Unix.gettimeofday () -. start_time in
  
  (* Measure linking time *)  
  let start_time = Unix.gettimeofday () in
  let linked_units = link_all_units compiled_units in
  let link_time = Unix.gettimeofday () -. start_time in
  
  (* Measure HTML generation time *)
  let start_time = Unix.gettimeofday () in
  generate_all_html linked_units;
  let html_time = Unix.gettimeofday () -. start_time in
  
  Printf.printf "Performance Results:\n";
  Printf.printf "  Compilation: %.2fs\n" compile_time;
  Printf.printf "  Linking: %.2fs\n" link_time;  
  Printf.printf "  HTML Generation: %.2fs\n" html_time;
  Printf.printf "  Total: %.2fs\n" (compile_time +. link_time +. html_time)

let validate_output_structure () =
  (* Validate that output follows package/library structure *)
  let output_dir = get_html_output_dir () in
  assert (directory_structure_matches_expected output_dir);
  
  (* Validate cross-references work *)
  assert (all_cross_references_resolve output_dir);
  
  (* Validate search functionality *)
  assert (search_database_works output_dir)
```

**Tests**: Performance benchmarks run successfully  
**Acceptance**: V3 shows performance improvement, output structure is correct

---

### Commit: Update documentation and examples
**Branch**: `odoc-v3-docs`  
**Files**: `doc/odoc.rst`, `CHANGES.md`, examples/

**Changes**:
- Update documentation to reflect new package/library structure
- Add migration notes for URL changes
- Update examples to use new directory layout  
- Document performance improvements
- Add troubleshooting guide for common issues

**Tests**: Documentation builds correctly, examples work  
**Acceptance**: Users can understand new structure and migrate successfully

---

### Commit: Enable v3 as default implementation
**Branch**: `odoc-v3-enable-default`  
**Files**: `src/dune_rules/odoc.ml`, `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Remove old implementation, make v3 the only implementation *)
(* Clean up unused code *)
(* Remove migration flags *)

(* Update default paths to use v3 structure *)
let default_doc_dir = Odoc_paths_v3.doc_root
let default_package_structure = true
```

**Tests**: All existing functionality works with v3 as default  
**Acceptance**: Seamless transition to v3 for all users

---

## Testing Strategy

### Per-Commit Testing
Each commit must pass:
- [ ] `dune build` (no compilation errors)
- [ ] `dune runtest` (all tests pass)  
- [ ] `dune build @doc` (documentation builds successfully)
- [ ] New commit-specific tests pass

### Integration Testing  
After groups of commits:
- [ ] Real-world project testing (dune, lwt, core, etc.)
- [ ] Performance validation (no regressions)
- [ ] Cross-platform compatibility (Linux, macOS, Windows)
- [ ] Documentation quality validation

### Pre-Release Validation
Before merge to main:
- [ ] Complete test suite passes
- [ ] Performance improvements validated
- [ ] Output structure matches specification
- [ ] Documentation is complete and accurate
- [ ] No regressions in functionality

## Risk Mitigation

### Medium-Risk Areas
- **Phase 2**: Major implementation replacement
- **Phase 4**: HTML generation changes may affect styling
- **Phase 5**: Default enabling affects all users

### Rollback Strategy
- Each commit is atomic and can be reverted
- Phase-by-phase rollback possible
- Performance monitoring throughout
- Comprehensive testing before each merge

### Success Criteria
- **Performance**: 20-30% improvement in build times
- **Structure**: Clean package/library directory layout
- **Compatibility**: All existing features preserved
- **Quality**: Documentation quality maintained or improved

## Timeline

**Total**: ~8-10 weeks (simplified from original 12-16 weeks)
- **Phase 1** (Infrastructure): 2 weeks
- **Phase 2** (Replace odoc.ml): 2-3 weeks  
- **Phase 3** (Replace odoc_new.ml): 2 weeks
- **Phase 4** (HTML Generation): 1-2 weeks
- **Phase 5** (Polish): 1-2 weeks

This simplified approach eliminates the complexity of maintaining dual implementations and version detection, making the migration more straightforward and faster to implement.