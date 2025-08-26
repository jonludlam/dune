# Detailed Implementation Plan: Dune odoc v3 Migration
## Commit-by-Commit Execution Strategy

This document provides a detailed, commit-by-commit plan for migrating Dune to odoc v3. Each commit is designed to be atomic, testable, and reviewable.

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
```

**Tests**: Interface compiles, basic type checking  
**Acceptance**: Interface is complete and well-typed

---

### Commit: Implement odoc v3 Id module
**Branch**: `odoc-v3-id-impl`  
**Files**: `src/dune_rules/odoc_v3.ml`

**Changes**:
```ocaml
(* odoc_v3.ml - Id module implementation *)
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
```

**Tests**: Unit tests for Id operations, path conversion  
**Acceptance**: All Id functions work correctly, tests pass

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

let parent_id_of_module pkg lib =
  Odoc_v3.Id.(of_string (Package.Name.to_string pkg) / Lib_name.to_string lib)

let parent_id_of_library pkg =
  Odoc_v3.Id.of_string (Package.Name.to_string pkg)

let parent_id_of_package = Odoc_v3.Id.root
```

**Tests**: Path generation tests, parent ID computation tests  
**Acceptance**: All paths generate correctly, no conflicts with existing code

---

### Commit: Implement basic odoc v3 compile function
**Branch**: `odoc-v3-compile`  
**Files**: `src/dune_rules/odoc_v3.ml` (continued)

**Changes**:
```ocaml
(* Add to odoc_v3.ml *)
let run_odoc sctx ~dir cmd =
  let context = Super_context.context sctx in
  let env = Env.empty in
  Command.run ~dir:(Path.build dir) ~env cmd

let compile ~output_dir ~input_file ~includes ~parent_id ?warnings_tag ~kind () =
  let open Command.Args in
  let cmd = 
    S [ A "odoc"; A "compile" 
      ; A "--output-dir"; Path (Path.build output_dir)
      ; A "--parent-id"; A (Id.to_string parent_id)
      ; S (Path.Build.Set.to_list_map includes ~f:(fun p -> 
          S [A "-I"; Path (Path.build p)]))
      ; match warnings_tag with Some t -> A ("--warnings-tag=" ^ t) | None -> Empty
      ; A (match kind with 
          | Intf {hidden = true} -> "--hidden"
          | Intf {hidden = false} -> ""  
          | Impl -> "--impl"
          | Mld -> ""
          | Asset -> "")
      ; Path (Path.build input_file)
      ]
  in
  ignore (run_odoc cmd)
```

**Tests**: Test compilation with various flags and parent IDs  
**Acceptance**: Basic compilation works, all flags handled correctly

---

### Commit: Implement odoc v3 link function  
**Branch**: `odoc-v3-link`  
**Files**: `src/dune_rules/odoc_v3.ml` (continued)

**Changes**:
```ocaml
(* Add to odoc_v3.ml *)
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
  ignore (run_odoc cmd)
```

**Tests**: Test linking with dependencies  
**Acceptance**: Linking works with proper include paths

---

## Phase 2: Core Implementation Migration

### Commit: Prepare odoc.ml for v3 migration
**Branch**: `odoc-v3-prepare-migration`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Add v3 flag preparation - we'll use this to gradually migrate functions *)
let migrate_to_v3 = ref false (* Start with v2, will flip to true later *)

(* Add helper functions for v3 migration *)
let use_v3_for_compilation () = !migrate_to_v3
let use_v3_for_linking () = !migrate_to_v3
let use_v3_for_html () = !migrate_to_v3

(* No functional changes yet - just preparation for migration *)
```

**Tests**: No functional changes, code still compiles  
**Acceptance**: Preparation complete, ready for gradual migration

---

### Commit: Implement v3 module compilation in odoc.ml
**Branch**: `odoc-v3-modules`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Replace placeholder in compile_module *)
let compile_module sctx ~obj_dir (m : Module.t) ~includes:(file_deps, iflags) ~dep_graphs ~pkg_or_lnu ~mode =
  if use_v3 then
    (* V3 implementation *)
    let ctx = Super_context.context sctx in
    let pkg_name = extract_package_name pkg_or_lnu in
    let lib_name = extract_library_name obj_dir in
    let parent_id = Odoc_paths_v3.parent_id_of_module pkg_name lib_name in
    let output_dir = Odoc_paths_v3.library_dir ctx pkg_name lib_name in
    let input_file = Obj_dir.Module.cmti_file obj_dir m in
    let includes = extract_includes_from_iflags iflags in
    Odoc_v3.compile ~output_dir ~input_file ~includes ~parent_id 
      ~kind:(Intf {hidden = not (Module.visibility m = Public)}) ()
  else
    (* Existing implementation unchanged *)
```

**Tests**: V3 module compilation works, generates correct parent IDs  
**Acceptance**: Modules compile with v3 when available

---

### Commit 9: Remove Hidden_deps from v3 compilation
**Branch**: `odoc-v3-9-remove-hidden-deps`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Update compile_module to not add Hidden_deps when using v3 *)
let compile_module sctx ~obj_dir (m : Module.t) ~includes:(file_deps, iflags) ~dep_graphs ~pkg_or_lnu ~mode =
  let odoc_file = Obj_dir.Module.odoc obj_dir m in
  let action_builder = 
    if use_v3 then
      (* V3: No hidden deps on parent files *)
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
      >>> compile_v3_action
    else
      (* V2: Keep existing hidden deps *)
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets (module_deps m ~obj_dir ~dep_graphs)
      >>> Action_builder.with_no_targets (parent_deps m ~parent_context)
      >>> compile_v2_action
  in
  add_rule sctx action_builder
```

**Tests**: No hidden dependencies on parent files in v3 mode  
**Acceptance**: Compilation dependencies correctly reduced

---

### Commit 10: Implement v3 .mld compilation
**Branch**: `odoc-v3-10-v3-mld`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Update compile_mld function *)
let compile_mld sctx (m : Mld.t) ~includes ~doc_dir ~pkg =
  if use_v3 then
    let ctx = Super_context.context sctx in
    let pkg_name = Package.name pkg in
    let parent_id = Odoc_paths_v3.parent_id_of_library pkg_name in
    let output_dir = Odoc_paths_v3.package_dir ctx pkg_name in
    let input_file = Mld.odoc_input m in
    let includes = extract_includes_set includes in
    Odoc_v3.compile ~output_dir ~input_file ~includes ~parent_id ~kind:Mld ()
  else
    (* Existing implementation *)
    let odoc_file = Mld.odoc_file m ~doc_dir in
    (* ... existing code ... *)
```

**Tests**: .mld files compile with correct parent IDs  
**Acceptance**: Package documentation generates correctly

---

### Commit 11: Update alias system for v3 paths
**Branch**: `odoc-v3-11-update-aliases`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Update Dep module for v3 paths *)
module Dep = struct
  let format_alias f ctx m = 
    let dir = 
      if use_v3 then
        match m with
        | Lib lib -> 
            let pkg = Lib_info.package (Lib.Local.info lib) in
            let lib_name = Lib.name (Lib.Local.to_lib lib) in
            Odoc_paths_v3.library_dir ctx pkg lib_name
        | Pkg pkg ->
            Odoc_paths_v3.package_dir ctx (Package.name pkg)
      else
        (* Existing path logic *)
        Paths.html ctx m
    in
    Output_format.alias f ~dir

  let deps ctx pkg requires =
    if use_v3 then
      (* Use v3 path system for dependencies *)
      let open Action_builder.O in
      let* libs = Resolve.read requires in
      Action_builder.deps (compute_v3_deps ctx pkg libs)
    else
      (* Existing implementation *)
```

**Tests**: Aliases work with v3 directory structure  
**Acceptance**: Library dependencies still enforced correctly

---

### Commit 12: Migrate odoc_new.ml to package-centric structure
**Branch**: `odoc-v3-12-new-package-centric`  
**Files**: `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Update hierarchical index system *)
let use_v3 = Odoc_config.supports_parent_id ()

(* Replace hierarchical categories with package structure *)
module Index_v3 = struct
  type t = Package of Package.Name.t | Library of Package.Name.t * Lib_name.t
  
  let odoc_dir ctx ~all = function
    | Package pkg -> Odoc_paths_v3.package_dir ctx pkg
    | Library (pkg, lib) -> Odoc_paths_v3.library_dir ctx pkg lib
    
  let parent_id = function
    | Package _ -> Odoc_paths_v3.parent_id_of_package
    | Library (pkg, _) -> Odoc_paths_v3.parent_id_of_library pkg
end

(* Update compile_odocs to use v3 when available *)
let compile_odocs sctx ~all ~quiet artifacts parent libs =
  if use_v3 then
    (* Use package-centric structure *)
    compile_odocs_v3 sctx artifacts parent libs
  else
    (* Existing hierarchical implementation *)
    compile_odocs_v2 sctx ~all ~quiet artifacts parent libs
```

**Tests**: Package-centric structure works  
**Acceptance**: External libraries organize under package structure

---

### Commit 13: Remove hierarchical compilation deps from odoc_new.ml
**Branch**: `odoc-v3-13-remove-hierarchical-deps`  
**Files**: `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Update compile_mld function *)
let compile_mld sctx a ~parent_opt ~is_index ~children =
  if use_v3 then
    (* V3: No parent deps, just use parent_id *)
    let parent_id = compute_v3_parent_id a in
    let output_dir = compute_v3_output_dir a in
    Odoc_v3.compile ~output_dir ~input_file:(Artifact.source_file a) 
      ~includes:Fpath.Set.empty ~parent_id ~kind:Mld ()
  else
    (* V2: Keep existing parent/child system *)
    let parent_args = parent_args parent_opt in
    let child_args = compute_child_args children in
    Odoc.run_odoc sctx ~dir:(Path.build doc_dir) "compile"
      (Command.Args.A "-o" :: Target odoc_file :: Dep odoc_input 
       :: As child_args :: quiet_arg :: parent_args)
```

**Tests**: No compilation dependencies on parent index files  
**Acceptance**: Hierarchical compilation constraints removed

---

## Phase 3: HTML Generation Migration

### Commit 14: Update HTML path generation for v3
**Branch**: `odoc-v3-14-html-paths`  
**Files**: `src/dune_rules/odoc.ml`, `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Update HTML file path generation *)
let html_file_path ctx pkg lib module_name =
  if use_v3 then
    (* V3: package/library/module.html *)
    let pkg_dir = Odoc_paths_v3.package_dir ctx pkg in
    let lib_dir = Path.Build.relative pkg_dir (Lib_name.to_string lib) in
    Path.Build.relative lib_dir (Module_name.to_string module_name ^ ".html")
  else
    (* V2: existing path logic *)
    existing_html_path_logic ctx pkg lib module_name

(* Update setup_generate function *)
let setup_generate sctx ~search_db odoc_file out =
  let ctx = Super_context.context sctx in
  let html_file = 
    if use_v3 then
      compute_v3_html_path odoc_file
    else
      compute_v2_html_path odoc_file
  in
  (* ... rest of generation logic ... *)
```

**Tests**: HTML generates in correct package/library structure  
**Acceptance**: HTML files appear in package-centric directories

---

### Commit 15: Update CSS and asset handling for v3
**Branch**: `odoc-v3-15-assets`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Update CSS path calculation *)
let setup_css_rule sctx =
  let ctx = Super_context.context sctx in
  let css_dir = 
    if use_v3 then
      (* V3: CSS at doc root *)
      Odoc_paths_v3.doc_root ctx
    else
      (* V2: existing location *)
      Paths.html_root ctx
  in
  (* Generate CSS copying rules with correct relative paths *)

(* Update support file generation *)
let support_files_rules ctx =
  let support_dir = 
    if use_v3 then
      Path.Build.relative (Odoc_paths_v3.doc_root ctx) "odoc.support"
    else
      Paths.odoc_support ctx
  in
  (* Copy support files to correct location *)
```

**Tests**: CSS and JS files work with new directory structure  
**Acceptance**: Styling and functionality preserved

---

### Commit 16: Update index generation for package structure
**Branch**: `odoc-v3-16-indexes`  
**Files**: `src/dune_rules/odoc.ml`, `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Update default_index generation *)
let default_index ~pkg entry_modules =
  if use_v3 then
    (* V3: Package-focused index *)
    let b = Buffer.create 512 in
    Printf.bprintf b "{0 %s}\n" (Package.Name.to_string pkg);
    Printf.bprintf b "This package contains the following libraries:\n\n";
    List.iter entry_modules ~f:(fun (lib, modules) ->
      Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string (Lib.name lib));
      Printf.bprintf b "{{!%s}Library documentation}\n\n" 
        (Lib_name.to_string (Lib.name lib)));
    Buffer.contents b
  else
    (* V2: existing index generation *)
    existing_default_index ~pkg entry_modules

(* Update package index rules *)
let setup_package_index_rules sctx pkg =
  if use_v3 then
    setup_v3_package_index sctx pkg
  else
    setup_v2_package_index sctx pkg
```

**Tests**: Package indexes list libraries correctly  
**Acceptance**: Navigation works in new structure

---

### Commit 17: Update search integration for v3
**Branch**: `odoc-v3-17-search`  
**Files**: `src/dune_rules/sherlodoc.ml`

**Changes**:
```ocaml
(* Update search database generation for v3 *)
let search_db sctx ~dir ~external_odocls odocls =
  let ctx = Super_context.context sctx in
  let search_dir = 
    if Odoc_config.supports_parent_id () then
      (* V3: Search at doc root *)
      Odoc_paths_v3.doc_root ctx
    else
      (* V2: existing location *)
      dir
  in
  (* Generate search database with correct base paths *)

(* Update search URI generation *)
let odoc_args sctx ~search_db ~dir_sherlodoc_dot_js =
  if Odoc_config.supports_parent_id () then
    (* V3: Adjust search paths for new structure *)
    [ "--search-uri"; compute_v3_search_uri search_db ]
  else
    (* V2: existing search args *)
    existing_odoc_args sctx ~search_db ~dir_sherlodoc_dot_js
```

**Tests**: Search works with new directory structure  
**Acceptance**: Search functionality preserved

---

## Phase 4: Integration and Testing

### Commit 18: Add comprehensive test suite for v3
**Branch**: `odoc-v3-18-tests`  
**Files**: `test/blackbox-tests/test-cases/odoc-v3/`, multiple test files

**Changes**:
```bash
# Add test cases for:
test/blackbox-tests/test-cases/odoc-v3/
├── basic-package/           # Single package with libraries
├── multi-package/          # Multiple packages
├── virtual-libraries/      # Virtual library handling
├── external-deps/          # External library integration  
├── private-libs/           # Private library documentation
├── json-output/            # JSON format testing
├── search-integration/     # Search functionality
└── migration-compat/       # V2->V3 compatibility
```

**Tests**: Comprehensive test coverage for all v3 features  
**Acceptance**: All tests pass, edge cases handled

---

### Commit 19: Add performance benchmarks
**Branch**: `odoc-v3-19-benchmarks`  
**Files**: `bench/`, `bench/odoc-v3-bench.ml`

**Changes**:
```ocaml
(* Performance comparison between v2 and v3 *)
let benchmark_compilation () =
  let ctx = create_test_context () in
  let packages = create_test_packages () in
  
  (* Benchmark v2 *)
  let v2_time = time_compilation ~use_v3:false packages in
  
  (* Benchmark v3 *)  
  let v3_time = time_compilation ~use_v3:true packages in
  
  Printf.printf "V2 time: %.2fs, V3 time: %.2fs, Improvement: %.1f%%\n"
    v2_time v3_time ((v2_time -. v3_time) /. v2_time *. 100.)
```

**Tests**: Performance benchmarks run successfully  
**Acceptance**: V3 shows measurable performance improvements

---

### Commit 20: Add feature flag and configuration
**Branch**: `odoc-v3-20-config`  
**Files**: `src/dune_rules/dune_env.ml`, `doc/odoc-v3.rst`

**Changes**:
```ocaml
(* Add environment variable support *)
let use_odoc_v3 () =
  match Env.get Env.initial "DUNE_ODOC_V3" with
  | Some "1" | Some "true" -> true
  | Some "0" | Some "false" -> false  
  | None -> Odoc_config.supports_parent_id ()
  | Some _ -> 
      User_error.raise
        [ Pp.text "DUNE_ODOC_V3 must be '1', 'true', '0', or 'false'" ]

(* Add workspace configuration *)
let workspace_config = {
  odoc_version = Some V3;
  enable_odoc_v3 = true;
}
```

**Tests**: Configuration options work correctly  
**Acceptance**: Users can control v3 usage

---

### Commit 21: Add migration utilities
**Branch**: `odoc-v3-21-migration`  
**Files**: `bin/dune_migrate_odoc_v3.ml`, `src/dune_rules/odoc_migration.ml`

**Changes**:
```ocaml
(* URL migration tool *)
let migrate_url old_url =
  match parse_old_url old_url with
  | Some (Local (pkg, lib, module_name)) ->
      sprintf "%s/%s/%s.html" pkg lib module_name
  | Some (External (category, pkg, lib, module_name)) ->
      sprintf "%s/%s/%s.html" pkg lib module_name  
  | None -> old_url

(* Documentation migration checker *)
let check_migration_compatibility project_root =
  (* Scan for potential issues *)
  (* Report breaking changes *)
  (* Suggest fixes *)
```

**Tests**: Migration utilities work on real projects  
**Acceptance**: Migration path is clear and automated

---

## Phase 5: Deployment and Rollout

### Commit 22: Update documentation
**Branch**: `odoc-v3-22-docs`  
**Files**: `doc/odoc.rst`, `CHANGES.md`, `doc/odoc-v3-migration.rst`

**Changes**:
- Comprehensive documentation of v3 features
- Migration guide with examples
- Troubleshooting section
- Performance comparison data
- Breaking changes clearly documented

**Tests**: Documentation builds correctly  
**Acceptance**: Users can understand and use new features

---

### Commit 23: Enable v3 by default (behind feature detection)
**Branch**: `odoc-v3-23-default`  
**Files**: `src/dune_rules/odoc_config.ml`

**Changes**:
```ocaml
(* Make v3 the default when available *)
let use_odoc_v3 () =
  match Env.get Env.initial "DUNE_ODOC_V2" with
  | Some "1" | Some "true" -> false  (* Explicit fallback *)
  | _ -> Odoc_config.supports_parent_id ()
```

**Tests**: V3 used by default when odoc supports it  
**Acceptance**: Seamless upgrade for users with odoc v3

---

### Commit 24: Add deprecation warnings for manual v2 usage
**Branch**: `odoc-v3-24-deprecation`  
**Files**: `src/dune_rules/odoc.ml`

**Changes**:
```ocaml
(* Warn when forcing v2 with v3 available *)
let warn_v2_usage () =
  if Odoc_config.supports_parent_id () && force_v2_mode () then
    User_warning.emit
      [ Pp.text "Using odoc v2 mode with v3 available."
      ; Pp.text "Consider upgrading to v3 for better performance."
      ; Pp.text "Set DUNE_ODOC_V3=1 to enable v3."
      ]
```

**Tests**: Deprecation warnings appear correctly  
**Acceptance**: Users encouraged to upgrade

---

### Commit 25: Remove legacy v2 implementation (future)
**Branch**: `odoc-v3-25-cleanup` *(Not for initial release)*  
**Files**: `src/dune_rules/odoc.ml`, `src/dune_rules/odoc_new.ml`

**Changes**:
```ocaml
(* Remove all v2-specific code paths *)
(* Simplify implementations *)  
(* Remove feature flags *)
```

**Timeline**: 2-3 releases after v3 deployment  
**Acceptance**: Codebase significantly simplified

---

## Testing Strategy

### Per-Commit Testing
Each commit must pass:
- [ ] `dune build` (no compilation errors)
- [ ] `dune runtest` (existing tests pass)  
- [ ] `dune build @doc` (documentation generation works)
- [ ] Commit-specific tests pass

### Integration Testing  
After groups of commits:
- [ ] Test with real-world projects (dune, lwt, core, etc.)
- [ ] Performance regression testing
- [ ] Documentation quality validation
- [ ] Cross-platform testing (Linux, macOS, Windows)

### Pre-Release Testing
Before merge to main:
- [ ] Full test suite passes
- [ ] Performance improvements validated  
- [ ] Migration path tested with multiple projects
- [ ] Documentation complete and accurate

## Risk Mitigation

### High-Risk Commits
- **Commits 7-9**: Version switching and Hidden_deps removal
- **Commits 12-13**: Major odoc_new.ml restructuring  
- **Commits 14-16**: HTML generation changes

### Rollback Strategy
Each phase can be rolled back independently:
- Phase 1: Remove infrastructure, no user impact
- Phase 2: Disable feature flag, revert to v2
- Phase 3: HTML generation reverts cleanly  
- Phase 4-5: Remove features and configuration

### Validation Gates
- Performance must not regress
- All existing features must work
- Documentation quality must be preserved
- Migration path must be clear

## Timeline

**Total**: ~12-16 weeks
- **Phase 1** (Commits 1-6): 3 weeks
- **Phase 2** (Commits 7-13): 4-5 weeks  
- **Phase 3** (Commits 14-17): 3 weeks
- **Phase 4** (Commits 18-21): 2-3 weeks
- **Phase 5** (Commits 22-24): 2 weeks

Each commit represents ~2-5 days of work including implementation, testing, and review.