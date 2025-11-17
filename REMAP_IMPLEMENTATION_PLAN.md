# Implementation Plan: Remap Facility for Dune Odoc Rules

## Overview

**Goal:** Implement remap facility similar to odoc_driver.ml to build HTML for local packages only, with links to dependencies remapped to ocaml.org.

**Current State:**
- All docs built locally in `_doc/_html/` with relative links
- `@doc` alias includes all transitive dependencies (local + installed)
- No remap support - all packages are built and linked locally

**Target State:**
- **`@doc`**: Local packages only → `_doc/_html/`, links to dependencies remapped to ocaml.org
- **`@doc-full`**: All packages (current behavior) → `_doc/_html_full/`, relative links
- Shared code path with mode selection parameter

---

## Implementation Strategy

### Phase 1: Core Infrastructure (Refactoring for Code Reuse)

#### 1.1 Define Documentation Mode Type

Add near `Output_format` module ([odoc.ml:233-270](src/dune_rules/odoc.ml#L233-L270)):

```ocaml
module Doc_mode = struct
  type t =
    | Local_only  (* @doc - only local packages, with remapping *)
    | Full        (* @doc-full - all packages, no remapping *)

  let output_subdir = function
    | Local_only -> "_html"
    | Full -> "_html_full"

  let alias_name output_format = function
    | Local_only -> Output_format.alias output_format  (* @doc, @doc-json *)
    | Full ->
      match output_format with
      | Html -> Alias.make "doc-full"
      | Json -> Alias.make "doc-json-full"

  let all = [Local_only; Full]
end
```

#### 1.2 Update `Paths` Module

Refactor to accept `Doc_mode.t` ([odoc.ml:164-203](src/dune_rules/odoc.ml#L164-L203)):

```ocaml
module Paths = struct
  let root ctx = Path.Build.relative (Context.build_dir ctx) "_doc"

  let html_root ctx mode =
    root ctx ++ Doc_mode.output_subdir mode

  let html ctx mode target =
    match target with
    | Lib (pkg, lib) ->
      html_root ctx mode
      ++ Package.Name.to_string pkg
      ++ Lib_name.to_string (Lib.name lib)
    | Pkg pkg ->
      html_root ctx mode
      ++ Package.Name.to_string pkg

  (* Keep existing functions, add _for_mode variants *)
  let odoc_support ctx mode = html_root ctx mode ++ "odoc.support"

  (* Remap file path *)
  let remap_file ctx pkg_name =
    root ctx ++ "_remap" ++ sprintf "remap-%s.txt" (Package.Name.to_string pkg_name)
end
```

---

### Phase 2: Remap File Generation

#### 2.1 Add Remap Generation Function

Add new function in [odoc.ml](src/dune_rules/odoc.ml):

```ocaml
(* Generate remap file content for non-local packages *)
let generate_remap_mappings sctx ~local_packages ~all_deps =
  let local_pkg_set = Package.Name.Set.of_list local_packages in

  (* Filter to non-local (external/installed) packages *)
  let external_deps =
    List.filter all_deps ~f:(fun target ->
      match target with
      | Pkg pkg_name -> not (Package.Name.Set.mem pkg_name local_pkg_set)
      | Lib (pkg_name, _lib) -> not (Package.Name.Set.mem pkg_name local_pkg_set))
  in

  (* Generate mappings: local_path:remote_url *)
  let* mappings =
    Memo.List.map external_deps ~f:(fun target ->
      match target with
      | Pkg pkg_name ->
        (* Get package version from installed database *)
        let+ version_opt = get_package_version sctx pkg_name in
        let version = Option.value version_opt ~default:"latest" in
        let local_path = Package.Name.to_string pkg_name in
        let remote_url = sprintf "https://ocaml.org/p/%s/%s/doc/"
          (Package.Name.to_string pkg_name) version in
        [ (local_path, remote_url) ]

      | Lib (pkg_name, lib) ->
        let+ version_opt = get_package_version sctx pkg_name in
        let version = Option.value version_opt ~default:"latest" in
        let pkg_path = Package.Name.to_string pkg_name in
        let lib_path = pkg_path ^ "/" ^ Lib_name.to_string (Lib.name lib) in
        let base_url = sprintf "https://ocaml.org/p/%s/%s/doc/"
          (Package.Name.to_string pkg_name) version in
        [ (pkg_path, base_url)
        ; (lib_path, base_url ^ Lib_name.to_string (Lib.name lib) ^ "/")
        ])
  in
  Memo.return (List.concat mappings)

(* Write remap file *)
let write_remap_file sctx ~remap_file ~mappings =
  let contents =
    String.concat ~sep:"\n"
      (List.map mappings ~f:(fun (local, remote) ->
        sprintf "%s:%s" local remote))
  in
  Super_context.add_rule sctx ~dir:(Path.Build.parent_exn remap_file)
    (Action_builder.write_file remap_file contents)
```

#### 2.2 Helper to Get Package Version

```ocaml
let get_package_version sctx pkg_name =
  (* Query installed packages database for version *)
  (* This may require extending Package_info or DB modules *)
  (* For MVP: return None and use "latest" *)
  Memo.return None
```

---

### Phase 3: Refactor HTML Generation

**UPDATED APPROACH**: Extract HTML generation logic into a shared helper function to avoid code duplication.

#### 3.1 Modify `generate_html_artifact` (DONE)

Add `?remap_file` and `?mode` parameters:

```ocaml
let generate_html_artifact sctx ~artifact ~search_db ~sidebar_file
    ?(remap_file : Path.Build.t option = None) ?(mode = Doc_mode.Local_only) () =
  let ctx = Super_context.context sctx in
  let odoc_support_path = Paths_for_mode.odoc_support ctx mode in
  let html_root = Paths_for_mode.html_root ctx mode in
  (* ... rest of implementation with mode-aware paths ... *)
```

**Status**: ✓ Completed. The function now accepts mode and remap_file parameters.

#### 3.2 Extract HTML generation helper (NEW)

Extract the HTML generation logic from the `"_html"` case into a reusable helper:

```ocaml
let generate_html_for_package sctx ~ctx ~pkg_or_lib_name ~library_artifacts
    ~package_pages ~artifacts_by_lib_complete ~dir ~mode () =
  (* Combine library artifacts and package pages for HTML generation *)
  let all_artifacts_for_html = library_artifacts @ package_pages in
  let visible_artifacts =
    List.filter all_artifacts_for_html ~f:(fun a -> not a.hidden)
  in

  (* Generate sidebar for non-synthetic packages *)
  let* sidebar_file_opt = ... in

  (* Create search_db for the entire package *)
  let* search_db = ... in

  (* Generate remap file for Local_only mode *)
  let* remap_file_opt =
    match mode with
    | Doc_mode.Local_only ->
      (* TODO: Implement remap generation based on dependencies *)
      Memo.return None
    | Doc_mode.Full -> Memo.return None
  in

  (* Generate HTML for all visible artifacts *)
  let* () =
    Memo.parallel_iter visible_artifacts ~f:(fun artifact ->
      generate_html_artifact sctx ~artifact ~search_db ~sidebar_file:sidebar_file_opt
        ?remap_file:remap_file_opt ~mode ())
  in

  (* Create format aliases *)
  (* ... alias setup logic ... *)
```

#### 3.3 Update `handle_package_artifacts`

Modify the routing to call the helper for both `_html` and `_html_full`:

```ocaml
let handle_package_artifacts sctx ~dir ~path_prefix pkg_or_lib_name =
  (* ... existing artifact discovery logic ... *)

  let rules =
    match path_prefix with
    | "_odoc" -> (* ... existing compilation logic ... *)
    | "_odocls" -> (* ... existing linking logic ... *)
    | "_html" ->
      (* HTML generation for local packages only *)
      generate_html_for_package sctx ~ctx ~pkg_or_lib_name ~library_artifacts
        ~package_pages ~artifacts_by_lib_complete ~dir ~mode:Doc_mode.Local_only ()
    | "_html_full" ->
      (* HTML generation for all packages (full mode) *)
      generate_html_for_package sctx ~ctx ~pkg_or_lib_name ~library_artifacts
        ~package_pages ~artifacts_by_lib_complete ~dir ~mode:Doc_mode.Full ()
    | _ -> (* ... other cases ... *)
```

#### 3.4 Update route dispatcher

Add routing for `_html_full` in the main dispatcher:

```ocaml
| [ "_html"; pkg_or_lib_name ] ->
  handle_package_artifacts sctx ~dir ~path_prefix:"_html" pkg_or_lib_name
| [ "_html_full" ] ->
  (* Root html_full directory - allow subdirs *)
  Memo.return (Build_config.Gen_rules.make ...)
| [ "_html_full"; pkg_or_lib_name ] ->
  handle_package_artifacts sctx ~dir ~path_prefix:"_html_full" pkg_or_lib_name
```

---

### Phase 4: Alias Configuration

#### 4.1 Refactor `setup_package_aliases_format`

Split into mode-specific logic ([odoc.ml:2724-2826](src/dune_rules/odoc.ml#L2724-L2826)):

```ocaml
let setup_package_aliases_format sctx ~pkg ~output ~doc_mode =
  let ctx = Super_context.context sctx in
  let alias = Doc_mode.alias_name output doc_mode in

  let deps_action =
    let open Action_builder.O in
    let* all_deps = compute_package_dependencies sctx pkg in

    (* Filter dependencies based on mode *)
    let+ relevant_deps =
      match doc_mode with
      | Local_only ->
        (* Only include local workspace packages *)
        let+ local_pkgs = get_workspace_packages sctx in
        let local_set = Package.Name.Set.of_list local_pkgs in
        List.filter all_deps ~f:(fun target ->
          match target with
          | Pkg p | Lib (p, _) -> Package.Name.Set.mem p local_set)

      | Full ->
        (* Include all transitive dependencies (current behavior) *)
        Memo.return all_deps
    in

    (* Convert to format aliases *)
    let dep_aliases =
      List.map relevant_deps ~f:(fun target ->
        let target_alias = Doc_mode.alias_name output doc_mode in
        (* Construct path for target's alias *)
        Dep.format_alias_for_mode output doc_mode ctx target)
    in
    Action_builder.deps (Dep.Set.of_list_map dep_aliases ~f:(fun f -> Dep.alias f))
  in

  Rules.Produce.Alias.add_deps alias deps_action
```

#### 4.2 Update Top-Level Alias Setup

```ocaml
let setup_package_aliases sctx ~pkg =
  Memo.List.iter Doc_mode.all ~f:(fun doc_mode ->
    Memo.List.iter Output_format.all ~f:(fun output ->
      setup_package_aliases_format sctx ~pkg ~output ~doc_mode))
```

---

### Phase 5: Helper Functions

#### 5.1 Get Workspace Packages

```ocaml
let get_workspace_packages sctx =
  let* workspace = Super_context.workspace sctx in
  let packages = Workspace.packages workspace in
  Memo.return (Package.Name.Map.keys packages)
```

#### 5.2 Compute All Dependencies

```ocaml
let compute_all_dependencies sctx target =
  (* Leverage existing dependency computation *)
  (* Extract from setup_package_aliases_format logic *)
  (* Return list of all transitive deps as targets *)
  ...
```

---

## Testing Strategy

### Test Case Structure

Create test in `test/blackbox-tests/test-cases/odoc/remap-test/`:

**Setup:**
- Workspace with local packages A, B
- A depends on installed package "lwt"
- B depends on A

**Verify `@doc` behavior:**
1. Only builds A and B HTML
2. Generates remap file with lwt mappings
3. Links to lwt point to ocaml.org

**Verify `@doc-full` behavior:**
1. Builds A, B, and lwt HTML
2. No remap file generated
3. Relative links between all packages

**Verify remap file format:**
- Check file contains correct `local:remote` mappings
- Verify ocaml.org URLs are well-formed

---

## Migration Path

**Phase 1**: Add infrastructure (types, paths) - **no breaking changes**
**Phase 2**: Add remap generation - **no breaking changes**
**Phase 3**: Refactor HTML generation to accept mode parameter - **internal refactor**
**Phase 4**: Wire up aliases - **new @doc-full alias, @doc behavior changes**
**Phase 5**: Test and validate

**Risk Mitigation:**
- Keep `Doc_mode.Full` identical to current behavior
- Default to `Full` mode if uncertain
- Both modes coexist, users can choose

---

## Code Reuse Summary

**Shared Functions** (parameterized by `doc_mode`):
- `generate_html_artifact` - takes optional `remap_file`
- `handle_package_artifacts` - iterates over both modes
- `setup_package_aliases_format` - filters deps by mode
- All artifact discovery logic - unchanged
- Sidebar generation - needs `doc_mode` for correct output dir
- Search DB generation - needs `doc_mode` for correct output dir

**Mode-Specific Logic:**
- Remap file generation (Local_only only)
- Dependency filtering (Local_only vs Full)
- Output directory (Paths.html_root)
- Alias names (@doc vs @doc-full)

**Estimated Code Duplication**: < 5% (only mode-specific branches)

---

## Open Questions

### 1. Package Version Resolution

How to get installed package versions?

**Options:**
- **Option A**: Query opam/package database
- **Option B**: Use "latest" placeholder
- **Option C**: Add to Package_info

**Recommendation**: Start with Option B (use "latest"), implement Option A later

### 2. Stdlib Handling

Should stdlib be remapped or built locally?

**Recommendation**: Build locally (it's always available and stable)

### 3. Intermediate .odoc/.odocl Files

Should we skip building them for remapped packages?

**Current Plan**: Still build (needed for link validation)
**Future Optimization**: Skip if `--remap-file` makes linking unnecessary

### 4. Search Database

Should it include remapped packages?

**Recommendation**: No - only local packages in search for better relevance

---

## Implementation Checklist

- [ ] Phase 1: Core infrastructure
  - [ ] Define `Doc_mode` module
  - [ ] Update `Paths` module
  - [ ] Add tests for path generation

- [ ] Phase 2: Remap generation
  - [ ] Implement `generate_remap_mappings`
  - [ ] Implement `write_remap_file`
  - [ ] Add `get_package_version` stub
  - [ ] Test remap file format

- [ ] Phase 3: HTML generation refactor
  - [ ] Modify `generate_html_artifact` signature
  - [ ] Update `handle_package_artifacts`
  - [ ] Add remap file to odoc command
  - [ ] Test both modes generate HTML

- [ ] Phase 4: Alias configuration
  - [ ] Refactor `setup_package_aliases_format`
  - [ ] Update top-level alias setup
  - [ ] Test @doc and @doc-full aliases
  - [ ] Verify dependency filtering

- [ ] Phase 5: Integration & testing
  - [ ] Create comprehensive test case
  - [ ] Verify remap URLs are correct
  - [ ] Check HTML links work as expected
  - [ ] Performance testing with large projects

---

## Expected Files Modified

- `src/dune_rules/odoc.ml` - Main implementation
- `src/dune_rules/odoc.mli` - Update interface if needed
- `test/blackbox-tests/test-cases/odoc/remap-test/` - New test case
- Documentation updates (if any)

---

## Notes

- This design maximizes code reuse by parameterizing existing functions
- Both documentation modes can coexist without conflicts
- The remap facility follows the same pattern as odoc_driver.ml
- Future enhancements can add version resolution and optimize build performance
