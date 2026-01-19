# Plan: Implement Asset Support for odoc Rules in Dune

## Overview

Add support for odoc assets (images, videos, audio, etc.) in dune's odoc rules. Assets are static files that can be referenced from documentation using `{image!/path/to/asset}` syntax.

## Background: How Assets Work in odoc

Based on analysis of odoc and odoc_driver:

1. **Asset Discovery**: Non-.mld files in `doc/<pkg>/odoc-pages/` or any files in `doc/<pkg>/odoc-assets/`
2. **Compilation**: `odoc compile-asset --output-dir <od> --parent-id <pid> --name <assetname>` creates `asset-<name>.odoc`
3. **Linking**: Assets are linked like other units via `odoc link`
4. **Generation**: `odoc html-generate-asset --output-dir <odir> --asset-unit <path.odocl> <actual/file.ext>` copies the file to the output

## Key Finding: Installation Already Works

The current dune code in `install_rules.ml` already installs ALL files from `(documentation (files ...))` to `odoc-pages/`:

```ocaml
let doc_install_files ~loc mld_contents =
  List.rev_map mld_contents ~f:(fun (mld : Doc_sources.mld) ->
    Install.Entry.make
      ~kind:`File
      ~dst:(sprintf "odoc-pages/%s" (Path.Local.to_string mld.in_doc))
      Section.Doc
      mld.path
    |> Install.Entry.Sourced.create ~loc)
```

This means non-.mld files are already installed to the correct location. The only missing piece is processing them through the odoc asset pipeline during documentation generation.

## Current Architecture

Dune's odoc rules use:
- `Odoc_artifact.t` with kinds: `Module` and `Page`
- `source`: `Local_source`, `Installed_source`, `Generated`
- `Doc_sources.mld` type already holds both mld files AND other files from `(files ...)`
- Discovery in `odoc_discovery.ml`
- Compilation/linking/generation in `odoc.ml`

## Implementation Plan

### Step 1: Extend Odoc_target with Asset Type

**File: `src/dune_rules/odoc_target.ml` / `odoc_target.mli`**

Add asset type (simple - just needs name and relative path):

```ocaml
type asset = { name : string; rel_path : string }
```

Assets use the existing `Pkg` target since they belong to packages.

### Step 2: Extend Odoc_artifact with Asset Kind

**File: `src/dune_rules/odoc_artifact.ml` / `odoc_artifact.mli`**

Add a new kind for assets:

```ocaml
type kind =
  | Module : Odoc_target.mod_ * Odoc_target.mod_ Odoc_target.t -> kind
  | Page : Odoc_target.page * Odoc_target.page Odoc_target.t -> kind
  | Asset : Odoc_target.asset * Odoc_target.page Odoc_target.t -> kind  (* NEW - uses page target *)
```

Update helper functions:
- `odoc_file` - for assets: `asset-<name>.odoc` in the package's odoc dir
- `odocl_file` - for assets: `asset-<name>.odocl`
- `parent_id` - same as pages (package name + relative path)
- Add `asset_name` accessor for the asset's filename

### Step 3: Separate Assets from Mlds in Discovery

**File: `src/dune_rules/doc_sources.ml` / `doc_sources.mli`**

The current `build_mlds_map` returns all files. We need to split by extension:

```ocaml
type doc_file =
  { path : Path.Build.t
  ; in_doc : Path.Local.t
  }

(* Keep existing mld type as alias *)
type mld = doc_file

(* New: asset is same structure *)
type asset = doc_file

val build_mlds_map : ... -> (Documentation.t * mld list) list Memo.t
val build_assets_map : ... -> (Documentation.t * asset list) list Memo.t
```

Filter logic:
- `.mld` extension → mld
- Other extensions → asset

### Step 4: Add Asset Discovery to Packages.ml

**File: `src/dune_rules/packages.ml` / `packages.mli`**

Add function to collect assets for a package:

```ocaml
val assets : Super_context.t -> Package.Name.t -> Doc_sources.asset list Memo.t
```

Similar to existing `mlds` function.

### Step 5: Add Asset Discovery to Odoc_discovery

**File: `src/dune_rules/odoc_discovery.ml`**

Add functions:
- `get_local_asset_infos` - get local assets from documentation stanzas
- `discover_pkg_asset_artifacts` - create Asset artifacts
- Integrate into `discover_local_pkg_artifacts`

For installed packages in `discover_installed_pkg_artifacts`:
- Use `Package_discovery` to find assets in `odoc-pages/` (non-.mld files)

### Step 6: Add Asset Compilation to odoc.ml

**File: `src/dune_rules/odoc.ml`**

Add `compile_asset` function:

```ocaml
let compile_asset sctx ~artifact =
  let ctx = Super_context.context sctx in
  let asset_name = Artifact.asset_name artifact in
  let parent_id = Artifact.parent_id artifact in
  run_odoc sctx "compile-asset"
    ~quiet:false
    ~flags_for:None
    [ A "--output-dir"; A "_odoc"
    ; A "--parent-id"; A parent_id
    ; A "--name"; A asset_name
    ]
  |> Action_builder.With_targets.add ~file_targets:[Artifact.odoc_file ctx artifact]
```

Modify `compile_artifact` to dispatch to `compile_asset` for Asset kinds.

### Step 7: Add Asset Linking

Assets link similarly to pages - modify `link_artifact` to handle Asset kind. The linking command is the same (`odoc link`), just operating on the asset's `.odoc` file.

### Step 8: Add Asset HTML Generation

**File: `src/dune_rules/odoc.ml`**

Add `generate_html_asset` function:

```ocaml
let generate_html_asset sctx ~artifact ~mode =
  let ctx = Super_context.context sctx in
  let html_root = Paths.html_root ctx mode in
  let odocl_file = Artifact.odocl_file ctx artifact in
  let asset_source = Artifact.source_file artifact in
  run_odoc sctx "html-generate-asset"
    ~quiet:false
    ~flags_for:None
    [ A "--output-dir"; Path (Path.build html_root)
    ; A "--asset-unit"; Dep (Path.build odocl_file)
    ; Dep asset_source
    ]
```

Modify `generate_html_artifact` to dispatch to `generate_html_asset` for Asset kinds.

### Step 9: Update Package_discovery for Installed Assets

**File: `src/dune_rules/package_discovery.ml` / `package_discovery.mli`**

Add function to find assets in installed packages:

```ocaml
val assets_of_package : t -> Package.Name.t -> Path.t list
```

Look for:
- Non-.mld files in `<pkg>/odoc-pages/`
- All files in `<pkg>/odoc-assets/` (if it exists)

## Files to Modify

1. `src/dune_rules/odoc_target.ml` / `.mli` - Add asset type
2. `src/dune_rules/odoc_artifact.ml` / `.mli` - Add Asset kind
3. `src/dune_rules/doc_sources.ml` / `.mli` - Separate mlds from assets
4. `src/dune_rules/packages.ml` / `.mli` - Add assets collection
5. `src/dune_rules/odoc_discovery.ml` - Integrate asset discovery
6. `src/dune_rules/odoc.ml` - Add compile/link/generate for assets
7. `src/dune_rules/package_discovery.ml` / `.mli` - Add installed asset discovery
8. `src/dune_rules/dir_contents.ml` / `.mli` - Add assets field (like mlds)

## Verification

1. Build: `dune build @check`
2. Create test case with assets in `test/blackbox-tests/test-cases/odoc/`
3. Run: `dune runtest test/blackbox-tests/test-cases/odoc/`
4. Verify HTML output contains copied asset files
5. Verify assets can be referenced with `{image!/pkg/asset}` syntax

## Test Case Design

```
test-cases/odoc/assets.t/
├── run.t
├── dune-project
├── dune
├── lib.ml
├── index.mld (references {image!/mypkg/logo.png})
└── logo.png
```

The test should verify:
1. `dune build @doc` succeeds
2. `_build/default/_doc/_html/mypkg/logo.png` exists
3. The generated HTML can reference the asset
