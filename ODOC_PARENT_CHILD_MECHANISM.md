# The --parent and --child Mechanism in odoc_new.ml

## Overview

In `odoc_new.ml`, the `--parent` and `--child` arguments are used during the odoc compilation phase to establish hierarchical relationships between documentation artifacts. This mechanism enables odoc to understand the documentation tree structure and generate proper navigation and cross-references. This document provides a detailed analysis for planning migration to odoc v3's new `--parent-id` mechanism.

## The --parent Argument

### Purpose
The `--parent` argument tells odoc which documentation page is the parent of the current artifact being compiled. This establishes the hierarchical relationship needed for:
- Navigation (breadcrumbs, up-links)
- Proper reference resolution in the documentation tree
- Determining output paths in the generated HTML

### Implementation Details

#### parent_args Function (lines 792-809)
```ocaml
let parent_args parent_opt =
  match parent_opt with
  | None -> []
  | Some mld ->
    let dir = Artifact.odoc_file mld |> Path.Build.parent_exn in
    let reference = Artifact.reference mld in
    let odoc_file = (* ... creates dependency ... *) in
    [ Command.Args.A "-I"
    ; Path (Path.build dir)
    ; A "--parent"
    ; A reference
    ; Hidden_deps odoc_file
    ]
```

Key components:
1. **Include path (`-I`)**: Directory containing the parent's `.odoc` file
2. **Parent reference**: A string identifier for the parent
3. **Hidden dependency**: **Critical constraint** - ensures parent is compiled before child

### Build Dependency Requirements

**This is a fundamental constraint of the current implementation**: The `Hidden_deps odoc_file` creates a build dependency requiring the parent's `.odoc` file to exist before any child can be compiled. This means:

1. **Strict ordering**: Index pages must be compiled before any modules or sub-pages
2. **Dependency chains**: Changes to parent indexes trigger recompilation of all children
3. **Build complexity**: The dependency graph includes parent-child relationships
4. **Parallel compilation limitations**: Children cannot compile until their parents exist

#### Reference Format (lines 723-733)
The reference passed to `--parent` follows specific patterns:
- For `.mld` files: `page-"<basename>"` (e.g., `page-"index"`)
- For modules: `module-<Name>` (e.g., `module-List`)

These references are odoc's internal naming scheme for cross-references.

### Usage Contexts

#### 1. Module Compilation (lines 1161-1165)
```ocaml
let parent_opt =
  match Artifact.artifact_ty a with
  | Module true -> Some parent  (* Visible modules get parent *)
  | _ -> None                    (* Hidden modules don't *)
```
Only **visible** modules receive a parent reference. This affects their position in the documentation hierarchy.

#### 2. Index Page Compilation (lines 1829-1841)
```ocaml
let parent_opt =
  match index with
  | [] -> None                     (* Root has no parent *)
  | _ :: idx -> Some (Artifact.index ctx ~all idx)  (* Parent is one level up *)
```
Index pages use their position in the hierarchy to determine parents:
- Root index (`[]`) has no parent
- Nested indexes have the parent index from one level up

#### 3. Regular .mld Files (lines 1136-1142)
Non-index `.mld` files in packages always receive their parent index as the parent.

## The --child Argument

### Purpose
The `--child` argument declares which documentation artifacts are children of the current page. This is primarily used for index pages to:
- Enable cross-references to child pages
- Build the navigation structure
- Generate appropriate links in index listings

### Implementation Details

#### Child Registration (lines 949-957)
```ocaml
let child_args =
  let child_args =
    List.fold_left children ~init:[] ~f:(fun args child ->
      match Artifact.artifact_ty child with
      | Module true | Mld -> "--child" :: Artifact.reference child :: args
      | Module false -> args)  (* Hidden modules excluded *)
  in
  if is_index && List.is_empty child_args then 
    [ "--child"; "dummy" ]  (* Workaround for empty indexes *)
  else child_args
```

Key behaviors:
1. **Selective inclusion**: Only visible modules and `.mld` files are registered
2. **Reference format**: Same as parent references (`page-"name"`, `module-Name`)
3. **Dummy child**: Empty indexes get a dummy child (odoc requirement workaround)

### Usage Patterns

#### 1. Index Pages (lines 1835-1841)
```ocaml
compile_mld
  sctx
  mld
  ~parent_opt
  ~is_index:true
  ~children:(extra_children @ all_artifacts)
```
Index pages receive:
- `extra_children`: Sub-indexes at the next level
- `all_artifacts`: All modules and `.mld` files at this level

#### 2. Regular .mld Files
Regular `.mld` files are compiled with `~children:[]` - they don't have children in the hierarchy.

## Hierarchical Structure Example

Consider a package `mylib` with sublibraries:
```
mylib/
├── index.mld
├── manual.mld
├── core/
│   └── (modules)
└── utils/
    └── (modules)
```

The compilation would establish:
1. **Root index** (`docs/local/mylib/`)
   - No parent
   - Children: `page-"manual"`, `page-"core"`, `page-"utils"`, plus all modules

2. **Sublibrary index** (`docs/local/mylib/core/`)
   - Parent: `page-"index"` (the package index)
   - Children: All core modules

3. **Manual page** (`docs/local/mylib/manual.mld`)
   - Parent: `page-"index"` (the package index)
   - Children: None

## Limitations and Workarounds

### 1. Reference Syntax Limitation (lines 1847-1851)
Currently, odoc cannot reference nested page children like `{!page-pkg.page-sublib}`. This limits the depth of indexes that can be properly cross-referenced to `cur_depth + 2`.

### 2. Dummy Child Workaround (line 956)
Empty indexes require at least one child, so a dummy child is added. This is a workaround for an odoc requirement.

### 3. Hidden Module Exclusion
Hidden modules (those with double underscores or not in entry modules) are excluded from parent-child relationships, making them inaccessible in the documentation tree.

## Impact on HTML Generation

The parent-child relationships established during compilation affect:

1. **Output paths**: While not directly determined by `--parent`, the hierarchy influences where HTML files are generated
2. **Navigation**: Parent links enable "up" navigation in the documentation
3. **Index content**: Children determine what appears in index listings
4. **Cross-references**: The hierarchy affects how references are resolved

## Key Differences from odoc.ml

`odoc.ml` does not use `--parent` or `--child` arguments at all. It has:
- Flat package structure
- No hierarchical navigation
- Simpler reference resolution
- No parent-child relationships in compilation

## Additional Context for Migration

### Features Affecting Parent-Child Relationships

#### Entry Module Filtering
- Only visible entry modules receive parent relationships
- `Modules.entry_modules` determines the public API surface
- Hidden modules (double underscore) are excluded from hierarchy
- May need special handling in odoc v3 migration

#### Virtual Library Support
- Virtual library implementations are filtered out during compilation
- Only virtual interfaces participate in parent-child relationships
- New mechanism must maintain this filtering

#### JSON Output Format
- Parent-child relationships must work for both HTML and JSON output
- `--as-json` flag affects how hierarchies are serialized
- Migration must preserve dual-format support

#### External Library Integration
- `odoc_new.ml` uses classification system for external libraries
- Parent-child relationships cross Dune/non-Dune boundaries
- Classification results may need to inform parent ID generation

#### Index Tree Limitations
- Current depth limitation (cur_depth + 2) due to odoc reference syntax
- odoc v3 may lift these restrictions with new parent ID system
- Migration opportunity to support deeper hierarchies

### Environment and Configuration

#### ODOC_SYNTAX Environment Variable
- Affects odoc compilation behavior
- Parent-child mechanism may be sensitive to syntax mode
- New parent ID system must account for env var dependencies

#### Dev Tool Lock Directory
- Uses specific odoc versions from lock files
- Parent ID mechanism compatibility across odoc versions
- May need version detection for migration path

## Critical Difference: Build Dependencies

### Current Implementation (odoc_new.ml)
**Requires parent pages to exist before compiling children**:
- Every child compilation has `Hidden_deps` on parent's `.odoc` file
- Creates strict build ordering: index → sub-indexes → modules → sub-modules
- Parent changes trigger cascading recompilation of all descendants
- Limits parallelization opportunities during compilation

### odoc v3 --parent-id Mechanism
**No build dependency on parent existence**:
- Children can be compiled without parent `.odoc` files existing
- Parent ID is just a string identifier, not a file dependency
- Removes hierarchical ordering constraints (parent → child)
- Still preserves module dependencies within libraries and library dependencies
- Parent-child relationships established at link or HTML generation time

## Migration Implications

This fundamental difference means migrating to `--parent-id` will:

### Enable New Opportunities
1. **Reduced serialization**: Removes parent-child ordering constraints from compilation
2. **Independent index generation**: Index pages don't block module compilation
3. **Better parallelization**: More compilation opportunities within dependency constraints
4. **Simpler dependency graphs**: No hierarchical dependencies, only module and library deps

### Require Significant Changes
1. **Dependency removal**: Eliminate `Hidden_deps` on parent files
2. **Build rule restructuring**: Remove parent-child ordering constraints  
3. **Reference resolution**: Move hierarchy resolution to link/HTML phases
4. **Index generation**: Decouple index creation from child compilation

### Maintain Compatibility
1. **Output structure**: Same hierarchical HTML organization
2. **Navigation**: Same parent-child relationships in final documentation
3. **Cross-references**: Same linking behavior between pages
4. **API surface**: Same user-facing documentation structure

## Considerations for odoc v3 Migration

When migrating to odoc v3's `--parent-id` mechanism, consider:

1. **Build dependency elimination**: Remove `Hidden_deps` and parent compilation requirements
2. **Parallel compilation enablement**: Restructure rules for independent compilation
3. **Reference format changes**: The new mechanism may use different reference formats
4. **Dependency graph simplification**: How to maintain correctness without parent dependencies
5. **Child registration**: Whether child registration remains necessary
6. **Path determination**: How output paths are computed from parent IDs
7. **Backward compatibility**: Supporting both mechanisms during transition
8. **Index handling**: Special cases for index pages may change
9. **Entry module filtering**: How visible/hidden modules affect hierarchy
10. **Virtual library integration**: Maintaining filtering of implementations
11. **External library classification**: Cross-boundary relationships
12. **Multi-format output**: JSON and HTML generation consistency

The current implementation tightly couples the hierarchy with the compilation phase through build dependencies. The new `--parent-id` mechanism in odoc v3 decouples these concerns, potentially allowing for:
- **Independent compilation**: No build dependencies on parent existence
- **Better parallelization**: More opportunities within module/library dependency constraints
- **Simplified build graphs**: Hierarchy only matters for output generation
- **Dynamic hierarchy changes**: Parent relationships not baked into compilation
- **Better support for external documentation**: No need to compile parents
- **Cleaner integration**: Hierarchy separate from dependency management

### Preserved Dependencies

The following dependency relationships remain necessary in odoc v3:

1. **Module dependencies within libraries**: Modules must be compiled after their dependencies
2. **Library dependencies**: All modules in dependency libraries must be compiled before dependent libraries
3. **Cross-reference resolution**: Link phase still requires all referenced `.odoc` files to exist

What is eliminated are the **hierarchical parent-child compilation dependencies** - the requirement that index pages and parent documentation must exist before children can be compiled.