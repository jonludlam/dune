# Alias-Based Dependency System in Odoc Implementation

## Overview

Both `odoc.ml` and `odoc_new.ml` use a sophisticated alias-based system to enforce the requirement that all dependency libraries must be fully compiled before dependent libraries can begin compilation. This document examines how this system works and its implications for odoc v3 migration.

## The `.odoc-all` Alias System

### Core Mechanism

Both implementations use a common pattern:
1. **`.odoc-all` alias per library/package**: Each library/package gets its own alias in its odoc directory
2. **Alias population**: All `.odoc` files for a library are added as dependencies to its `.odoc-all` alias
3. **Dependency chaining**: Dependent libraries depend on the `.odoc-all` aliases of their dependencies

### Implementation in `odoc.ml`

#### Alias Definition (lines 174-199)
```ocaml
let alias = Alias.make (Alias.Name.of_string ".odoc-all")

let deps ctx pkg requires =
  let* libs = Resolve.read requires in
  Action_builder.deps
    (List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
       match Lib.Local.of_lib lib with
       | None -> acc  (* Skip external libraries *)
       | Some lib ->
         let dir = Paths.odocs ctx (Lib lib) in
         let alias = alias ~dir in  (* .odoc-all in lib's odoc dir *)
         Dep.Set.add acc (Dep.alias alias)))

let setup_deps ctx m files =
  Rules.Produce.Alias.add_deps (alias ctx m) (Action_builder.path_set files)
```

#### Dependency Flow
1. **Library compilation** (lines 454): All module `.odoc` files are added to the library's `.odoc-all` alias
2. **Module compilation** (lines 434, 347): Each module compilation depends on `Dep.deps` which includes all dependency library aliases
3. **Transitive enforcement**: Through alias chaining, ensures full dependency tree compilation

### Implementation in `odoc_new.ml`

#### Enhanced Alias System (lines 625-662)
```ocaml
let alias = Alias.make (Alias.Name.of_string ".odoc-all")

let deps ctx ~all maps valid_libs pkg requires =
  let* libs = Resolve.read requires in
  Action_builder.deps
    (List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
       match List.mem ~equal:lib_equal valid_libs lib with
       | false -> acc  (* Skip invalid/filtered libraries *)
       | true ->
         let index = (* determine library's index location *) in
         let dir = Index.odoc_dir ctx ~all index in
         let alias = alias ~dir in  (* .odoc-all in lib's odoc dir *)
         Dep.Set.add acc (Dep.alias alias)))
```

#### Key Differences from `odoc.ml`
1. **Validation filtering**: Only depends on libraries in the valid set
2. **Index-based locations**: Uses hierarchical index system for alias placement
3. **External library support**: Handles both local and external libraries

## Alias Directory Structure

### `odoc.ml` Structure
```
_doc/
├── _odoc/
│   ├── lib1/
│   │   ├── .odoc-all        <- alias for lib1
│   │   ├── module1.odoc
│   │   └── module2.odoc
│   └── lib2/
│       ├── .odoc-all        <- alias for lib2 (depends on lib1's .odoc-all)
│       └── module3.odoc
```

### `odoc_new.ml` Structure  
```
_doc_new/
├── odoc/
│   ├── local/lib1/
│   │   ├── .odoc-all        <- alias for lib1
│   │   ├── module1.odoc
│   │   └── module2.odoc
│   ├── local/lib2/
│   │   ├── .odoc-all        <- alias for lib2 (depends on lib1's .odoc-all)
│   │   └── module3.odoc
│   └── stdlib/
│       └── .odoc-all        <- alias for stdlib
```

## Dependency Enforcement Flow

### 1. Library Setup Phase
- Each library's modules are compiled to `.odoc` files
- All `.odoc` files for a library are added to its `.odoc-all` alias via `setup_deps`
- This creates: **Library's .odoc-all depends on all its module .odoc files**

### 2. Dependency Resolution Phase  
- When compiling a dependent library, `Dep.deps` is called with the library's requirements
- This creates dependencies on all required libraries' `.odoc-all` aliases
- This creates: **Dependent library compilation depends on all dependency .odoc-all aliases**

### 3. Build Execution
- Dune's build system resolves the alias dependencies
- A library's `.odoc-all` can only be satisfied when all its modules are compiled
- Dependent libraries cannot start until all dependency `.odoc-all` aliases are satisfied
- This enforces: **Complete dependency library compilation before dependent compilation**

## Example Dependency Chain

Given libraries: `base` → `core` → `myapp`

1. **Base library**:
   - `base/module1.odoc` → `base/.odoc-all`
   - `base/module2.odoc` → `base/.odoc-all`

2. **Core library** (depends on base):
   - Core module compilation depends on `base/.odoc-all` 
   - `core/module3.odoc` → `core/.odoc-all`
   - `core/module4.odoc` → `core/.odoc-all`

3. **Myapp library** (depends on core):
   - Myapp module compilation depends on `core/.odoc-all` (which transitively depends on `base/.odoc-all`)
   - `myapp/module5.odoc` → `myapp/.odoc-all`

**Result**: `base` must be fully compiled before `core` can start, `core` must be fully compiled before `myapp` can start.

## Special Cases

### Package Dependencies
Both implementations handle package-level dependencies:
- Package's `.odoc` files (from `.mld` files) are included in dependency resolution
- Package aliases are included when a library belongs to a package

### External Libraries (`odoc_new.ml`)
- External libraries get `.odoc-all` aliases in their classified locations
- Same dependency enforcement applies across Dune/non-Dune boundaries

### Stdlib Handling
- Standard library is automatically included in dependency closures
- Gets its own `.odoc-all` alias that others depend on

## Implications for odoc v3 Migration

### Current System Strengths
1. **Guaranteed ordering**: Complete dependency libraries before dependents
2. **Transitive enforcement**: Works across any dependency depth  
3. **Incremental builds**: Changes only affect dependents, not dependencies
4. **Parallel safety**: No race conditions between libraries

### Migration Considerations
1. **Alias system preservation**: The `.odoc-all` mechanism should be preserved
2. **Parent dependency removal**: Only remove hierarchical parent-child deps, not library deps
3. **Build performance**: Current system already enables optimal library-level parallelization
4. **Compatibility**: Same dependency semantics must be maintained

### What Changes with `--parent-id`
- **Remove**: `Hidden_deps` on parent index files
- **Preserve**: All `.odoc-all` alias dependencies between libraries
- **Result**: Same library dependency ordering, without hierarchical compilation constraints

The alias system is the foundation that ensures correctness of library dependency ordering and should remain unchanged in the odoc v3 migration. The `--parent-id` change only affects the hierarchical documentation structure dependencies, not the fundamental library compilation dependencies.