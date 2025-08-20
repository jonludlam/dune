# How Dune Uses Odoc to Generate HTML Documentation

## Overview
Dune uses odoc (OCaml documentation generator) to create HTML documentation from OCaml source files. There are two implementations: `odoc.ml` (current/legacy) and `odoc_new.ml` (new hierarchical approach). Both compile `.cmti`/`.cmi` files into `.odoc` files, then link them to `.odocl` files, and finally generate HTML output.

## Core Workflow

### 1. Compilation Phase (`odoc compile`)
- **Input**: `.cmti` files (compiled module type interfaces) or `.mld` files (documentation pages)
- **Output**: `.odoc` files (odoc's intermediate format)
- **Key operations**:
  - Extract module signatures and documentation from compiled interfaces
  - Resolve cross-references between modules
  - Handle dependencies between modules within libraries
  - Apply package and library scoping

### 2. Linking Phase (`odoc link`)
- **Input**: `.odoc` files
- **Output**: `.odocl` files (linked odoc files)
- **Purpose**: Resolve all cross-module and cross-library references
- **Key operations**:
  - Include paths to dependency libraries via `-I` flags
  - Establish parent-child relationships for hierarchical documentation
  - Link references across package boundaries

### 3. HTML Generation Phase (`odoc html-generate`)
- **Input**: `.odocl` files
- **Output**: HTML files and directories
- **Key operations**:
  - Generate actual HTML pages with proper styling
  - Create directory structure (one directory per module with `index.html`)
  - Include search functionality (Sherlodoc integration)
  - Generate support files (CSS, JavaScript)

## Implementation Details in `odoc.ml` (Current/Legacy)

### Key Concepts:
1. **Library Unique Names (LNU)**: Unique identifiers for libraries combining name and project context
2. **Scope Keys**: Encode library location for cross-project references
3. **Target Types**: Either `Lib` (library) or `Pkg` (package)

### Directory Structure:
```
_doc/
├── _odoc/           # Compiled .odoc files
│   └── pkg/<name>/  # Package odoc files
├── _odocls/         # Linked .odocl files
├── _html/           # Generated HTML
│   ├── <pkg>/       # Package documentation
│   └── odoc.support/# CSS and support files
└── _mlds/           # Generated .mld files
```

### Key Functions:
- `compile_module`: Compiles a single module to `.odoc`
- `compile_mld`: Compiles documentation pages (`.mld` files)
- `link_odoc_rules`: Links `.odoc` to `.odocl` with dependency resolution
- `setup_generate`: Generates HTML from `.odocl` files
- `odoc_include_flags`: Builds include paths for dependencies

### Compilation Phase Dependencies in `odoc.ml`:

#### Module-level Dependencies:
The `module_deps` function (lines 296-307) computes intra-library module dependencies using Dune's dependency graphs:
```ocaml
let module_deps (m : Module.t) ~obj_dir ~(dep_graphs : Dep_graph.Ml_kind.t) =
  (* Uses dep_graphs.intf for modules with .mli files *)
  (* Falls back to dep_graphs.impl for modules without .mli *)
  (* Returns paths to .odoc files of dependency modules *)
```

This leverages Dune's pre-computed dependency information from the compilation phase. The dependencies are determined by:
- For modules with interfaces (`.mli`): uses interface dependency graph
- For modules without interfaces: uses implementation dependency graph
- All dependent module `.odoc` files must be built before compiling the current module

#### Library-level Dependencies:
The compilation phase requires (lines 428-434):
1. **Compilation context dependencies** (`Compilation_context.requires_compile`)
2. **Package-level dependencies** (if the library belongs to a package)
3. Include flags are computed via `odoc_include_flags` which:
   - Collects paths to all dependency library `.odoc` directories
   - Adds the package's `.odoc` directory if applicable
   - Generates `-I` flags for each directory

### Link Phase Dependencies in `odoc.ml`:

The `link_odoc_rules` function (lines 397-417) handles linking dependencies:
1. **Dependency collection** via `Dep.deps`:
   - Gathers all `.odoc` files from required libraries
   - Includes package-level dependencies if specified
   - Creates aliases for dependency tracking (`.odoc-all` alias)

2. **Include path generation** via `odoc_include_flags`:
   - Resolves all library dependencies
   - Builds paths to `.odoc` directories for local libraries only
   - Skips external libraries (returns empty path set for non-local libs)
   - Adds package directory if the library belongs to a package

3. **Execution order**:
   - All dependency `.odoc` files must exist (via alias dependencies)
   - Link command includes all dependency paths via `-I` flags
   - Input: single `.odoc` file
   - Output: corresponding `.odocl` file

## Implementation Details in `odoc_new.ml` (New Hierarchical)

### Key Improvements:
1. **Hierarchical Index System**: Multi-level documentation structure
2. **External Library Support**: Better handling of non-Dune packages
3. **Fallback Mechanisms**: Graceful handling of incomplete information
4. **Unified dependency discovery**: Uses `odoc compile-deps` for all modules

### Index Types:
```ocaml
type ty =
  | Private_lib of string       # Private library documentation
  | Top_dir of ext_ty           # Top-level categorization
  | Sub_dir of string           # Subdirectory in hierarchy

type ext_ty =
  | Local_packages              # Workspace packages
  | Relative_to_stdlib          # Standard library packages
  | Relative_to_findlib of path # External packages
  | Other                       # Fallback category
```

### Directory Structure (Enhanced):
```
_doc_new/
├── index/          # Index compilation artifacts
├── odoc/           # Organized odoc files
│   ├── docs/       # Documentation hierarchy
│   ├── local/      # Local packages
│   ├── stdlib/     # Standard library
│   └── findlib-N/  # External packages
├── html/           # Generated HTML
│   └── docs/       # Hierarchical documentation
└── classify/       # Classification metadata
```

### Compilation Phase Dependencies in `odoc_new.ml`:

#### Module Dependency Discovery via `odoc compile-deps`:
A critical difference from `odoc.ml` is the use of `odoc compile-deps` (lines 1084-1118):

```ocaml
let external_module_deps_rule sctx ~all a =
  (* Runs: odoc compile-deps <cmti_file> > <deps_file> *)
  (* Output format: "Module_name hash" per line *)
```

**Key characteristics**:
1. **Universal approach**: Used for BOTH internal and external libraries (despite the function name)
2. **No reliance on Dune's dependency graphs**: Asks odoc directly for dependencies
3. **Output parsing**: `parse_odoc_deps` extracts module names and hashes from odoc output
4. **Self-filtering**: Removes self-dependencies (module depending on itself)

#### Compilation Workflow (lines 1121-1178):
The `compile_odocs` function orchestrates compilation:

1. **Dependency computation**:
   - For each module artifact, runs `odoc compile-deps` to generate `.deps` file
   - Parses the `.deps` file to get module dependencies
   - Maps module names to corresponding `.odoc` files within the same artifact set
   - Creates `Action_builder` dependencies on these `.odoc` files

2. **Library dependency resolution**:
   - `compile_requires` computes transitive closure of library dependencies
   - Includes stdlib if available
   - Filters out the libraries being compiled (avoids self-dependency)
   - Validates libraries against the allowed set

3. **Include path construction**:
   - More complex than `odoc.ml` due to external library support
   - Uses `Index` structure to determine correct paths
   - Handles both local and external libraries with different path strategies

### Link Phase Dependencies in `odoc_new.ml`:

The `link_odoc_rules` function (lines 977-1016) provides enhanced linking:

1. **Dependency resolution**:
   - `link_requires` computes full transitive closure via `Lib.closure`
   - Includes stdlib automatically if available
   - Validates against allowed library set

2. **Advanced include path generation**:
   - Uses `libs_maps` for external library location mapping
   - Supports multiple index locations via `indices` parameter
   - Each index can provide additional include paths
   - Handles hierarchical structure with parent-child relationships

3. **Index dependencies**:
   - Explicit `Hidden_deps` on index `.odoc` files
   - Ensures indexes are compiled before linking artifacts
   - Supports multiple levels of hierarchy

4. **Batch processing**:
   - Links multiple artifacts in parallel
   - Shares dependency computation across all artifacts
   - More efficient than individual linking in `odoc.ml`

### Key Differences in Dependency Handling:

| Aspect | `odoc.ml` | `odoc_new.ml` |
|--------|-----------|---------------|
| **Module deps discovery** | Uses Dune's `Dep_graph` | Uses `odoc compile-deps` |
| **External library support** | Limited/none | Full support with classification |
| **Dependency computation** | At rule creation time | Dynamic via file reading |
| **Include path strategy** | Simple directory list | Hierarchical with mappings |
| **Compilation approach** | Per-module rules | Batch compilation |
| **Link phase** | Individual artifacts | Batch with shared deps |

### The `odoc compile-deps` Advantage:

Using `odoc compile-deps` provides several benefits:
1. **Consistency**: Same dependency discovery for all modules
2. **External library support**: Works without Dune's build information
3. **Accuracy**: Odoc understands OCaml module dependencies natively
4. **Flexibility**: No need to maintain separate dependency logic

The output format is simple:
```
Module_name1 hash1
Module_name2 hash2
```

This is parsed to create build dependencies, ensuring modules are compiled in correct order within each library/package.

### Advanced Features:

1. **Index Tree Generation**:
   - Automatically generates index pages for navigation
   - Creates hierarchical structure matching package organization
   - Supports custom index.mld files or auto-generates them

2. **External Package Handling**:
   - Maps findlib packages to documentation locations
   - Handles packages without module information gracefully
   - Falls back to filesystem structure when needed

3. **Artifact Management**:
   - `Artifact` module encapsulates all documentation units
   - Tracks visibility, references, and output paths
   - Distinguishes between modules and documentation pages

4. **Classification System**:
   - Categorizes libraries as Dune-built or external
   - Determines documentation strategy per library
   - Handles mixed environments (Dune and non-Dune packages)

## Key Differences Between Implementations

| Aspect | odoc.ml | odoc_new.ml |
|--------|---------|-------------|
| **Structure** | Flat package organization | Hierarchical with categories |
| **External Libs** | Limited support | Full classification system |
| **Index Generation** | Simple package listing | Multi-level tree structure |
| **Dependency Tracking** | Alias-based | Tree-based with validation |
| **Fallback Handling** | Minimal | Comprehensive fallback modes |

## Odoc Program Invocation

Both implementations use similar odoc commands:

### 1. Compilation:
```bash
odoc compile -I <include_dir> --pkg <package> -o <output.odoc> <input.cmti>
```

### 2. Linking:
```bash
odoc link -I <deps_dir> -o <output.odocl> <input.odoc>
```

### 3. HTML Generation:
```bash
odoc html-generate -o <html_dir> --support-uri <css_path> <input.odocl>
```

## Package Documentation (.mld Files)

Both implementations handle `.mld` files (Markup Language Documentation) which provide package-level documentation pages. These are processed differently from module documentation and have special handling for `index.mld` files.

### .mld File Processing in `odoc.ml`

#### Discovery and Collection (lines 918-941):
The `package_mlds` function orchestrates `.mld` file discovery:

1. **Source discovery**:
   - `Packages.mlds` finds all `.mld` files in package directories
   - Searches `documentation` stanzas in `dune` files
   - Returns list of `.mld` file paths

2. **Index.mld generation**:
   - If no `index.mld` exists, automatically generates one
   - Creates in `_doc/_mlds/<package>/index.mld`
   - Content generated by `default_index` function (lines 889-916)

3. **Generated index content**:
   - Package title: `{0 <package> index}`
   - Lists all libraries in the package
   - For each library, shows entry modules
   - Uses odoc reference syntax: `{!module-Name}` for single entry, `{!modules:A B C}` for multiple

#### Compilation Process:

The `compile_mld` function (lines 356-375) compiles `.mld` to `.odoc`:
```ocaml
compile_mld sctx m ~includes ~doc_dir ~pkg:
  (* Input: .mld file path *)
  (* Output: page-<name>.odoc in doc_dir *)
  (* Includes: dependency library paths (usually empty for mlds) *)
  (* Package association via --pkg flag *)
```

Key characteristics:
- `.mld` files become `page-<name>.odoc` files
- No module dependencies (unlike `.cmti` compilation)
- Simple compilation with package association
- Links are resolved later in link phase

#### Integration with Package Documentation:

The `odoc_artefacts` function (lines 670-692) handles both modules and mlds:
- For packages: collects all `.mld` files, ensures `index.mld` exists
- For libraries: collects module `.odoc` files only
- Returns unified artifact list for linking

### .mld File Processing in `odoc_new.ml`

#### Enhanced Discovery (lines 1286-1324):

The `pkg_mlds` function provides extended support:
1. **Local packages**: Uses `Packages.mlds` like `odoc.ml`
2. **External packages**: Searches findlib installation directories
3. **Returns**: Map of basename to path (detecting duplicates)

The `pkg_artifacts` function creates `Artifact.t` values:
- Separates `index.mld` from other `.mld` files
- Creates artifacts for non-index mlds
- Returns tuple: `(index_path_opt, other_mld_artifacts)`

#### Hierarchical Index System (lines 1694-1769):

The `default_index` function generates sophisticated index pages:

1. **Context-aware titles**:
   - Local packages: "Package <name>"
   - External packages: "Directory <path>" (fallback mode)
   - Private libraries: "Private library <name>"
   - Stdlib packages: "Packages located relative to stdlib"

2. **Hierarchical navigation**:
   - Lists sub-indexes with links: `{{!page-"subindex"}subindex}`
   - Maintains parent-child relationships
   - Supports multi-level documentation trees

3. **Content generation modes**:
   - **Standard mode**: Lists libraries with entry modules
   - **Fallback mode**: Lists all modules found in directory (for external packages)

#### Advanced Compilation (lines 935-973):

The `compile_mld` function supports hierarchy:
```ocaml
compile_mld sctx a ~parent_opt ~is_index ~children:
  (* Parent linking via --parent flag *)
  (* Child registration via --child flags *)
  (* Special handling for index pages *)
```

Features:
- **Parent references**: Links to parent index for navigation
- **Child registration**: Declares child pages/modules for cross-references
- **Dummy child**: Added to empty indexes (workaround for odoc requirement)

#### Index Tree Generation (lines 1806-1874):

The `hierarchical_index_rules` function creates the entire tree:

1. **Index file creation**:
   - Symlinks predefined `index.mld` if it exists
   - Otherwise generates content based on tree structure
   - Creates different content for top-level vs nested indexes

2. **Compilation with context**:
   - Compiles index as special `.mld` with children
   - Links to parent index (except for root)
   - Includes all child artifacts for reference resolution

3. **Recursive processing**:
   - Processes tree depth-first
   - Each level gets its own index page
   - Maintains proper parent-child relationships

### Key Differences in .mld Handling:

| Aspect | `odoc.ml` | `odoc_new.ml` |
|--------|-----------|---------------|
| **Index generation** | Simple library listing | Hierarchical with navigation |
| **External packages** | Not supported | Full support with discovery |
| **Parent-child links** | None | Full hierarchy support |
| **Compilation flags** | Basic (`--pkg`) | Extended (`--parent`, `--child`) |
| **Index content** | Fixed format | Context-aware formatting |
| **Tree structure** | Flat package level | Multi-level hierarchy |

### HTML Generation for .mld Files:

Both implementations convert `.odocl` files to HTML, but with differences:

**odoc.ml**:
- Simple transformation: `page-<name>.odocl` → `<package>/<name>.html`
- Index pages become `<package>/index.html`

**odoc_new.ml**:
- Hierarchical paths: follows index tree structure
- Index pages at each level: `docs/<path>/index.html`
- Non-index pages: `docs/<path>/<name>.html`

The HTML generation preserves the hierarchical structure, creating a navigable documentation tree with proper parent-child relationships and cross-references.

## Additional Features

### JSON Output Format
Both implementations support dual output formats:
- **HTML**: Standard documentation format (`@doc` alias)
- **JSON**: Machine-readable format (`@doc_json` alias)
- Generated with `--as-json` flag to odoc
- Produces `.html.json` files alongside HTML
- Enables programmatic documentation consumption

### Virtual Library Handling
- Implementations of virtual libraries are filtered out
- Only the virtual interface is documented
- Prevents duplicate documentation of concrete implementations
- Ensures clean API documentation

### Private Library Documentation
**odoc.ml specific**:
- `@private_doc` alias for workspace-private libraries
- Separate documentation generation for private APIs
- Enables internal documentation without public exposure

### Entry Module Detection
- Identifies public API surface via `Modules.entry_modules`
- Filters modules to show only intended public interface
- Used in auto-generated index.mld files
- Ensures documentation matches library's intended API

### Vendored Dependencies
**odoc_new.ml specific**:
- Automatically excludes vendored directories
- Prevents documenting vendored dependencies
- Keeps documentation focused on project's own code

### Melange Support
- Both implementations support Melange (JavaScript backend)
- Different compilation modes for Melange vs OCaml
- Uses appropriate `.cmi` files based on target

### Library Validation System
**odoc_new.ml specific**:
- `Valid` module provides sophisticated filtering
- Determines which libraries should be documented
- Handles package masks (document subset)
- Categorizes libraries (local, external, stdlib)
- Excludes vendored and filters by visibility

### Classification System
**odoc_new.ml specific**:
- `odoc classify` command for external libraries
- Categorizes modules by owning library
- Determines documentation strategy per directory
- Handles mixed Dune/non-Dune environments

### Stdlib Special Handling
- Automatically included in dependency closure
- Special categorization in hierarchical structure
- Always available for cross-references

### Scope Key System
**odoc.ml specific**:
- Encodes library locations across projects
- Enables cross-workspace documentation
- Handles private project dependencies
- Generates unique identifiers for libraries

### Environment Variable Support
- `ODOC_SYNTAX` environment variable
- Runtime configuration of odoc features
- Build dependency tracking on env vars

### Dev Tool Lock Directory
- Can use odoc from `_build/.dev-tool-lock/odoc`
- Falls back to system odoc if unavailable
- Ensures consistent odoc version

### Vlib Module Processing
**odoc.ml specific**:
- Special handling for virtual library modules
- `Modules.With_vlib.drop_vlib` removes metadata
- Ensures clean module processing

## Special Features

1. **Sherlodoc Integration**: Full-text search with JavaScript UI, separate indexing for internal/external
2. **Warning Control**: Fatal vs non-fatal warning modes
3. **Dev Tool Support**: Can use vendored odoc from lock files
4. **Module Dependencies**: Automatic computation via `odoc compile-deps`
5. **Parent-Child Relationships**: Hierarchical documentation structure
6. **Auto-generated Indexes**: Creates index.mld when missing
7. **External Package Support**: Documents non-Dune packages in `odoc_new.ml`

## Rule Generation Entry Points

Both implementations provide `gen_rules` functions that generate build rules based on paths:
- `_doc/_html/` → HTML generation rules
- `_doc/_odoc/pkg/` → Package compilation rules
- `_doc/_odocls/` → Linking rules
- Index and classification rules in `odoc_new.ml`

## Implementation Guide for AI Agents

To recreate this functionality, an AI agent familiar with Dune would need to:

### 1. Set up the compilation pipeline:
- Identify all `.cmti` files from compiled libraries
- Create compilation rules that invoke `odoc compile` with proper include paths
- Handle both local and external library dependencies
- Generate `.odoc` files in the appropriate directory structure

### 2. Implement the linking phase:
- Collect all `.odoc` files that need linking
- Resolve dependency ordering
- Create link rules with proper `-I` flags for all dependencies
- Generate `.odocl` files

### 3. Generate HTML output:
- Process `.odocl` files through `odoc html-generate`
- Set up proper support file paths (CSS/JS)
- Create directory structure for HTML output
- Handle search integration if required

### 4. Manage dependencies:
- Track file-level dependencies for incremental builds
- Use Dune's alias system for dependency grouping
- Ensure proper ordering of compilation, linking, and generation phases

### 5. Handle special cases:
- Package-level documentation (`.mld` files)
- Private vs public library visibility
- Virtual libraries and implementations
- External packages without full module information

This architecture allows Dune to incrementally build documentation, tracking dependencies precisely and regenerating only what's necessary when source files change.