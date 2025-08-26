# Step-by-Step Plan: Migrating Dune to odoc v3

## Overview

This plan outlines the migration from Dune's current odoc implementations (`odoc.ml` and `odoc_new.ml`) to odoc v3's `--parent-id` mechanism and package-centric layout, using the reference driver as a guide.

## Migration Goals

1. **Replace hierarchical build dependencies** with string-based parent IDs
2. **Adopt package-centric directory layout** (`package/library/module` structure)
3. **Improve build performance** through better parallelization
4. **Simplify code complexity** by removing hierarchical compilation constraints
5. **Maintain compatibility** with existing features and workflows

## Phase 1: Infrastructure Setup

### Step 1.1: Create New odoc Module Interface
**Objective**: Define clean API matching reference driver

**Tasks**:
1. **Create `src/dune_rules/odoc_v3.mli`**:
   ```ocaml
   module Id : sig
     type t
     val of_fpath : Fpath.t -> t
     val to_string : t -> string
   end
   
   val compile :
     output_dir:Path.Build.t ->
     input_file:Path.Build.t ->
     includes:Path.Build.Set.t ->
     parent_id:Id.t ->
     warnings_tag:string option ->
     unit
   
   val link :
     input_file:Path.Build.t ->
     output_file:Path.Build.t ->
     includes:Path.Build.t list ->
     docs:(string * Path.Build.t) list ->
     libs:(string * Path.Build.t) list ->
     unit
   ```

2. **Create `src/dune_rules/odoc_v3.ml`** implementing the interface
3. **Add feature detection** for odoc v3 in `src/dune_rules/odoc_config.ml`

**Acceptance Criteria**:
- [ ] New API compiles without errors
- [ ] Feature detection correctly identifies odoc v3 vs v2
- [ ] Basic compile/link functions work with simple test case

### Step 1.2: Implement Package-Centric Path System
**Objective**: Define new directory layout logic

**Tasks**:
1. **Create `src/dune_rules/odoc_paths_v3.ml`**:
   ```ocaml
   val doc_dir : Context.t -> Path.Build.t
   val package_dir : Context.t -> Package.Name.t -> Path.Build.t  
   val library_dir : Context.t -> Package.Name.t -> Lib_name.t -> Path.Build.t
   val module_dir : Context.t -> Package.Name.t -> Lib_name.t -> Module_name.t -> Path.Build.t
   ```

2. **Update path generation** to use package-centric structure
3. **Create migration utilities** to convert from old paths

**Acceptance Criteria**:
- [ ] New path functions generate correct `package/library/module` structure
- [ ] Path conversion utilities work for existing projects
- [ ] No conflicts with existing path generation

### Step 1.3: Implement Parent ID System
**Objective**: Replace file dependencies with string IDs

**Tasks**:
1. **Create parent ID computation logic**:
   ```ocaml
   val parent_id_of_module : Package.Name.t -> Lib_name.t -> Odoc_v3.Id.t
   val parent_id_of_library : Package.Name.t -> Odoc_v3.Id.t  
   val parent_id_of_package : unit -> Odoc_v3.Id.t
   ```

2. **Remove `Hidden_deps` usage** in compilation rules
3. **Update compilation functions** to use `--parent-id` instead of `--parent`

**Acceptance Criteria**:
- [ ] Parent IDs correctly computed for all artifact types
- [ ] No `Hidden_deps` on parent files in new system
- [ ] Compilation still respects module and library dependencies

## Phase 2: Core Implementation Migration

### Step 2.1: Replace odoc.ml Compilation Logic
**Objective**: Migrate flat package-based approach to use odoc v3

**Tasks**:
1. **Update `compile_module` function** in `odoc.ml`:
   - Replace `--parent` with `--parent-id`
   - Remove `Hidden_deps` on parent files
   - Update include path generation
   - Use new path system

2. **Update `compile_mld` function**:
   - Use `--parent-id` for package documentation
   - Remove child registration dependencies
   - Simplify compilation flags

3. **Preserve alias system** (`.odoc-all`):
   - Keep library dependency enforcement
   - Update alias directory locations
   - Maintain dependency ordering semantics

**Acceptance Criteria**:
- [ ] All modules compile with `--parent-id`
- [ ] Library dependencies still enforced via aliases
- [ ] Package documentation generates correctly
- [ ] Build times improve due to removed hierarchical deps

### Step 2.2: Replace odoc_new.ml Compilation Logic  
**Objective**: Migrate hierarchical approach to package-centric v3

**Tasks**:
1. **Replace hierarchical index system**:
   - Remove category-based structure (`local/`, `stdlib/`, etc.)
   - Implement package-centric organization
   - Update index generation to new layout

2. **Update `compile_odocs` function**:
   - Keep `odoc compile-deps` usage (already v3-compatible)
   - Replace hierarchical parent dependencies
   - Update to package-centric parent IDs

3. **Simplify external library handling**:
   - Remove complex classification system
   - Use uniform package structure for all libraries
   - Simplify fallback mechanisms

**Acceptance Criteria**:
- [ ] External libraries documented in package structure
- [ ] No deep category hierarchies in output
- [ ] Classification still works but outputs to package dirs
- [ ] Performance improves due to simplified logic

### Step 2.3: Update Linking Phase
**Objective**: Adapt linking to work with new structure

**Tasks**:
1. **Update `link_odoc_rules`** in both implementations:
   - Use package-centric include paths
   - Remove hierarchical dependency resolution
   - Preserve library-level dependency enforcement

2. **Update include path generation**:
   - Generate paths matching new directory layout
   - Handle both local and external libraries uniformly
   - Remove category-specific path logic

3. **Preserve search and cross-reference functionality**:
   - Update search database generation
   - Ensure cross-references resolve correctly
   - Maintain Sherlodoc integration

**Acceptance Criteria**:
- [ ] All cross-references resolve correctly
- [ ] Search functionality works with new layout
- [ ] Library dependencies still enforced
- [ ] Both local and external libraries link properly

## Phase 3: HTML Generation Migration

### Step 3.1: Update HTML Generation
**Objective**: Generate package-centric HTML structure

**Tasks**:
1. **Update HTML path generation**:
   - Generate `package/library/module.html` structure
   - Remove category-based directories
   - Update index page generation

2. **Update CSS and asset handling**:
   - Update relative path calculations
   - Ensure support files work with new structure
   - Update asset copying rules

3. **Update JSON output**:
   - Ensure JSON format works with new paths
   - Update search indexing for new structure
   - Preserve dual HTML/JSON generation

**Acceptance Criteria**:
- [ ] HTML generates in correct package/library structure
- [ ] CSS and JavaScript work correctly
- [ ] JSON output maintains compatibility
- [ ] Search indexing works with new paths

### Step 3.2: Update Index and Navigation Generation
**Objective**: Create package-focused navigation

**Tasks**:
1. **Implement package-level indexes**:
   - Generate package `index.html` with library listings
   - Remove category-based top-level indexes
   - Update library index generation

2. **Update navigation links**:
   - Update breadcrumb generation
   - Fix inter-package linking
   - Ensure proper parent-child navigation

3. **Preserve auto-generated content**:
   - Update `default_index` generation for new structure
   - Maintain entry module detection
   - Keep library listing functionality

**Acceptance Criteria**:
- [ ] Package indexes list libraries correctly
- [ ] Navigation works between packages and libraries
- [ ] Auto-generated content follows new structure
- [ ] Breadcrumbs reflect package/library hierarchy

## Phase 4: Integration and Testing

### Step 4.1: Feature Parity Testing
**Objective**: Ensure all existing features work with new system

**Tasks**:
1. **Test core functionality**:
   - Module documentation generation
   - Package documentation (`.mld` files)
   - Cross-references and linking
   - Search functionality

2. **Test advanced features**:
   - Virtual library handling
   - Private library documentation
   - Melange support
   - JSON output format
   - External library documentation

3. **Performance testing**:
   - Measure build time improvements
   - Test parallel compilation
   - Verify incremental builds work

**Acceptance Criteria**:
- [ ] All existing odoc functionality preserved
- [ ] Build times improve measurably
- [ ] Incremental builds work correctly
- [ ] No regressions in documentation quality

### Step 4.2: Migration Path Implementation
**Objective**: Support transitioning existing projects

**Tasks**:
1. **Implement feature detection**:
   - Auto-detect odoc v3 availability
   - Fall back to current implementation if unavailable
   - Provide clear error messages for version mismatches

2. **Create migration utilities**:
   - Tool to convert existing documentation URLs
   - Documentation for layout changes
   - Compatibility warnings for breaking changes

3. **Update build system integration**:
   - Ensure `dune build @doc` works with both systems
   - Update opam file generation
   - Test with various project configurations

**Acceptance Criteria**:
- [ ] Automatic fallback to legacy implementation works
- [ ] Migration tools help users update existing setups
- [ ] Integration with external tools preserved
- [ ] Clear documentation for migration process

### Step 4.3: Edge Case Handling
**Objective**: Handle special cases and corner cases

**Tasks**:
1. **Complex project structures**:
   - Multi-package workspaces
   - Vendored dependencies
   - Mixed Dune/non-Dune projects
   - Virtual library implementations

2. **Integration edge cases**:
   - Custom documentation workflows
   - CI/CD pipeline integration
   - Documentation hosting setups
   - Custom CSS/theming

3. **Error handling and diagnostics**:
   - Clear error messages for migration issues
   - Debugging support for new system
   - Fallback mechanisms for failures

**Acceptance Criteria**:
- [ ] Complex project structures work correctly
- [ ] Integration with existing workflows preserved
- [ ] Error messages are clear and actionable
- [ ] Debugging tools help identify issues

## Phase 5: Deployment and Rollout

### Step 5.1: Gradual Rollout Strategy
**Objective**: Deploy new system safely with fallbacks

**Tasks**:
1. **Feature flag implementation**:
   - Environment variable to enable new system
   - Dune workspace configuration option
   - Per-project override capability

2. **Documentation and communication**:
   - Update Dune documentation
   - Blog post about improvements
   - Migration guide for users

3. **Community testing**:
   - Beta testing with select projects
   - Gather feedback on performance improvements
   - Address reported issues

**Acceptance Criteria**:
- [ ] Feature flag allows safe testing
- [ ] Documentation clearly explains changes
- [ ] Community feedback incorporated
- [ ] No major regressions reported

### Step 5.2: Legacy System Deprecation
**Objective**: Plan sunset of old implementations

**Tasks**:
1. **Deprecation timeline**:
   - Mark old implementations as deprecated
   - Set timeline for removal (e.g., 2-3 releases)
   - Provide migration warnings

2. **Code cleanup**:
   - Remove old implementations after deprecation period
   - Clean up unused path generation code
   - Remove hierarchical dependency logic

3. **Final validation**:
   - Comprehensive testing of final implementation
   - Performance benchmarking
   - User acceptance validation

**Acceptance Criteria**:
- [ ] Clear deprecation timeline communicated
- [ ] Old code removed cleanly
- [ ] Final implementation meets all requirements
- [ ] Users successfully migrated to new system

## Risk Mitigation

### High-Risk Areas
1. **URL Breaking Changes**: Existing documentation links will break
   - *Mitigation*: Provide URL rewriting tools and clear migration docs
2. **Complex Project Compatibility**: Edge cases may not work initially
   - *Mitigation*: Extensive testing with real-world projects
3. **Performance Regressions**: New system might be slower in some cases
   - *Mitigation*: Benchmark throughout development
4. **Integration Breakage**: External tools may not work with new layout
   - *Mitigation*: Early communication with tool maintainers

### Validation Strategy
1. **Continuous testing** against reference projects
2. **Performance monitoring** throughout migration
3. **User feedback loops** during beta testing
4. **Rollback plan** if major issues discovered

## Success Metrics

- **Build Performance**: 20-30% improvement in odoc build times
- **Code Complexity**: Significant reduction in odoc rule complexity
- **User Experience**: Cleaner, more intuitive documentation structure  
- **Maintainability**: Easier to extend and modify odoc integration
- **Compatibility**: All existing features preserved
- **Adoption**: Smooth migration for existing projects

## Timeline Estimate

- **Phase 1-2**: 3-4 weeks (Infrastructure + Core Migration)
- **Phase 3**: 2-3 weeks (HTML Generation)
- **Phase 4**: 2-3 weeks (Integration + Testing)
- **Phase 5**: 2-4 weeks (Deployment + Rollout)

**Total**: ~10-14 weeks for complete migration

This plan provides a systematic approach to migrating Dune's odoc integration to v3 while minimizing risk and maintaining compatibility.