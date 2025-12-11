(** Path computation utilities for odoc files *)

open Import

module Doc_mode = Odoc_target.Doc_mode

type sidebar_scope =
  | Per_package of Package.Name.t
  | Global

val root : Context.t -> Path.Build.t
val odocs : Context.t -> 'a Odoc_target.t -> Path.Build.t
val html_root : Context.t -> Doc_mode.t -> Path.Build.t
val odocl_root : Context.t -> Path.Build.t
val sherlodoc_root : Context.t -> Path.Build.t
val html : Context.t -> Doc_mode.t -> 'a Odoc_target.t -> Path.Build.t
val odocl : Context.t -> 'a Odoc_target.t -> Path.Build.t
val gen_mld_dir : Context.t -> Package.Name.t -> Path.Build.t
val lib_index_mld : Context.t -> Package.Name.t -> Lib_name.t -> Path.Build.t
val odoc_support : Context.t -> Doc_mode.t -> Path.Build.t
val toplevel_index_mld : Context.t -> Doc_mode.t -> Path.Build.t
val index_file : Context.t -> Doc_mode.t -> sidebar_scope -> Path.Build.t
val sidebar_file : Context.t -> Doc_mode.t -> sidebar_scope -> Path.Build.t
val sidebar_json : Context.t -> Doc_mode.t -> sidebar_scope -> Path.Build.t
val remap_file : Context.t -> Path.Build.t
