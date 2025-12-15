(** Core artifact type and operations for odoc *)

open Import

type kind =
  | Module : Odoc_target.mod_ * Odoc_target.mod_ Odoc_target.t -> kind
  | Page : Odoc_target.page * Odoc_target.page Odoc_target.t -> kind

type source =
  | Local_source of Path.Build.t
  | Installed_source of { src_path : Path.t }
  | Generated of { content : string; output_path : Path.Build.t }

type t

val get_kind : t -> kind
val source_file : t -> Path.t
val generated_content : t -> string option
val odoc_file : Context.t -> t -> Path.Build.t
val odocl_file : Context.t -> t -> Path.Build.t
val odoc_dir : Context.t -> t -> Path.Build.t
val html_file : Context.t -> Odoc_paths.Doc_mode.t -> t -> Path.Build.t
val json_file : Context.t -> Odoc_paths.Doc_mode.t -> t -> Path.Build.t
val html_dir_target : Context.t -> Odoc_paths.Doc_mode.t -> t -> Path.Build.t option
val json_dir_target : Context.t -> Odoc_paths.Doc_mode.t -> t -> Path.Build.t option
val pkg : t -> Package.Name.t option
val lib_name : t -> Lib_name.t
val lib : t -> Lib.t option
val extra_libs : t -> Lib.t list
val extra_packages : t -> Package.Name.t list
val hidden : t -> bool
val parent_id : t -> string
val should_suppress_output : t -> bool Memo.t

val create
  :  kind:kind
  -> source:source
  -> extra_libs:Lib.t list
  -> extra_packages:Package.Name.t list
  -> t
