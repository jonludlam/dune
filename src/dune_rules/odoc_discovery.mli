(** Artifact discovery for odoc packages and libraries *)

open Import

val get_workspace_packages : unit -> Package.Name.t list Memo.t
val is_local_package : Package.Name.t -> bool Memo.t
val libs_of_pkg : Context.t -> pkg:Package.Name.t -> Lib.t list Memo.t
val get_private_libraries : Context.t -> Lib.Local.t list Memo.t
val toplevel_index_artifact : Context.t -> mode:Odoc_target.Doc_mode.t -> Odoc_artifact.t Memo.t

module Toplevel_index : sig
  type pkg_item = { name : string; version : Package_version.t option }
  type private_lib_item = { unique_name : string; display_name : string }
  type item = Package of pkg_item | Private_lib of private_lib_item

  val get_items : mode:Odoc_target.Doc_mode.t -> Context.t -> item list Memo.t
  val mld_content : item list -> string
end

val discover_package_artifacts
  :  Super_context.t
  -> Context.t
  -> pkg_or_lib_unique_name:string
  -> (Odoc_artifact.t list * string list) Memo.t

val collect_all_visible_odocls
  :  Super_context.t
  -> mode:Odoc_target.Doc_mode.t
  -> ?include_all_deps:bool
  -> unit
  -> (Package.Name.t list * Path.Build.t list) Memo.t

val expand_packages_with_odoc_config
  :  Context.t
  -> packages:Package.Name.t list
  -> private_libs:Lib.t list
  -> Package.Name.Set.t Memo.t
