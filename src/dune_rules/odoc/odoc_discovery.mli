open Import

module Toplevel_index : sig
  type item =
    { name : string
    ; version : Package_version.t option
    ; link : string
    }

  val of_packages
    :  Package.t Package.Name.Map.t
    -> Odoc_paths.Output_format.t
    -> item list

  val content : Odoc_paths.Output_format.t -> item list -> string
end

val libs_of_pkg : Context_name.t -> pkg:Package.Name.t -> Lib.Local.t list Memo.t
val entry_modules_by_lib : Super_context.t -> Lib.Local.t -> Module.t list Memo.t

val entry_modules
  :  Super_context.t
  -> pkg:Package.Name.t
  -> Module.t list Lib.Local.Map.t Memo.t

val check_mlds_no_dupes
  :  pkg:Package.Name.t
  -> mlds:('path * string) list
  -> path_to_string:('path -> string)
  -> ('path * string) String.Map.t

val report_warnings : Doc_sources.mld list -> unit

val mlds
  :  Super_context.t
  -> Dune_lang.Package_name.t
  -> ((Path.Build.t * string) list * Doc_sources.mld list) Memo.t
