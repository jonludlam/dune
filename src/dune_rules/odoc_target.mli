(** Target types for odoc artifacts *)

open Import

module Doc_mode : sig
  type t =
    | Local_only
    | Full

  val subdir : t -> string
  val html_subdir : t -> string
  val all : t list
end

type page = { name : string; pkg_libs : Lib.t list }

type mod_ =
  { visible : bool
  ; module_name : Module_name.t
  }

type _ t =
  | Lib : Package.Name.t * Lib.t -> mod_ t
  | Private_lib : string * Lib.t -> mod_ t
  | Pkg : Package.Name.t -> page t
  | Toplevel : Doc_mode.t -> page t

type any = Any : 'a t -> any

val compare_any : any -> any -> Ordering.t

val target_of_lib : Package_discovery.t -> Lib.t -> mod_ t Memo.t
