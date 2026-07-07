(** Scope and naming utilities for odoc library disambiguation. *)

open Import

module Scope_id : sig
  (** Identifies a scope for documentation generation — either a named
      package or a private library (identified by its unique [name\@key]
      string). *)
  type t =
    | Package of Package.Name.t
    | Private_lib of
        { unique_name : string
        ; lib_name : Lib_name.t
        ; project : Dune_project.t
        }

  (** Parse a scope string.  A string containing ['\@'] is treated as a
      private-library scope key; otherwise as a package name. *)
  val of_string : string -> t Memo.t
end

module Scope_key : sig
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end

val lib_unique_name : Lib.t -> string
val pkg_or_lnu : Lib.t -> string
