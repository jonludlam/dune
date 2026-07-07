(** Scope and naming utilities for odoc library disambiguation. *)

open Import

module Scope_key : sig
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end

val lib_unique_name : Lib.t -> string
val pkg_or_lnu : Lib.t -> string
