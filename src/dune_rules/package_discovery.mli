(** Package discovery for odoc documentation generation.
    
    This module provides functionality to discover which OCaml packages
    contain which libraries and files, using opam installation information
    and dune build artifacts. *)

open Import

(** The type representing package discovery state *)
type t

(** Create a new package discovery instance from a dune context *)
val create : context:Context.t -> t Memo.t

(** Find which package a library belongs to *)
val package_of_library : t -> Lib.t -> Package.Name.t option

(** Get all libraries belonging to a specific package *)
val libraries_of_package : t -> Package.Name.t -> Lib.t list

(** Get all mld files belonging to a specific package *)
val mlds_of_package : t -> Package.Name.t -> Path.t list

(** Get the source file (cmti/cmt) for a specific module in an installed library.
    Returns the path to the .cmti file if it exists, otherwise .cmt *)
val module_source_file : t -> lib:Lib.t -> module_name:string -> Path.t option

(** Get the odoc configuration for a specific package *)
val config_of_package : t -> Package.Name.t -> Odoc_config.t

(** Get the version of an installed package.
    Returns None if the package is not installed or version cannot be determined. *)
val version_of_package : t -> Package.Name.t -> string option Memo.t
