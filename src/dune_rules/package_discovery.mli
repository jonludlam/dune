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

(** Check if a package is available for documentation *)
val is_package_available : t -> Package.Name.t -> bool

(** {1 Functions exposed for testing} *)

module For_tests : sig
  (** Parse an opam changes file and return the list of installed files *)
  val parse_changes_file : 
    changes_file_path:Path.t -> string -> string list

  (** Build a package discovery map from changes file data *)
  val build_from_changes_data :
    packages_with_files:(Package.Name.t * string list) list ->
    opam_prefix:Path.t ->
    libs:Lib.t list ->
    t
    
  (** Find which package owns a specific archive file path *)
  val package_of_archive_file :
    packages_with_files:(Package.Name.t * string list) list ->
    opam_prefix:Path.t ->
    archive_file:Path.t ->
    Package.Name.t option
    
  (** Create an empty discovery state *)
  val empty : t
  
  (** {1 Real filesystem testing functions} *)
  
  (** Find opam root directory on the current system *)
  val find_opam_root : unit -> Path.t option
  
  (** Find OCaml compiler package names in opam installation *)  
  val find_ocaml_packages : opam_root:Path.t -> Package.Name.t list
  
  (** Read and parse a real changes file *)
  val read_real_changes_file : opam_root:Path.t -> package_name:Package.Name.t -> string list option
  
  (** Find a specific file within opam installation *)
  val find_archive_in_opam : opam_root:Path.t -> filename:string -> Path.t option
  
  (** Determine package ownership using real opam data *)
  val package_of_real_archive : archive_path:Path.t -> Package.Name.t option
end