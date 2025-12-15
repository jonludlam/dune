(** Scope and naming utilities for odoc v2 library disambiguation *)

open Import

(** Identifies a scope for documentation generation - either a package or a private library.

    This type replaces ad-hoc string parsing that checks for '@' in names
    to distinguish between packages and private libraries. *)
module Scope_id : sig
  type t =
    | Package of Package.Name.t
    | Private_lib of string  (** unique name, e.g. "libname@projectkey" *)

  (** Parse a string into a scope ID. Strings containing '@' are treated as
      private library unique names; others are treated as package names. *)
  val of_string : string -> t

  (** Convert to string for use in paths. *)
  val to_string : t -> string

  (** Check if this is a private library (vs a regular package). *)
  val is_private_lib : t -> bool

  (** Get as a Package.Name.t. For private libs, this parses the string as a package name
      (which may be useful for certain operations that need a Package.Name.t). *)
  val as_package_name : t -> Package.Name.t
end

(** Scope key encoding for v2 library names.

    In odoc v2, private libraries from different projects need unique identifiers
    to avoid name collisions. The Scope_key module encodes project information
    into library names using the format "libname@key" where key is a 12-character
    hash derived from the project name and root path. *)
module Scope_key : sig
  (** Parse a v2 library name string and resolve it to a library name and database.

      - Input without '@': treated as public library, uses public_libs database
      - Input with '@': "libname@key" format, looks up project by key and uses its scope *)
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t

  (** Convert a library name to its v2 unique name format.

      For a private library in the given project, generates "libname@key" where
      key is a 12-character hash uniquely identifying the project. *)
  val to_string : Lib_name.t -> Dune_project.t -> string
end

(** Generate a unique name for a local library.

    - Public libraries: use their plain name
    - Private libraries: use v2 format "libname@key" from Scope_key.to_string
    - Raises: assertion failure if called on installed libraries *)
val lib_unique_name : Lib.Local.t -> string
