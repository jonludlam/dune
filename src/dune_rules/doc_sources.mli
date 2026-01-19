open Import

(** A documentation file (mld or asset) with its path and location in doc hierarchy *)
type doc_file =
  { path : Path.Build.t (** The path to the file *)
  ; in_doc : Path.Local.t (** Where in the doc hierarchy the file should appear *)
  }

(** Mld documentation file *)
type mld = doc_file

(** Asset file (image, video, etc.) that can be referenced from documentation *)
type asset = doc_file

(** Builds a map of [mld] files from the [(documentation ...)] stanza.
    Only includes files with .mld extension. *)
val build_mlds_map
  :  Dune_file.t
  -> dir:Path.Build.t
  -> files:Filename.Set.t
  -> Expander.t
  -> (Documentation.t * mld list) list Memo.t

(** Builds a map of asset files from the [(documentation ...)] stanza.
    Only includes non-.mld files from the (files ...) entry. *)
val build_assets_map
  :  Dune_file.t
  -> dir:Path.Build.t
  -> files:Filename.Set.t
  -> Expander.t
  -> (Documentation.t * asset list) list Memo.t
