open Import

(** A single documentation artifact: a module or a page, belonging to a
    documentation target, produced from a source file. All paths (the compiled
    [.odoc], the linked [.odocl] and the rendered outputs) are computed from
    the artifact's kind and source. *)

type kind =
  | Module : Odoc_target.mod_ * Odoc_target.mod_ Odoc_target.t -> kind
  | Page : Odoc_target.page * Odoc_target.page Odoc_target.t -> kind

type t

(** [make ~source payload target] creates the artifact for [source] (a module's
    [.cmti]/[.cmt] or a page's [.mld]) within [target]. *)
val make : source:Path.Build.t -> 'a -> 'a Odoc_target.t -> t

val get_kind : t -> kind
val source_file : t -> Path.Build.t
val odoc_dir : Context.t -> t -> Path.Build.t
val odoc_file : Context.t -> t -> Path.Build.t
val odocl_file : Context.t -> t -> Path.Build.t
val output_file : Context.t -> Odoc_paths.Output_format.t -> t -> Path.Build.t
