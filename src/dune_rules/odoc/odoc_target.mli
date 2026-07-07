open Import

type page = { name : string }
type asset = { asset_name : string }

(** [visible] is true for a library's entry modules: the modules to link and
    render. The remaining modules (e.g. the mangled modules of a wrapped
    library) are only compiled, for odoc to resolve references through them. *)
type mod_ =
  { module_name : Module_name.t
  ; visible : bool
  }

(** A documentation target: the scope a set of documentation artifacts belongs
    to. Public libraries are identified by their package and library; private
    libraries by their unique name; packages carry their own pages. *)
type _ t =
  | Lib : Package.Name.t * Lib.Local.t -> mod_ t
  | Private_lib : string * Lib.Local.t -> mod_ t
  | Pkg : Package.Name.t -> page t
