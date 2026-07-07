open Import

type page = { name : string }
type mod_ = { module_name : Module_name.t }

(** A documentation target: the scope a set of documentation artifacts belongs
    to. Public libraries are identified by their package and library; private
    libraries by their unique name; packages carry their own pages. *)
type _ t =
  | Lib : Package.Name.t * Lib.Local.t -> mod_ t
  | Private_lib : string * Lib.Local.t -> mod_ t
  | Pkg : Package.Name.t -> page t
