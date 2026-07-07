open Import

type page = { name : string }

type mod_ =
  { module_name : Module_name.t
  ; visible : bool
  }

type _ t =
  | Lib : Package.Name.t * Lib.Local.t -> mod_ t
  | Private_lib : string * Lib.Local.t -> mod_ t
  | Pkg : Package.Name.t -> page t
