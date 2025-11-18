(** use [ocamlobjinfo] binary to read the input compiled file and try to find
    the source file *)
val get_source : Fpath.t -> Fpath.t list -> Fpath.t option
