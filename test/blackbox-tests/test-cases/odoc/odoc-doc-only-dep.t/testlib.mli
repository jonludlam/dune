(** Test library with documentation-only dependency.

    This library does NOT compile against cmdliner, but its documentation
    references cmdliner types. The {!Cmdliner.Arg.t} type is used for
    command-line argument specifications.

    See also {!Cmdliner.Term.t} for term definitions. *)

(** A simple function.
    For CLI apps, you might want to use {!Cmdliner.Cmd.v} instead. *)
val hello : unit -> string
