(** A trivial wrapper around cmdliner.

    See {!Cmdliner.Term.const} for the underlying primitive. *)

let hello = Cmdliner.Term.const ()
