(** Library A which depends on Libc and Odoc_parser.

    This tests that transitive library dependencies get -P flags
    even when the dependency's package is not listed as :with-doc.

    Uses {!Libc.c_type} from pkgc and {!Odoc_parser.t} from odoc-parser. *)

(** A type alias for {!Libc.c_type}. *)
type t = Libc.c_type

(** Wrap a value using {!Libc.c_function}. *)
val wrap : Libc.c_type -> Libc.c_type

(** A parsed documentation comment.
    See {!Odoc_parser.t}. *)
type doc = Odoc_parser.t
