open! Stdune

type t =
  { packages : Package_dependency.t list
  ; url : string option
  ; index : string option (* Custom workspace-level index.mld path *)
  }

let encode { packages; url; index } =
  let open Dune_sexp.Encoder in
  match packages, url, index with
  | [], Some url, None -> string url
  | _ ->
    list sexp
    @@ record_fields
         [ field_l "depends" Package_dependency.encode packages
         ; field_o "url" string url
         ; field_o "index" string index
         ]
;;

let decode ~toplevel =
  let open Dune_sexp.Decoder in
  let+ res =
    either string
    @@ fields
         (let+ loc, packages =
            located @@ field ~default:[] "depends" (repeat Package_dependency.decode)
          and+ url = field_o "url" string
          and+ index = field_o "index" string in
          let () =
            if toplevel && not (List.is_empty packages)
            then
              User_warning.emit
                ~loc
                [ Pp.textf
                    "The depends field of the documentation stanza can only be non-empty \
                     when the documentation stanza is inside a package stanza."
                ]
          in
          let () =
            if (not toplevel) && Option.is_some index
            then
              User_warning.emit
                ~loc
                [ Pp.textf
                    "The index field of the documentation stanza can only be set at the \
                     top level of dune-project, not inside a package stanza."
                ]
          in
          { packages; url; index })
  in
  match res with
  | Left url -> { packages = []; url = Some url; index = None }
  | Right res -> res
;;

let to_dyn { packages; url; index } =
  let open Dyn in
  record
    [ "url", option string url
    ; "packages", list Package_dependency.to_dyn packages
    ; "index", option string index
    ]
;;

let superpose d1 d2 =
  let url =
    match d2.url with
    | Some _ as u -> u
    | None -> d1.url
  in
  let index =
    match d2.index with
    | Some _ as i -> i
    | None -> d1.index
  in
  { url; packages = d2.packages; index }
;;
