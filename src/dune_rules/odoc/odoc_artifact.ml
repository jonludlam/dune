open Import
open Odoc_paths

type kind =
  | Module : Odoc_target.mod_ * Odoc_target.mod_ Odoc_target.t -> kind
  | Page : Odoc_target.page * Odoc_target.page Odoc_target.t -> kind
  | Asset : Odoc_target.asset * Odoc_target.page Odoc_target.t -> kind

type t =
  { kind : kind
  ; source : Path.Build.t
  }

let ( ++ ) = Path.Build.relative

let make : type a. source:Path.Build.t -> a -> a Odoc_target.t -> t =
  fun ~source payload target ->
  let kind =
    match target with
    | Lib _ -> Module (payload, target)
    | Private_lib _ -> Module (payload, target)
    | Pkg _ -> Page (payload, target)
  in
  { kind; source }
;;

let asset ~source payload target = { kind = Asset (payload, target); source }
let get_kind t = t.kind
let source_file t = t.source

let visible t =
  match t.kind with
  | Module (mod_, _) -> mod_.Odoc_target.visible
  | Page _ | Asset _ -> true
;;

(* The basename of a module's artifacts is that of its source (the mangled
   object name, e.g. [foo__Bar]); a page's comes from its page name, which may
   differ from its source file name. *)
let basename t =
  match t.kind with
  | Module (_, _) ->
    Path.Build.basename t.source |> Filename.remove_extension |> Filename.to_string
  | Page (page, _) -> page.name
  | Asset (asset, _) -> asset.asset_name
;;

let odoc_dir ctx t =
  match t.kind with
  | Module (_, target) -> Paths.odocs ctx target
  | Page (_, target) -> Paths.odocs ctx target
  | Asset (_, target) -> Paths.odocs ctx target
;;

let odoc_file ctx t =
  let basename = basename t in
  match t.kind with
  | Module (_, target) -> Paths.odocs ctx target ++ (basename ^ ".odoc")
  | Page (_, target) -> Paths.odocs ctx target ++ ("page-" ^ basename ^ ".odoc")
  | Asset (_, target) -> Paths.odocs ctx target ++ ("asset-" ^ basename ^ ".odoc")
;;

let odocl_file ctx t =
  let basename = basename t in
  match t.kind with
  | Module (_, target) -> Paths.odocl ctx target ++ (basename ^ ".odocl")
  | Page (_, target) -> Paths.odocl ctx target ++ ("page-" ^ basename ^ ".odocl")
  | Asset (_, target) -> Paths.odocl ctx target ++ ("asset-" ^ basename ^ ".odocl")
;;

let output_file ctx (output : Output_format.t) t =
  let basename = basename t in
  let suffix = Filename.of_string_exn (Output_format.extension output) in
  match t.kind with
  | Module (_, target) ->
    let base = Paths.output ctx output target in
    (match (output : Paths.output_format) with
     | Html | Json ->
       base ++ Stdune.String.capitalize basename ++ "index"
       |> Path.Build.extend_basename ~suffix
     | Markdown ->
       base ++ Stdune.String.capitalize basename |> Path.Build.extend_basename ~suffix)
  | Page (_, target) ->
    Paths.output ctx output target ++ basename |> Path.Build.extend_basename ~suffix
  | Asset (_, target) -> Paths.output ctx output target ++ basename
;;
