open Import
open Odoc_target
open Odoc_paths

type t =
  { odoc_file : Path.Build.t
  ; target : target
  }

let ( ++ ) = Path.Build.relative
let make ~target odoc_file = { odoc_file; target }
let odoc_file t = t.odoc_file
let target t = t.target

let basename t =
  Path.Build.basename t.odoc_file |> Filename.remove_extension |> Filename.to_string
;;

let odocl_file ctx t = Paths.odocl ctx t.target ++ (basename t ^ ".odocl")

let output_file ctx (output : Output_format.t) t =
  let basename = basename t in
  let suffix = Filename.of_string_exn (Output_format.extension output) in
  match t.target with
  | Lib _ ->
    (match output with
     | Html | Json ->
       Paths.html ctx t.target ++ Stdune.String.capitalize basename ++ "index"
       |> Path.Build.extend_basename ~suffix
     | Markdown ->
       Paths.markdown ctx t.target ++ Stdune.String.capitalize basename
       |> Path.Build.extend_basename ~suffix)
  | Pkg _ ->
    let base =
      match output with
      | Markdown -> Paths.markdown ctx t.target
      | Html | Json -> Paths.html ctx t.target
    in
    base ++ (basename |> String.drop_prefix ~prefix:"page-" |> Option.value_exn)
    |> Path.Build.extend_basename ~suffix
;;
