open Import
open Memo.O
open Odoc_paths

let sp = Printf.sprintf
let mld_ext = Filename.Extension.of_string_exn ".mld"

module Toplevel_index = struct
  type item =
    { name : string
    ; version : Package_version.t option
    ; link : string
    }

  let of_packages packages output_format =
    Package.Name.Map.to_list_map packages ~f:(fun name package ->
      let name = Package.Name.to_string name in
      let extension =
        match (output_format : Output_format.t) with
        | Markdown -> "md"
        | Html | Json -> "html"
      in
      { name; version = Package.version package; link = sp "%s/index.%s" name extension })
  ;;

  let html_list_items t =
    List.map t ~f:(fun { name; version; link } ->
      let link = sp {|<a href="%s">%s</a>|} link name in
      let version_suffix =
        match version with
        | None -> ""
        | Some v -> sp {| <span class="version">%s</span>|} (Package_version.to_string v)
      in
      sp "<li>%s%s</li>" link version_suffix)
    |> String.concat ~sep:"\n      "
  ;;

  let html t =
    sp
      {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>index</title>
    <link rel="stylesheet" href="./%s/odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  </head>
  <body>
    <main class="content">
      <div class="by-name">
      <h2>OCaml package documentation</h2>
      <ol>
      %s
      </ol>
      </div>
    </main>
  </body>
</html>|}
      Paths.odoc_support_dirname
      (html_list_items t)
  ;;

  let string_to_json s = `String s
  let list_to_json ~f l = `List (List.map ~f l)

  let option_to_json ~f = function
    | None -> `Null
    | Some x -> f x
  ;;

  let item_to_json { name; version; link } =
    `Assoc
      [ "name", string_to_json name
      ; ( "version"
        , Option.map ~f:Package_version.to_string version
          |> option_to_json ~f:string_to_json )
      ; "link", string_to_json link
      ]
  ;;

  (** This format is public API. *)
  let to_json items = `Assoc [ "packages", list_to_json items ~f:item_to_json ]

  let json t = Json.to_string (to_json t)

  let markdown t =
    let b = Buffer.create 256 in
    Buffer.add_string b "# OCaml Package Documentation\n\n";
    List.iter t ~f:(fun { name; version; link } ->
      Buffer.add_string b (sp "- [%s](%s)" name link);
      (match version with
       | None -> ()
       | Some v -> Buffer.add_string b (sp " (version %s)" (Package_version.to_string v)));
      Buffer.add_char b '\n');
    Buffer.contents b
  ;;

  let content (output : Output_format.t) t =
    match output with
    | Html -> html t
    | Json -> json t
    | Markdown -> markdown t
  ;;
end

let libs_of_pkg ctx ~pkg =
  let+ { Scope.DB.Lib_entry.Set.libraries; _ } =
    Scope.DB.lib_entries_of_package ctx pkg
  in
  (* Filter out all implementations of virtual libraries *)
  List.filter_map libraries ~f:(fun lib ->
    match Lib.Local.to_lib lib |> Lib.info |> Lib_info.implements with
    | None -> Some lib
    | Some _ -> None)
;;

let entry_modules_by_lib sctx lib =
  let info = Lib.Local.info lib in
  let for_merlin =
    Compilation_mode.Set.of_lib_mode_set (Lib_info.modes info)
    |> Compilation_mode.Set.for_merlin
  in
  Dir_contents.modules_of_local_lib sctx lib ~for_:for_merlin >>| Modules.entry_modules
;;

let entry_modules sctx ~pkg =
  let* l =
    Super_context.context sctx
    |> Context.name
    |> libs_of_pkg ~pkg
    >>| List.filter ~f:(fun lib ->
      Lib.Local.info lib |> Lib_info.status |> Lib_info.Status.is_private |> not)
  in
  let+ l =
    Memo.parallel_map l ~f:(fun l ->
      let+ m = entry_modules_by_lib sctx l in
      l, m)
  in
  Lib.Local.Map.of_list_exn l
;;

let check_mlds_no_dupes ~pkg ~mlds ~path_to_string =
  match
    List.rev_map mlds ~f:(fun ((_path, mld_name) as mld) -> mld_name, mld)
    |> String.Map.of_list
  with
  | Ok m -> m
  | Error (_, (p1, _name1), (p2, _name2)) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (path_to_string p1)
          (path_to_string p2)
      ]
;;

let report_warnings warnings =
  match warnings with
  | [] -> ()
  | _ :: _ ->
    let l =
      warnings
      |> List.map ~f:(fun (mld : Doc_sources.mld) -> Path.Local.to_string mld.in_doc)
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:", "
    in
    User_warning.emit
      [ Pp.textf
          "Dune does not yet support building documentation for files in a non-flat \
           hierarchy. Ignoring %s."
          l
      ]
;;

let mlds sctx pkg =
  let+ files = Packages.mlds sctx pkg in
  let mlds, assets, warnings =
    List.fold_left
      files
      ~init:([], [], [])
      ~f:(fun (mlds, assets, warnings) (mld : Doc_sources.mld) ->
        match Path.Local.explode mld.in_doc with
        | [ name ] ->
          let ext = Filename.extension name in
          if Filename.Extension.Or_empty.check ext mld_ext
          then
            ( (mld.path, Filename.remove_extension name |> Filename.to_string) :: mlds
            , assets
            , warnings )
          else mlds, (mld.path, Filename.to_string name) :: assets, warnings
        | _ -> mlds, assets, mld :: warnings)
  in
  List.rev mlds, List.rev assets, List.rev warnings
;;
