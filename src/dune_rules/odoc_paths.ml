(** Path computation utilities for odoc files *)

open Import

let ( ++ ) = Path.Build.relative

module Doc_mode = Odoc_target.Doc_mode

type sidebar_scope =
  | Per_package of Package.Name.t
  | Global

let odoc_support_dirname = "odoc.support"
let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"

let index_root ctx mode =
  let subdir =
    match mode with
    | Doc_mode.Local_only -> "_index"
    | Doc_mode.Full -> "_index_full"
  in
  root ctx ++ subdir
;;

let maybe_prefix base prefix =
  match prefix with
  | Some p -> base ++ p
  | None -> base
;;

let odocs : type a. Context.t -> ?prefix:string -> a Odoc_target.t -> Path.Build.t =
  fun ctx ?prefix -> function
  | Odoc_target.Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    maybe_prefix (root ctx ++ "_odoc") prefix
    ++ Package.Name.to_string pkg
    ++ Lib_name.to_string lib_name
  | Odoc_target.Private_lib (lib_unique_name, _) ->
    maybe_prefix (root ctx ++ "_odoc") prefix ++ lib_unique_name
  | Odoc_target.Pkg pkg ->
    maybe_prefix (root ctx ++ "_odoc") prefix ++ Package.Name.to_string pkg
  | Odoc_target.Toplevel mode -> index_root ctx mode
;;

let html_root ctx mode = root ctx ++ Doc_mode.html_subdir mode
let json_root ctx mode = root ctx ++ Doc_mode.json_subdir mode
let markdown_root ctx mode = root ctx ++ Doc_mode.markdown_subdir mode
let odocl_root ctx = root ctx ++ "_odocl"
let sherlodoc_root ctx = root ctx ++ "_sherlodoc"

let html : type a. Context.t -> Doc_mode.t -> ?prefix:string -> a Odoc_target.t -> Path.Build.t =
  fun ctx mode ?prefix target ->
  match target with
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    maybe_prefix (html_root ctx mode) prefix
    ++ Package.Name.to_string pkg
    ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) ->
    maybe_prefix (html_root ctx mode) prefix ++ lib_unique_name
  | Pkg pkg ->
    maybe_prefix (html_root ctx mode) prefix ++ Package.Name.to_string pkg
  | Toplevel _ -> html_root ctx mode
;;

(* Source HTML directory: like [html] but with a "src" component between
   the package and library name, matching the odoc_driver convention
   (e.g. <pkg>/src/<lib>/ instead of <pkg>/<lib>/) *)
let html_src ctx mode ?prefix (target : Odoc_target.mod_ Odoc_target.t) =
  match target with
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    maybe_prefix (html_root ctx mode) prefix
    ++ Package.Name.to_string pkg
    ++ "src"
    ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) ->
    maybe_prefix (html_root ctx mode) prefix ++ lib_unique_name
;;

let json : type a. Context.t -> Doc_mode.t -> ?prefix:string -> a Odoc_target.t -> Path.Build.t =
  fun ctx mode ?prefix target ->
  match target with
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    maybe_prefix (json_root ctx mode) prefix
    ++ Package.Name.to_string pkg
    ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) ->
    maybe_prefix (json_root ctx mode) prefix ++ lib_unique_name
  | Pkg pkg ->
    maybe_prefix (json_root ctx mode) prefix ++ Package.Name.to_string pkg
  | Toplevel _ -> json_root ctx mode
;;

let markdown : type a. Context.t -> Doc_mode.t -> ?prefix:string -> a Odoc_target.t -> Path.Build.t =
  fun ctx mode ?prefix target ->
  match target with
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    maybe_prefix (markdown_root ctx mode) prefix
    ++ Package.Name.to_string pkg
    ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) ->
    maybe_prefix (markdown_root ctx mode) prefix ++ lib_unique_name
  | Pkg pkg ->
    maybe_prefix (markdown_root ctx mode) prefix ++ Package.Name.to_string pkg
  | Toplevel _ -> markdown_root ctx mode
;;

let odocl : type a. Context.t -> ?prefix:string -> a Odoc_target.t -> Path.Build.t =
  fun ctx ?prefix -> function
  | Lib (pkg, lib) ->
    let lib_name = Lib.name lib in
    maybe_prefix (odocl_root ctx) prefix
    ++ Package.Name.to_string pkg
    ++ Lib_name.to_string lib_name
  | Private_lib (lib_unique_name, _) ->
    maybe_prefix (odocl_root ctx) prefix ++ lib_unique_name
  | Pkg pkg ->
    maybe_prefix (odocl_root ctx) prefix ++ Package.Name.to_string pkg
  | Toplevel mode -> index_root ctx mode
;;

let gen_mld_dir ctx ?prefix pkg =
  maybe_prefix (root ctx ++ "_mlds") prefix ++ Package.Name.to_string pkg
;;

let lib_mld_dir ctx ?prefix pkg lib_name =
  gen_mld_dir ctx ?prefix pkg ++ Lib_name.to_string lib_name
;;

let lib_index_mld ctx ?prefix pkg lib_name =
  lib_mld_dir ctx ?prefix pkg lib_name ++ "index.mld"
;;
let odoc_support ctx mode = html_root ctx mode ++ odoc_support_dirname
let odoc_support_for_pkg ctx mode ?prefix pkg =
  maybe_prefix (html_root ctx mode) prefix ++ pkg ++ odoc_support_dirname
;;
let toplevel_index_mld ctx mode = index_root ctx mode ++ "index.mld"

let sidebar_root ctx mode =
  let subdir =
    match mode with
    | Doc_mode.Local_only -> "_sidebar"
    | Doc_mode.Full -> "_sidebar_full"
  in
  root ctx ++ subdir
;;

let index_file ctx mode scope =
  match scope with
  | Global -> sidebar_root ctx mode ++ "index.odoc-index"
  | Per_package pkg ->
    sidebar_root ctx mode ++ Package.Name.to_string pkg ++ "index.odoc-index"
;;

let sidebar_file ctx mode scope =
  match scope with
  | Global -> sidebar_root ctx mode ++ "sidebar.odoc-sidebar"
  | Per_package pkg ->
    sidebar_root ctx mode ++ Package.Name.to_string pkg ++ "sidebar.odoc-sidebar"
;;

type output_format =
  | Html
  | Json
  | Markdown

let sidebar_json ctx mode scope output_format =
  let root =
    match output_format with
    | Html -> html_root ctx mode
    | Json -> json_root ctx mode
    | Markdown -> markdown_root ctx mode
  in
  match scope with
  | Global -> root ++ "sidebar.json"
  | Per_package pkg -> root ++ Package.Name.to_string pkg ++ "sidebar.json"
;;

let remap_file ctx = root ctx ++ "_remap" ++ "remap.txt"
