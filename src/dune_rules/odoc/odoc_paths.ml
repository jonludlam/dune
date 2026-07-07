open Import
open Odoc_target

let ( ++ ) = Path.Build.relative

module Paths = struct
  type output_format =
    | Html
    | Json
    | Markdown

  let output_subdir = function
    | Html | Json -> "_html"
    | Markdown -> "_markdown"
  ;;

  let odoc_support_dirname = "odoc.support"
  let root (context : Context.t) = Path.Build.relative (Context.build_dir context) "_doc"
  let odoc_root ctx = root ctx ++ "_odoc"

  (* Public libraries nest under their package: <root>/<pkg>/<lib>. Private
     libraries use their own <root>/<lnu>; package pages live at
     <root>/<pkg>. *)
  let target_subdir : type a. Path.Build.t -> a Odoc_target.t -> Path.Build.t =
    fun base -> function
    | Lib (pkg, lib) ->
      base
      ++ Package.Name.to_string pkg
      ++ Lib_name.to_string (Lib.name (Lib.Local.to_lib lib))
    | Private_lib (lib_unique_name, _) -> base ++ lib_unique_name
    | Pkg pkg -> base ++ Package.Name.to_string pkg
  ;;

  let odocs : type a. Context.t -> a Odoc_target.t -> Path.Build.t =
    fun ctx m -> target_subdir (odoc_root ctx) m
  ;;

  let output_root ctx format = root ctx ++ output_subdir format
  let html_root ctx = output_root ctx Html
  let markdown_root ctx = output_root ctx Markdown
  let odocl_root ctx = root ctx ++ "_odocls"
  let output ctx format target = target_subdir (output_root ctx format) target
  let html ctx m = target_subdir (html_root ctx) m
  let markdown ctx m = target_subdir (markdown_root ctx) m
  let odocl ctx m = target_subdir (odocl_root ctx) m
  let gen_mld_dir ctx pkg = root ctx ++ "_mlds" ++ Package.Name.to_string pkg
  let odoc_support ctx = html_root ctx ++ odoc_support_dirname
  let toplevel_index ctx = html_root ctx ++ "index.html"
  let markdown_index ctx = markdown_root ctx ++ "index.md"
end

module Output_format = struct
  type t = Paths.output_format =
    | Html
    | Json
    | Markdown

  let all = [ Html; Json; Markdown ]
  let iter ~f = Memo.parallel_iter all ~f

  let extension = function
    | Html -> ".html"
    | Json -> ".html.json"
    | Markdown -> ".md"
  ;;

  let args = function
    | Html -> Command.Args.empty
    | Json -> A "--as-json"
    | Markdown -> Command.Args.empty
  ;;

  let alias t ~dir =
    match t with
    | Html -> Alias.make Alias0.doc ~dir
    | Json -> Alias.make Alias0.doc_json ~dir
    | Markdown -> Alias.make Alias0.doc_markdown ~dir
  ;;

  let toplevel_index_path format ctx =
    let base = Paths.toplevel_index ctx in
    match format with
    | Html -> base
    | Json -> Path.Build.extend_basename base ~suffix:Filename.json
    | Markdown -> Paths.markdown_index ctx
  ;;
end
