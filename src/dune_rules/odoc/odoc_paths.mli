open Import

module Paths : sig
  type output_format =
    | Html
    | Json
    | Markdown

  val odoc_support_dirname : string
  val root : Context.t -> Path.Build.t
  val odocs : Context.t -> 'a Odoc_target.t -> Path.Build.t
  val output_root : Context.t -> output_format -> Path.Build.t
  val html_root : Context.t -> Path.Build.t
  val markdown_root : Context.t -> Path.Build.t
  val odocl_root : Context.t -> Path.Build.t
  val output : Context.t -> output_format -> 'a Odoc_target.t -> Path.Build.t
  val html : Context.t -> 'a Odoc_target.t -> Path.Build.t
  val markdown : Context.t -> 'a Odoc_target.t -> Path.Build.t
  val odocl : Context.t -> 'a Odoc_target.t -> Path.Build.t
  val gen_mld_dir : Context.t -> Package.Name.t -> Path.Build.t
  val odoc_support : Context.t -> Path.Build.t
  val toplevel_index : Context.t -> Path.Build.t
  val markdown_index : Context.t -> Path.Build.t
end

module Output_format : sig
  type t = Paths.output_format =
    | Html
    | Json
    | Markdown

  val all : t list
  val iter : f:(t -> unit Memo.t) -> unit Memo.t
  val extension : t -> string
  val args : t -> 'a Command.Args.t
  val alias : t -> dir:Path.Build.t -> Alias.t
  val toplevel_index_path : t -> Context.t -> Path.Build.t
end
