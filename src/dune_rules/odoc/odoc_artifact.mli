open Import

type t

val make : target:Odoc_target.target -> Path.Build.t -> t
val odoc_file : t -> Path.Build.t
val target : t -> Odoc_target.target
val basename : t -> string
val odocl_file : Context.t -> t -> Path.Build.t
val output_file : Context.t -> Odoc_paths.Output_format.t -> t -> Path.Build.t
