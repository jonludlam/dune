open Stdune

module Mutator : sig
  open OpamParserTypes

  type t

  val (>>>) : t -> t -> t

  val mkstring : string -> value

  val set_var : string -> value -> t

  (** [set_string v s] is a mutator that sets the opam variable [v] to the
      string [s]. If [v] is already bound in the opamfile the value is updated. 
      If [v] is not present in the opam file it is inserted at the top of the file *)
  val set_string : string -> string -> t

  (** [set_list v conv l] is a mutator that sets the opam variable [v] to the list
      [l] after applying the convertor [conv] to the elements of [l]. If [v] is already
      bound in the opamfile the value is updated. If [v] is not present in the opam 
      file it is inserted at the top of the file. *)
  val set_list : string -> ('a -> value) -> 'a list -> t

  (** [opt v f] returns an identity transformer if [v] is None and if it is
      [Some x] applies [f] to [x] to return a transformer. Useful for constructing a
      mutator that is only applied if an optional value has been given.  *)
  val opt : 'a option -> ('a -> t) -> t

  (** [list v f] returns an identity transformer if [v] is the empty list, and if
      not returns a transformer with the semantics of {v:set_list} *)
  val list : 'a list -> ('a list -> t) -> t

  (** [apply t] returns a function that applies the transformation [t] to an
      {{val:OpamParserTypes.opamfile}opamfile} *)
  val apply : t -> OpamParserTypes.opamfile -> OpamParserTypes.opamfile
end 
 = struct
  open OpamParserTypes

  type t = opamfile_item list -> opamfile_item list

  let (>>>) : t -> t -> t = fun x y z -> y (x z)

  let nopos = ("",0,0) (* Null position *)

  let _remove_var : string -> t =
    fun str -> List.filter ~f:(function
      | Variable (_, v, _) when v=str -> false
      | _ -> true)

  let add_var : string -> OpamParserTypes.value -> t = fun var value l ->
    (Variable (nopos, var, value))::l

  let remap x f =
    List.filter_map ~f:(function
      | Variable (_, v, y) when v = x -> begin
        match f (Some y) with
        | Some y' -> Some (Variable (nopos, v, y'))
        | None -> None
        end
      | z -> Some z)
  
  let binding_present x =
    List.exists ~f:(function
      | Variable (_, v, _) when v = x -> true
      | _ -> false)

  let _map_var x f zs =
    if binding_present x zs
    then remap x f zs
    else begin
      match f None with
      | Some y -> (Variable (nopos, x, y))::zs
      | None -> zs
    end

  let set_var x y zs =
    if binding_present x zs
    then remap x (fun _ -> Some y) zs
    else add_var x y zs

  let mkstring x = String (nopos, x)
  let mklist f xs = List (nopos, List.map ~f xs)

  let set_string x y = set_var x (mkstring y)

  let set_list x conv l = set_var x (mklist conv l)
  let id x = x

  let opt opt f : t = match opt with | None -> id | Some x -> f x
  let list l f : t = match l with [] -> id | xs -> f xs

  let apply t opamfile =
    {
      opamfile with
      file_contents = t opamfile.file_contents
    }
end

let correct_specific _project package =
  let open Mutator in
  let open Dune_project.Opam_package in
  set_string "synopsis" package.synopsis >>>
  set_string "description" package.description

let correct project package_name =
  let open Mutator in
  opt (Dune_project.opam_package project package_name) (correct_specific project) >>>
  opt (Dune_project.license project) (set_string "license") >>>
  list (Dune_project.authors project) (set_list "authors" mkstring) >>>
  opt (Dune_project.version project) (set_string "version") >>>
  opt (Option.map ~f:(Format.asprintf "%a" Dune_project.Source_kind.pp) (Dune_project.source project)) (set_string "dev-repo")

let add_rules sctx ~dir =
  let open Build.O in
  let scope = Super_context.find_scope_by_dir sctx dir in
  let project = Scope.project scope in
  Local_package.defined_in sctx ~dir
  |> List.iter ~f:(fun pkg ->
    let opam_path = Local_package.opam_file pkg in
    let expected_path = Path.extend_basename opam_path ~suffix:".expected" in
    let rule =
      Build.contents opam_path >>^ (fun contents ->
      let opamfile = Lexing.from_string contents |> Opam_file.parse in
      let package_name = Local_package.name pkg |> Package.Name.to_string in
      let corrected = Mutator.apply (correct project package_name) opamfile in
      OpamPrinter.opamfile corrected) >>>
      Build.write_file_dyn expected_path
    in
    let ctx = Super_context.context sctx in
    Super_context.add_rule sctx ~dir:ctx.build_dir rule)