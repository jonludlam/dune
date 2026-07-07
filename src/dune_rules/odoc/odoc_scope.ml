open Import
open Memo.O

let file_key project =
  let name = Dune_project.name project in
  let root = Dune_project.root project in
  let digest =
    Digest.repr Repr.(pair Dune_project_name.repr Path.Source.repr) (name, root)
    |> Digest.to_string
  in
  String.take digest 12
;;

let find_project_by_key =
  let memo =
    let make_map projects =
      String.Map.of_list_map_exn projects ~f:(fun project -> file_key project, project)
      |> Memo.return
    in
    let module Input = struct
      type t = Dune_project.t list

      let equal = List.equal Dune_project.equal
      let hash = List.hash Dune_project.hash
      let to_dyn = Dyn.list Dune_project.to_dyn
    end
    in
    Memo.create "project-by-keys" ~input:(module Input) make_map
  in
  fun key ->
    let* projects = Dune_load.projects () in
    let+ map = Memo.exec memo projects in
    String.Map.find_exn map key
;;

module Scope_id = struct
  type t =
    | Package of Package.Name.t
    | Private_lib of
        { unique_name : string
        ; lib_name : Lib_name.t
        ; project : Dune_project.t
        }

  let of_string s =
    match String.rsplit2 s ~on:'@' with
    | None -> Memo.return (Package (Package.Name.of_string s))
    | Some (lib, key) ->
      let+ project = find_project_by_key key in
      let lib_name = Lib_name.parse_string_exn (Loc.none, lib) in
      Private_lib { unique_name = s; lib_name; project }
  ;;
end

module Scope_key : sig
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end = struct
  let of_string context s =
    match String.rsplit2 s ~on:'@' with
    | None ->
      let+ public_libs = Scope.DB.public_libs context in
      Lib_name.parse_string_exn (Loc.none, s), public_libs
    | Some (lib, key) ->
      let+ scope = find_project_by_key key >>= Scope.DB.find_by_project context in
      Lib_name.parse_string_exn (Loc.none, lib), Scope.libs scope
  ;;

  let to_string lib project =
    let key = file_key project in
    sprintf "%s@%s" (Lib_name.to_string lib) key
  ;;
end

let lib_unique_name lib =
  let name = Lib.name lib in
  let info = Lib.info lib in
  let status = Lib_info.status info in
  match status with
  | Installed_private | Installed -> assert false
  | Public _ -> Lib_name.to_string name
  | Private (project, _) -> Scope_key.to_string name project
;;
