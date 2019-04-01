open! Stdune
open Import
open Stanza.Decoder


module Blang = struct
  include Blang

  let ops =
    [ "=", Op.Eq
    ; ">=", Gte
    ; "<=", Lt
    ; ">", Gt
    ; "<", Lt
    ; "<>", Neq
    ]

  let decode =
    let ops =
      List.map ops ~f:(fun (name, op) ->
        ( name
        , (let+ x = String_with_vars.decode
           and+ y = String_with_vars.decode
           in
           Compare (op, x, y))))
    in
    let decode =
      fix begin fun t ->
        if_list
          ~then_:(
            [ "or", repeat t >>| (fun x -> Or x)
            ; "and", repeat t >>| (fun x -> And x)
            ] @ ops
            |> sum)
          ~else_:(String_with_vars.decode >>| fun v -> Expr v)
      end
    in
    let+ () = Syntax.since Stanza.syntax (1, 1)
    and+ decode = decode
    in
    decode
end

include Blang



