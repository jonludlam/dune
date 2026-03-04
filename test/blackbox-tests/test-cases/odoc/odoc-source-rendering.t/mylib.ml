let hello () = "hello"

type t = { name : string }

let greet t = hello () ^ ", " ^ t.name
