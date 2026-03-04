type t = { name : string }

let create name = { name }

let greet t = "Hello, " ^ t.name
