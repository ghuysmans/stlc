type 'a t = (string * 'a) list

let empty = [];;

let find env variable = List.assoc variable env

let add env variable typ = (variable, typ) :: env

let iter = List.iter
