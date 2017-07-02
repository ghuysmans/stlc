let check_is_arrow typ = match typ with
  | Grammar.TypeArrow _ -> ()
  | _ -> failwith (Printf.sprintf "Type %s is not an arrow" (Printer.string_of_typ typ))

let arrow_as_tuple typ = match typ with
  | Grammar.TypeArrow(typ1, typ2) -> typ1, typ2
  | _ -> failwith (Printf.sprintf "Type %s is not an arrow" (Printer.string_of_typ typ))

let check_is_subtype typ1 typ2 =
  if not @@ Subtyping.subtyping typ1 typ2
  then failwith (Printf.sprintf
                  "%s is not a subtype of %s"
                  (Printer.string_of_typ typ1)
                  (Printer.string_of_typ typ2))

let rec check_is_same_type typ1 typ2 = match (typ1, typ2) with
  | Grammar.TypeBase x, Grammar.TypeBase y ->
    if x <> y then
      failwith
        (Printf.sprintf
           "%s and %s must be the same type"
           x
           y
        )
  | Grammar.TypeArrow(t1, t2), Grammar.TypeArrow(u1, u2) ->
    check_is_same_type t1 u1;
    check_is_same_type t2 u2
  | _ -> failwith (Printf.sprintf
                  "%s is not the same type as %s"
                  (Printer.string_of_typ typ1)
                  (Printer.string_of_typ typ2)
               )

let extract_field t f = match t with
  | Grammar.TypeRecord l ->
    (try
      List.assoc f l
    with Not_found ->
      failwith (Printf.sprintf
                  "%s doesn't have any %s field"
                  (Printer.string_of_typ t)
                  f
               ))
  | _ -> failwith (Printf.sprintf
                  "%s is not a record with %s"
                  (Printer.string_of_typ t)
                  f
               )
