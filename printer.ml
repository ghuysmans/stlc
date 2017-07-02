let rec string_of_term term = match term with
  | Grammar.TermUnit -> "()"
  | Grammar.TermInt i -> string_of_int i
  | Grammar.TermString s -> s
  | Grammar.TermVariable x -> x
  | Grammar.TermAbstraction(x, typ, term) ->
    Printf.sprintf
      "(λ(%s : %s) -> %s)"
      x
      (string_of_typ typ)
      (string_of_term term)
  | Grammar.TermApplication(term1, term2) ->
    Printf.sprintf
      "(%s %s)"
      (string_of_term term1)
      (string_of_term term2)
  | Grammar.TermRecord(l) ->
    Printf.sprintf
      "{ %s }"
      ((List.map (fun (x, term) -> x ^ " : " ^ (string_of_term term)) l) |> String.concat " ; ")
  | Grammar.TermLet (x, t, t') ->
    Printf.sprintf
      "let %s = %s in %s"
      x
      (string_of_term t)
      (string_of_term t')
  | Grammar.TermProjection (t, f) ->
    Printf.sprintf
      "%s.%s"
      (string_of_term t)
      f

and string_of_raw_top_level_term t = match t with
  | Grammar.Top_level_let (x, t) ->
    Printf.sprintf
      "let %s = %s;;"
      x
      (string_of_term t)

and string_of_typ typ = match typ with
  | Grammar.TypeBase t -> t
  | Grammar.TypeArrow(typ1, typ2) ->
    Printf.sprintf
      "(%s → %s)"
      (string_of_typ typ1)
      (string_of_typ typ2)
  | Grammar.TypeRecord l ->
    Printf.sprintf
      "{ %s }"
      ((List.map (fun (x, typ) -> x ^ " : " ^ (string_of_typ typ)) l) |> String.concat " ; ")
