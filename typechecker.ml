let rec typer env tenv term =
  match term with
  | Grammar.TermUnit -> Grammar.TypeBase "unit"
  | Grammar.TermInt _ -> Grammar.TypeBase "int"
  | Grammar.TermString _ -> Grammar.TypeBase "string"
  | Grammar.TermVariable x ->
    (try
      Context.find env x
    with Not_found ->
      failwith (Printf.sprintf "Unbound value %s" x))
  | Grammar.TermAbstraction(x, typ, term) ->
    let env' = (x, typ) :: env in
    let type_of_term = typer env' tenv term in
    Grammar.TypeArrow(typ, type_of_term)
  | Grammar.TermApplication(t1, t2) ->
    let typ_of_t1 = typer env tenv t1 in
    let typ_of_t2 = typer env tenv t2 in
    let typ1, typ2 = Checkutils.arrow_as_tuple typ_of_t1 in
    Checkutils.check_is_subtype typ_of_t2 typ1;
    typ2
  | Grammar.TermRecord l ->
    Grammar.TypeRecord(List.map (fun (x, term) -> x, (typer env tenv term)) l)
  | Grammar.TermLet (x, t, t') ->
    let typ = typer env tenv t in
    let env' = (x, typ) :: env in
    typer env' tenv t'
  | Grammar.TermProjection (t, f) ->
    let typ = typer env tenv t in
    Checkutils.extract_field typ f
  | Grammar.TypeAbstraction (x, t) ->
    let typ = typer env tenv t in
    Grammar.TypeUniversal (x, typ)
  | Grammar.TypeApplication (f, t) ->
    let typ = typer env tenv f in
    let x, _ = Checkutils.type_arrow_as_tuple typ in
    let tenv' = (x, t) :: tenv in
    (match f with
    | Grammar.TypeAbstraction (_, t') ->
      typer env tenv' t'
    (* this can't fail since type_arrow_as_tuple has succeeded *)
    | _ -> assert false)
