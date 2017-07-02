let rec typer env term =
  match term with
  | Grammar.TermUnit -> Grammar.TypeBase "unit"
  | Grammar.TermInt _ -> Grammar.TypeBase "int"
  | Grammar.TermString _ -> Grammar.TypeBase "string"
  | Grammar.TermVariable x ->
    (try
      List.assoc x env
    with Not_found ->
      failwith (Printf.sprintf "Unbound value %s" x))
  | Grammar.TermAbstraction(x, typ, term) ->
    let env' = (x, typ) :: env in
    let type_of_term = typer env' term in
    Grammar.TypeArrow(typ, type_of_term)
  | Grammar.TermApplication(t1, t2) ->
    let typ_of_t1 = typer env t1 in
    let typ_of_t2 = typer env t2 in
    let typ1, typ2 = Checkutils.arrow_as_tuple typ_of_t1 in
    Checkutils.check_is_subtype typ_of_t2 typ1;
    typ2
  | Grammar.TermRecord l ->
    Grammar.TypeRecord(List.map (fun (x, term) -> x, (typer env term)) l)
  | Grammar.TermLet (x, t, t') ->
    let typ = typer env t in
    let env' = (x, typ) :: env in
    typer env' t'
  | Grammar.TermProjection (t, f) ->
    let typ = typer env t in
    Checkutils.extract_field typ f
