let rec subtyping s t = match (s, t) with
  | Grammar.TypeBase(c), Grammar.TypeBase(d) when c = d -> true
  | Grammar.TypeArrow(s1, s2), Grammar.TypeArrow(t1, t2) ->
    subtyping t1 s1 && subtyping s2 t2
  | Grammar.TypeRecord(l1), Grammar.TypeRecord(l2) ->
    List.for_all (
      fun(label2, type_label2) ->
        try
          ignore (List.find (
            fun(label1, type_label1) ->
              not (label1 = label2) || subtyping type_label1 type_label2
          )
          l1);
          true
        with Not_found -> false
    )
    l2
  | _, _ -> false
    
