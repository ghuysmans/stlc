let top_level_term_as_tuple term = match term with
  | Grammar.Top_level_let (x, t) -> x, t

let env = ref Context.empty;;

let rec read_file lexbuf =
  try
    let top = Parser.top_level_term Lexer.top lexbuf in
    let x, t = top_level_term_as_tuple top in
    let typ = Typechecker.typer (!env) t in
    env := Context.add (!env) x typ;
    Printer.string_of_raw_top_level_term top |> print_endline;
    Printf.printf "val %s : %s\n" x (Printer.string_of_typ typ);
    read_file lexbuf
  with End_of_file -> ()

let () =
  let ch =
    if Array.length Sys.argv = 1 then
      stdin
    else
      let filename = Sys.argv.(1) in
      open_in filename
  in read_file (Lexing.from_channel ch)


