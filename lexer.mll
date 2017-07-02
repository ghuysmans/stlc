{
  exception IllegalCharacter of char
  exception UnterminatedComment

  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      {
        pos with Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
                 Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
      }

  let inner_comments_number = ref 0
}


let id = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let num = '0' | '-'? ['1'-'9']['0'-'9']*
let white = [' ' '\t' '\r']
let newline = ['\n']

rule top = parse
| white { top lexbuf }
| newline { next_line lexbuf; top lexbuf }
| "fun" | "\\" { Parser.LAMBDA }
| '{' { Parser.LEFT_CURLY }
| '}' { Parser.RIGHT_CURLY }
| "." { Parser.DOT }
| ";" { Parser.SEMICOLON }
| '(' { Parser.LEFT_PARENT }
| ')' { Parser.RIGHT_PARENT }
| "=" { Parser.EQUAL }
| "int" { Parser.INT }
| "string" { Parser.STRING }
| "let" { Parser.LET }
| "in" { Parser.IN }
| id as l { Parser.ID l } 
| ":" { Parser.COLON }
| num as n { Parser.LINT (int_of_string n) }
| "\"" [^'"']* "\"" as s { Parser.LSTRING s }
| "->" { Parser.ARROW }
| "(*" { comment lexbuf; top lexbuf }
| eof { Parser.EOF }

and comment = parse
  | "*)" {
   if (!inner_comments_number) = 0
   then ()
   else (
     inner_comments_number := (!inner_comments_number) - 1;
     comment lexbuf
   )
  }
  | newline { next_line lexbuf;
              comment lexbuf
            }
  | "(*" {
   incr inner_comments_number;
   comment lexbuf
  }
  | eof { raise UnterminatedComment }
  | _ { comment lexbuf }

