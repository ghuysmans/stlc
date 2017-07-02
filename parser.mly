%token LAMBDA
%token LEFT_CURLY
%token RIGHT_CURLY
%token DOT
%token SEMICOLON
%token LEFT_PARENT
%token RIGHT_PARENT
%token EQUAL
%token INT
%token STRING
%token LET
%token IN
%token <string> ID
%token ARROW
%right ARROW
%token COLON
%token <int> LINT
%token <string> LSTRING
%token EOF

%start <Grammar.raw_top_level_term> top_level_term
%%

top_level_term:
| LET ; x = ID ; EQUAL ; t = term ; SEMICOLON ; SEMICOLON { Grammar.Top_level_let (x, t) }
| EOF { raise End_of_file }

term:
| LAMBDA ; LEFT_PARENT ; x = ID ; COLON ; t = typ ; RIGHT_PARENT ; ARROW ; tt = term { Grammar.TermAbstraction (x, t, tt) }
| LEFT_CURLY ; l = record_term ; RIGHT_CURLY { Grammar.TermRecord l }
| i = LINT { Grammar.TermInt i }
| LEFT_PARENT ; RIGHT_PARENT { Grammar.TermUnit }
| LEFT_PARENT ; t = term ; RIGHT_PARENT { t }
| s = LSTRING { Grammar.TermString s }
| LET ; x = ID ; EQUAL ; t = term ; IN ; tt = term { Grammar.TermLet (x, t, tt) }
| t = term ; DOT ; id = ID { Grammar.TermProjection (t, id) }
| i = ID { Grammar.TermVariable i }
(*
FIXME stratification
term:
  ...
  t params
params:
  t
  t params
*)
| t = term ; tt = term { Grammar.TermApplication (t, tt) }

record_term:
| x = ID ; EQUAL ; t = term { [x, t] }
| x = ID ; EQUAL ; t = term ; SEMICOLON ; l = record_term { (x, t) :: l }

record_type:
| x = ID ; COLON ; t = typ { [x, t] }
| x = ID ; COLON ; t = typ ; SEMICOLON ; l = record_type { (x, t) :: l }

typ:
| LEFT_PARENT ; t = typ ; RIGHT_PARENT { t }
| INT { Grammar.TypeBase "int" }
| STRING { Grammar.TypeBase "string" }
| LEFT_CURLY ; t = record_type ; RIGHT_CURLY { Grammar.TypeRecord t }
| inp = typ ; ARROW ; ret = typ { Grammar.TypeArrow (inp, ret) }
