type term =
  | TermUnit
  | TermInt of int
  | TermString of string
  | TermVariable of string
  | TermAbstraction of string * typ * term
  | TermApplication of term * term
  | TermRecord of (string * term) list
  | TermLet of string * term * term
  | TermProjection of term * string

and typ =
  | TypeBase of string
  | TypeArrow of typ * typ
  | TypeRecord of (string * typ) list

type raw_top_level_term =
  | Top_level_let of string * term
