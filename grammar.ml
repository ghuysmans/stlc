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
  (* those are terms!! *)
  | TypeAbstraction of string * term
  | TypeApplication of term * typ

and typ =
  | TypeBase of string
  | TypeVariable of string (* beware of cycles *)
  | TypeArrow of typ * typ
  | TypeRecord of (string * typ) list
  | TypeUniversal of string * typ

type raw_top_level_term =
  | Top_level_let of string * term
  | Top_level_type of string * typ
