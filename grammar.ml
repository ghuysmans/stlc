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

let base_string = TypeBase "String"
let base_int = TypeBase "Integer"
let base_unit = TypeBase "Unit"

type raw_top_level_term =
  | Top_level_let of string * term

(* Some examples. *)
(*
let print_infered_type term =
  let type_of_term = typer [] term in
  Printf.printf "%s\n" (string_of_typ type_of_term)

let test0 = TermAbstraction("x", base_int, TermVariable("x"));;
let test1 = TermAbstraction("x",
                            base_string,
                            TermAbstraction("y",
                                            base_int,
                                            TermApplication(
                                              TermVariable("x"),
                                              TermVariable("y")
                                            )
                                           )
                           );;
let test2 = TermAbstraction("x",
                            TypeArrow(base_string, base_int),
                            TermAbstraction("y",
                                            base_int,
                                            TermApplication(
                                              TermVariable("x"),
                                              TermVariable("y")
                                            )
                                           )
                           );;
let test3 = TermAbstraction("x",
                            TypeArrow(base_int, base_string),
                            TermAbstraction("y",
                                            base_int,
                                            TermApplication(
                                              TermVariable("x"),
                                              TermVariable("y")
                                            )
                                           )
                           );;

let test4 = TermRecord(
    [("test0", test0) ; ("test3", test3)]
  );;
*)
(* print_infered_type test0;; *)
(* print_infered_type test1;; *)
(* print_infered_type test2;; *)
(* print_infered_type test3;; *)
(* print_infered_type test4;; *)
