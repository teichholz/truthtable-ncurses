open Base
open Stdio
open Lexing
open Tok


let rec output_value = function
  | `Var c -> "(Var " ^ (Char.to_string c) ^ ")"
  | `Term t -> "(Term " ^ (Bool.to_string t) ^ ")"
  | `Unary (c, v) -> "(" ^ (match c with | '!' -> "not " | _ -> "unknown") ^ output_value v ^ ")"
  | `Binary (c, v1, v2) -> "(" ^ output_value v1 ^ (match c with | '&' -> " And " | '|' -> " Or " | _ -> "unknown") ^ output_value v2 ^ ")"

let value_pp v =
  print_endline (output_value v)

(* let v_to_t_list (v : value) : value_var_to_term list = *)

let vars (expr : value) =
  let rec aux lst expr =
    match expr with
    | `Var c -> c :: lst
    | `Term t -> lst
    | `Unary (c, v) -> aux lst v
    | `Binary (c, v1, v2) -> (aux lst v1) @ (aux lst v2) in
  Set.to_list( Set.of_list (module Char) (aux [] expr) )

let rec eval val_vars = function
  | `Var c -> Caml.List.assoc c val_vars
  | `Term t -> t
  | `Unary (c, v) -> (match c with | '!' -> not (eval val_vars v) )
  | `Binary (c, v1, v2) -> (match c with | '&' -> eval val_vars v1 && eval val_vars v2
                                         | '|' -> eval val_vars v1 || eval val_vars v2)


let rec table_make val_vars vars (* expr *) =
  match vars with
  | [] -> [`Row (val_vars)(* , `Val (eval val_vars expr) *)]
  | v :: tl ->
    table_make (`Var (v, true) :: val_vars) tl (* expr *)
    @ table_make (`Var (v, false) :: val_vars) tl (* expr *)

let table (vars : char list) (* expr *) = List.rev (table_make [] vars (* expr *))

let rec table_pp = function
  | `Row [var_list] :: tl ->
    begin
      match var_list with
      | `Var (name, value) -> Char.to_string name ^ Bool.to_string value ^ "| \n" ^ table_pp tl
    end
  | [] -> ""

let parse_with_error lexbuf =
  try Parser.start (Lexer.read) lexbuf with
  | Lexer.SyntaxError msg ->
    print_endline ("Syntax Fehler:\n" ^ msg);
    None
  | Parser.Error ->
    print_endline "Parser Fehler";
    Caml.exit (-1)

let rec parse lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    value_pp value; vars value
  | None -> Caml.exit(-1)


let str = "x & y | x & y"

let () =
  print_endline (table_pp (table (parse (Lexing.from_string str))))
