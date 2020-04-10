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


type var =
  | Var of char * bool

type row =
  | Row of var list


let rec table_make val_vars vars  =
  match vars with
  | [] -> [Row ((List.rev (val_vars)))]
  | v :: tl ->
    table_make (Var (v, false) :: val_vars) tl
    @ table_make (Var (v, true) :: val_vars) tl

let table (vars : char list) = table_make [] vars

let rec eval expr (env : var list) =
  let rec lookup var row=
    match row with
    | Var (c, bol) :: tl -> if Char.equal var c then bol else lookup var tl
    | [] -> raise (Failure "Looku error")
  in
  match expr with
  | `Var c -> lookup c env
  | `Term t -> t
  | `Unary (c, v) -> (match c with | '!' -> not (eval v env) )
  | `Binary (c, v1, v2) -> (match c with | '&' -> eval v1 env && eval v2 env
                                         | '|' -> eval v1 env || eval v2 env)

let rec eval_with_table (table : row list) expr =
  match table with
  | Row var_list :: tl -> eval expr var_list :: (eval_with_table tl expr)
  | [] -> []


let rec vars_pp = function
  | Var (v, bol) :: tl -> print_string (Char.to_string v ^ ": " ^ Bool.to_string bol ^ " ") ; ignore (vars_pp tl)
  | [] -> print_newline ()


let parse_with_error lexbuf =
  try Parser.start (Lexer.read) lexbuf with
  | Lexer.SyntaxError msg ->
    print_endline ("Syntax Fehler:\n" ^ msg);
    None
  | Parser.Error ->
    print_endline "Parser Fehler";
    Caml.exit (-1)

let rec fully_eval_expr var_list expr =
    match expr with
    | `Var c as expr-> print_endline ((Char.to_string c) ^ ": " ^ (Bool.to_string (eval expr var_list)))
    | `Term t -> ()
    | `Unary (c, v) as expr -> fully_eval_expr var_list v; print_endline ((output_value expr) ^ ": " ^ (Bool.to_string (eval expr var_list)))
    | `Binary (c, v1, v2) as expr -> fully_eval_expr var_list v1; fully_eval_expr var_list v2; print_endline ((output_value expr) ^ ": " ^ (Bool.to_string (eval expr var_list)))

let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    let vars = vars value in
    let table = table vars in
    value_pp value;
    List.iter table
      ~f:(fun var_list ->
          let Row var_list = var_list in
          let expr_result = eval value var_list in
          (* vars_pp var_list; *) (* print_endline ("->" ^ (Bool.to_string expr_result)) *) fully_eval_expr var_list value)
  | None -> Caml.exit(-1)

let str = "x & y | x & y"

let rec loop () =
  let lexbuf = Lexing.from_channel stdin in
  print_string "> ";
  Stdlib.flush_all ();
  parse_and_print lexbuf;
  loop ()


let () =
  print_endline "Wahrheitstabellen-Loeser v1";
  ignore (loop ())
