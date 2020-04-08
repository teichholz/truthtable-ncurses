type token =
  | VAR of char
  | TERMINAL of bool
  | NOT | AND | OR
  | LPAR | RPAR
  | EOF

(* AST *)
type value = [
  | `Var of char
  | `Binary of char * value * value
  | `Unary of char * value
  | `Term of bool
]

type value_var_to_term = [
  | `Var of char * bool
  | `Binary of char * value * value
  | `Unary of char * value
  | `Term of bool
]
