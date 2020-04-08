
(* The type of tokens. *)

type token = Tok.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tok.value option)
