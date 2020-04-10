{
  open Lexing
  open Tok
  open Base

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let var = ['a'-'z' 'A'-'Z']

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { EOF }
  | "true" { TERMINAL true }
  | "false" { TERMINAL false }
  | '&' { AND }
  | '|' { OR }
  | '!' { NOT }
  | '(' { LPAR }
  | ')' { RPAR }
  | var { VAR (Char.of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (SyntaxError ("Unerwartetes Zeichen:" ^ Lexing.lexeme lexbuf)) }
