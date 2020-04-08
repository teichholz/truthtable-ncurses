%token <char> VAR
%token <bool> TERMINAL
%token NOT AND OR
%token LPAR RPAR
%token EOF

%left OR
%left AND
%nonassoc NOT

%start <Tok.value option> start
%%

start:
  | EOF { None }
  | m = expr EOF { Some m }

expr:
  /* unary */
  | NOT; o = expr { `Unary ('!', o) }
  /* paren */
  | LPAR n = expr RPAR { n }
  /* Term */
  | v = VAR { `Var v }
  | t = TERMINAL { `Term t }
  /* binary */
  | left = expr; OR; right = expr { `Binary ('|' ,left, right) }
  | left = expr; AND; right = expr { `Binary ('&' ,left, right) }
