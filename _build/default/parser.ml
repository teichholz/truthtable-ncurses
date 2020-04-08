
module MenhirBasics = struct
  
  exception Error
  
  type token = Tok.token
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState9
  | MenhirState7
  | MenhirState4
  | MenhirState3
  | MenhirState0

let rec _menhir_run7 : _menhir_env -> 'ttv_tail * _menhir_state * (Tok.value) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Tok.LPAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | Tok.NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | Tok.TERMINAL _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | Tok.VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (Tok.value) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Tok.LPAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | Tok.NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | Tok.TERMINAL _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Tok.VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Tok.value) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Tok.AND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
        | Tok.OR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
        | Tok.RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (n : (Tok.value))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Tok.value) = 
# 22 "parser.mly"
                       ( n )
# 89 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Tok.AND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
        | Tok.EOF | Tok.OR | Tok.RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (left : (Tok.value))), _, (right : (Tok.value))) = _menhir_stack in
            let _2 = () in
            let _v : (Tok.value) = 
# 27 "parser.mly"
                                  ( `Binary ('|' ,left, right) )
# 112 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (left : (Tok.value))), _, (right : (Tok.value))) = _menhir_stack in
        let _2 = () in
        let _v : (Tok.value) = 
# 28 "parser.mly"
                                   ( `Binary ('&' ,left, right) )
# 129 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (o : (Tok.value))) = _menhir_stack in
        let _1 = () in
        let _v : (Tok.value) = 
# 20 "parser.mly"
                  ( `Unary ('!', o) )
# 140 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Tok.AND ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
        | Tok.EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (m : (Tok.value))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 11 "parser.mly"
       (Tok.value option)
# 158 "parser.ml"
            ) = 
# 16 "parser.mly"
                 ( Some m )
# 162 "parser.ml"
             in
            _menhir_goto_start _menhir_env _menhir_stack _menhir_s _v
        | Tok.OR ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 1 "parser.mly"
       (char)
# 200 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (v : (
# 1 "parser.mly"
       (char)
# 208 "parser.ml"
    )) = _v in
    let _v : (Tok.value) = 
# 24 "parser.mly"
            ( `Var v )
# 213 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 2 "parser.mly"
       (bool)
# 220 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (t : (
# 2 "parser.mly"
       (bool)
# 228 "parser.ml"
    )) = _v in
    let _v : (Tok.value) = 
# 25 "parser.mly"
                 ( `Term t )
# 233 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Tok.LPAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | Tok.NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | Tok.TERMINAL _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Tok.VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Tok.LPAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | Tok.NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | Tok.TERMINAL _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Tok.VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_goto_start : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (Tok.value option)
# 278 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 11 "parser.mly"
       (Tok.value option)
# 286 "parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 11 "parser.mly"
       (Tok.value option)
# 305 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Tok.EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (
# 11 "parser.mly"
       (Tok.value option)
# 327 "parser.ml"
        ) = 
# 15 "parser.mly"
        ( None )
# 331 "parser.ml"
         in
        _menhir_goto_start _menhir_env _menhir_stack _menhir_s _v
    | Tok.LPAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | Tok.NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | Tok.TERMINAL _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Tok.VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 350 "parser.ml"
