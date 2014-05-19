(*
  Program ::= Stmts
  Stmts ::= Stmt | Stmt ';' Stmts
  Stmt ::= Assign | IfElse | While
  Assign ::= Identifier ':=' AExp
  IfElse ::= 'if' BExp 'then' '{' Stmts '}' 'else' '{' Stmts '}'
  While ::= 'while' BExp 'do' '{' Stmts '}'

  Exp ::= OrExp
  OrExp ::= AndExp ( 'or' AndExp )*
  AndExp ::= ROpExp (' and' ROpExp )*
  ROpExp ::= PlusSubExp [ ('>' | '<') PlusSubExp ]
  PlusSubExp ::= MulDivExp ( ['+' | '-'] MulDivExp )*
  MulDivExp ::= PrimaryExp ( ['*' | '/'] PrimaryExp )*
  PrimaryExp ::= '(' Exp ')' | Identifier | Number | Bool

  Bool ::= 'true' | 'false'
  Number ::= ([0-9])+
  Identifier ::= [A-Za-z][a-zA-Z0-9]*
*)

type token = Bool of bool
           | Number of int
           | Identifier of string
           | SemiColon
           | LeftBracket
           | RightBracket
           | LeftParen
           | RightParen
           | KeywordIf
           | KeywordThen
           | KeywordElse
           | KeywordWhile
           | KeywordDo
           | KeywordAnd
           | KeywordOr
           | OpAssign
           | OpPlus
           | OpSub
           | OpMul
           | OpDiv
           | OpLT
           | OpGT

type exp = PlusExp of exp * exp
         | SubExp of exp * exp
         | MulExp of exp * exp
         | DivExp of exp * exp
         | Variable of string
         | NumLiteral of int
         | LTExp of exp * exp
         | GTExp of exp * exp
         | AndExp of exp * exp
         | OrExp of exp * exp
         | BoolLiteral of bool

type prog = Stmts of prog list
          | Assign of string * exp
          | IfElse of exp * prog * prog
          | While of exp * prog

exception Invalid_char of char
exception Syntax_error
exception Runtime_error

(* lexer *)
let rec lexer_of_channel channel =
  let src = Stream.of_channel channel in
  Stream.from (function _ -> lex src)

and lexer_of_string str =
  let src = Stream.of_string str in
  Stream.from (function _ -> lex src)

and lex stream =
  if Stream.peek stream = None then None
  else
    match Stream.next stream with
      (* skip whitespace *)
      | ' ' | '\t' | '\r' | '\n' -> lex stream
      (* identifier and keyword *)
      | ('A'..'Z' | 'a'..'z' as ch) ->
          let buffer = Buffer.create 10 in
          Buffer.add_char buffer ch;
          Some (lex_ident stream buffer)
      (* numbers *)
      | ('0'..'9' as ch) ->
          let buffer = Buffer.create 12 in
          Buffer.add_char buffer ch;
          Some (lex_number stream buffer)
      (* assignment operator *)
      | ':' when Stream.peek stream = Some '=' ->
          Stream.junk stream;
          Some OpAssign
      (* other tokens *)
      | ';' -> Some SemiColon
      | '{' -> Some LeftBracket
      | '}' -> Some RightBracket
      | '(' -> Some LeftParen
      | ')' -> Some RightParen
      | '+' -> Some OpPlus
      | '-' -> Some OpSub
      | '*' -> Some OpMul
      | '/' -> Some OpDiv
      | '<' -> Some OpLT
      | '>' -> Some OpGT
      | _ as ch -> raise (Invalid_char ch)

and lex_ident stream buffer =
  match Stream.peek stream with
    | Some ('A'..'Z' | 'a'..'z' | '0'..'9' as ch) ->
        Buffer.add_char buffer ch;
        Stream.junk stream;
        lex_ident stream buffer
    | _ ->
        let ident = Buffer.contents buffer in
        begin
          match ident with
            | "true"  -> Bool true
            | "false" -> Bool false
            | "if"    -> KeywordIf
            | "then"  -> KeywordThen
            | "else"  -> KeywordElse
            | "while" -> KeywordWhile
            | "do"    -> KeywordDo
            | "and"   -> KeywordAnd
            | "or"    -> KeywordOr
            | _       -> Identifier ident
        end

and lex_number stream buffer =
  match Stream.peek stream with
    | Some ('0'..'9' as ch) ->
        Buffer.add_char buffer ch;
        Stream.junk stream;
        lex_number stream buffer
    | _ -> Number (int_of_string (Buffer.contents buffer))

(* parser *)
let rec parse stream = parse_stmts stream

and parse_stmts stream =
  let stmt = parse_stmt stream in
  let rec loop () =
    match Stream.peek stream with
      | Some SemiColon ->
          Stream.junk stream;
          let stmt = parse_stmt stream in
          stmt :: (loop ())
      | _ -> []
  in
  let rest = loop () in
  if rest = [] then stmt else Stmts (stmt :: rest)

and parse_stmt stream =
  match Stream.peek stream with
    | Some KeywordIf -> parse_if_else stream
    | Some KeywordWhile -> parse_while stream
    | Some (Identifier _) -> parse_assign stream
    | _ -> raise Syntax_error

and parse_if_else stream =
  Stream.junk stream; (* 'if' *)
  (* if *)
  let pred = parse_exp stream in
  gulp stream KeywordThen; (* 'then' *)
  gulp stream LeftBracket; (* '{' *)
  (* then *)
  let thn = parse_stmts stream in
  gulp stream RightBracket; (* '}' *)
  gulp stream KeywordElse;  (* 'else' *)
  gulp stream LeftBracket;  (* '{' *)
  (* else *)
  let els = parse_stmts stream in
  gulp stream RightBracket; (* '}' *)
  IfElse (pred, thn, els)

and parse_while stream =
  Stream.junk stream; (* 'while' *)
  (* guard *)
  let guard = parse_exp stream in
  gulp stream KeywordDo;
  gulp stream LeftBracket;
  (* body *)
  let body = parse_stmts stream in
  gulp stream RightBracket;
  While (guard, body)

and parse_assign stream =
  let var = Stream.next stream in
  gulp stream OpAssign; (* ':=' *)
  let rhs = parse_exp stream in
  match var with
    | Identifier lhs -> Assign (lhs, rhs)
    | _ -> raise Syntax_error

and parse_exp stream = parse_or_exp stream

and parse_or_exp stream =
  let lhs = parse_and_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
      | Some KeywordOr ->
          Stream.junk stream;
          let rhs = parse_and_exp stream in
          loop (OrExp (lhs, rhs))
      | _ -> lhs
  in
  loop lhs

and parse_and_exp stream =
  let lhs = parse_rop_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
      | Some KeywordAnd ->
          Stream.junk stream;
          let rhs = parse_rop_exp stream in
          loop (AndExp (lhs, rhs))
      | _ -> lhs
  in
  loop lhs

and parse_rop_exp stream =
  let lhs = parse_plus_sub_exp stream in
  match Stream.peek stream with
    | Some OpLT ->
        Stream.junk stream;
        let rhs = parse_plus_sub_exp stream in LTExp (lhs, rhs)
    | Some OpGT ->
        Stream.junk stream;
        let rhs = parse_plus_sub_exp stream in GTExp (lhs, rhs)
    | _ -> lhs

and parse_plus_sub_exp stream =
  let lhs = parse_mul_div_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
      | Some OpPlus ->
          Stream.junk stream;
          let rhs = parse_mul_div_exp stream in
          loop (PlusExp (lhs, rhs))
      | Some OpSub ->
          Stream.junk stream;
          let rhs = parse_mul_div_exp stream in
          loop (SubExp (lhs, rhs))
      | _ -> lhs
  in
  loop lhs

and parse_mul_div_exp stream =
  let lhs = parse_primary_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
      | Some OpMul ->
          Stream.junk stream;
          let rhs = parse_primary_exp stream in
          loop (MulExp (lhs, rhs))
      | Some OpDiv ->
          Stream.junk stream;
          let rhs = parse_primary_exp stream in
          loop (DivExp (lhs, rhs))
      | _ -> lhs
  in
  loop lhs

and parse_primary_exp stream =
  match Stream.peek stream with
    | Some LeftParen ->
        Stream.junk stream;
        let exp = parse_exp stream in
        gulp stream RightParen;
        exp
    | Some (Identifier x) ->
        Stream.junk stream;
        Variable x
    | Some (Number n) ->
        Stream.junk stream;
        NumLiteral n
    | Some (Bool b) ->
        Stream.junk stream;
        BoolLiteral b
    | _ -> raise Syntax_error

and gulp stream token =
  match Stream.peek stream with
    | Some t when t = token -> Stream.junk stream
    | _ -> raise Syntax_error

(* eval *)
let rec eval prog env =
  match prog with
    | Stmts [] -> ()
    | Stmts (x::xs) ->
        eval x env;
        eval (Stmts xs) env
    | Assign (lhs, rhs) ->
        let value = eval_aexp rhs env in
        Hashtbl.replace env lhs value
    | IfElse (pred, thn, els) ->
        if (eval_bexp pred env) then
          eval thn env
        else
          eval els env
    | While (guard, body) ->
        let rec loop () =
          if (eval_bexp guard env) then
            begin
              eval body env;
              loop ()
            end
          else ()
        in
        loop ()

and eval_aexp aexp env =
  match aexp with
    | PlusExp (l, r) ->
        let l' = eval_aexp l env
        and r' = eval_aexp r env in
        l' + r'
    | SubExp (l, r) ->
        let l' = eval_aexp l env
        and r' = eval_aexp r env in
        l' - r'
    | MulExp (l, r) ->
        let l' = eval_aexp l env
        and r' = eval_aexp r env in
        l' * r'
    | DivExp (l, r) ->
        let l' = eval_aexp l env
        and r' = eval_aexp r env in
        l' / r'
    | Variable x -> Hashtbl.find env x
    | NumLiteral n -> n
    | _ -> raise Runtime_error

and eval_bexp bexp env =
  match bexp with
    | LTExp (l, r) ->
        let l' = eval_aexp l env
        and r' = eval_aexp r env in
        l' < r'
    | GTExp (l, r) ->
        let l' = eval_aexp l env
        and r' = eval_aexp r env in
        l' > r'
    | AndExp (l, r) ->
        let l' = eval_bexp l env
        and r' = eval_bexp r env in
        l' && r'
    | OrExp (l, r) ->
        let l' = eval_bexp l env
        and r' = eval_bexp r env in
        l' || r'
    | BoolLiteral b -> b
    | _ -> raise Runtime_error

let _ =
  let src = lexer_of_channel stdin in
  let prog = parse src in
  let env = Hashtbl.create 16 in
  eval prog env;
  let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) env [] in
  let pairs' = List.sort (fun (k1, _) (k2, _) -> compare k1 k2) pairs in
  List.iter (fun (k, v) -> Printf.printf "%s %d\n" k v) pairs'