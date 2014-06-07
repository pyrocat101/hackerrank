(*
  EXP = VAR
  EXP = '(' EXP ' ' EXP ')'
  EXP = '(' '\' VARLIST '.' EXP ')'
  VARLIST = VAR VARLIST0
  VARLIST0 = eps
  VARLIST0 = ' ' VAR VARLIST0
*)

type token = LParen
           | RParen
           | Backslash
           | Dot
           | Symbol of string

type ast = Lambda of string * ast
         | App of ast * ast
         | Var of string
         | S | K | I | C | B

exception Invalid_char of char
exception Syntax_error
exception Transform_error

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
    (* symbols *)
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_' as ch) ->
        let buffer = Buffer.create 8 in
        Buffer.add_char buffer ch;
        Some (lex_var stream buffer)
    (* backslash *)
    | '\\' -> Some Backslash
    (* dot *)
    | '.' -> Some Dot
    (* left parenthesis *)
    | '(' -> Some LParen
    | ')' -> Some RParen
    | _ as ch -> raise (Invalid_char ch)

and lex_var stream buffer =
  match Stream.peek stream with
    | Some ('A'..'Z' | 'a'..'z' | '0'..'9' | '_' as ch) ->
        Buffer.add_char buffer ch;
        Stream.junk stream;
        lex_var stream buffer
    | _ -> Symbol (Buffer.contents buffer)

(* parser *)
let rec parse stream = parse_exp stream

and parse_exp stream =
  match Stream.peek stream with
    | Some (Symbol s) ->
        Stream.junk stream;
        Var s
    | Some LParen ->
        Stream.junk stream;
        let exp = (match Stream.peek stream with
          | Some Backslash -> parse_lambda stream
          | Some LParen | Some (Symbol _) -> parse_app stream
          | _ -> raise Syntax_error)
        in
        gulp stream RParen;
        exp
    | _ -> raise Syntax_error

and parse_app stream =
  let x = parse_exp stream in
  let y = parse_exp stream in
  let rec loop x =
    match Stream.peek stream with
      | Some (Symbol _ | LParen) ->
          let y = parse_exp stream in
          loop (App (x, y))
      | _ -> x
  in
  loop (App (x, y))

and parse_lambda stream =
  Stream.junk stream;
  let rec loop () =
    match Stream.peek stream with
      | Some Dot ->
          Stream.junk stream;
          parse_exp stream
      | Some (Symbol s) ->
          Stream.junk stream;
          Lambda (s, loop ())
      | _ -> raise Syntax_error
  in
  match Stream.peek stream with
    | Some (Symbol s) ->
        Stream.junk stream;
        Lambda (s, loop ())
    | _ -> raise Syntax_error

and gulp stream token =
  match Stream.peek stream with
    | Some t when t = token -> Stream.junk stream
    | _ -> raise Syntax_error

(* transformation *)
let rec transform exp =
  let exp = eta_reduce exp in
  match exp with
    | Var x -> Var x
    | App (e1, e2) -> App (transform e1, transform e2)
    | Lambda (x, e) when not (free x e) -> App (K, transform e)
    | Lambda (x, Var y) when x = y -> I
    | Lambda (x, Lambda (y, e)) when (free x e) ->
        transform (Lambda (x, transform (Lambda (y, e))))
    | Lambda (x, App (e1, e2)) when ((free x e1) && (free x e2)) ->
        App (App (S, transform (Lambda (x, e1))), transform (Lambda (x, e2)))
    | Lambda (x, App (e1, e2)) when (free x e1 && not (free x e2)) ->
        App (App (C, transform (Lambda (x, e1))), transform e2)
    | Lambda (x, App (e1, e2)) when (free x e2 && not (free x e1)) ->
        App (App (B, transform e1), transform (Lambda (x, e2)))
    | S | K | I | C | B -> exp
    | _ -> raise Transform_error

and eta_reduce exp =
  let rec loop exp =
    match exp with
      | Lambda (x, App (e, Var y)) when x = y && not (free x e) -> loop e
      | _ -> exp
  in
  loop exp

and free x exp =
  match exp with
    | (Var v) when x = v -> true
    | Lambda (a, b) when x <> a -> free x b
    | App (a, b) -> (free x a) || (free x b)
    | _ -> false

(* print combinators *)
let string_of_combinators exp =
  let buffer = Buffer.create 10 in
  let rec to_string exp =
    match exp with
      | S -> Buffer.add_char buffer 'S'
      | K -> Buffer.add_char buffer 'K'
      | I -> Buffer.add_char buffer 'I'
      | C -> Buffer.add_char buffer 'C'
      | B -> Buffer.add_char buffer 'B'
      | App (e1, e2) ->
          to_string e1;
          (match e2 with
            | App _ ->
                Buffer.add_char buffer '(';
                to_string e2;
                Buffer.add_char buffer ')'
            | _ -> to_string e2)
      | _ -> raise Transform_error
  in
  to_string exp;
  Buffer.contents buffer

let convert s =
  let tokens = lexer_of_string s in
  let tree = parse tokens in
  let tree' = transform tree in
  string_of_combinators tree'

let main () =
  let n = read_int () in
  let rec loop i =
    if i > 0 then
      let s = read_line () in
      let s' = convert s in
      print_endline s';
      loop (i - 1)
    else
      ()
  in
  loop n

let () = main ()