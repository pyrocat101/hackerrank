type token =
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LPAREN
  | RPAREN
  | NUM of int

(*

expr = term [+-] expr
     | term

term = factor [*/] term
     | factor

factor = num
       | [+-] factor
       | '(' expr ')'

*)

let p = 1000000007

exception Invalid_char of char
exception Syntax_error

(* lexer *)
let rec lexer_of_channel channel =
  let src = Stream.of_channel channel in
  Stream.from (function _ -> lex src)

and lexer_of_string str =
  let src = Stream.of_string str in
  Stream.from (function _ -> lex src)

and lex stream =
  if Stream.peek stream = None then None
  else match Stream.next stream with
    | ' ' | '\t' | '\r' | '\n' -> lex stream
    | ('0'..'9' as ch) ->
        let buffer = Buffer.create 10 in
        Buffer.add_char buffer ch;
        Some (lex_num stream buffer)
    | '(' -> Some LPAREN
    | ')' -> Some RPAREN
    | '+' -> Some PLUS
    | '-' -> Some MINUS
    | '*' -> Some MUL
    | '/' -> Some DIV
    | ch -> raise @@ Invalid_char ch

and lex_num stream buffer =
  match Stream.peek stream with
  | Some ('0'..'9' as ch) ->
      Stream.junk stream;
      Buffer.add_char buffer ch;
      lex_num stream buffer
  | _ -> NUM (buffer |> Buffer.contents |> int_of_string)

(* parser *)
let rec parse stream = parse_expr stream

and eat stream token =
  match Stream.peek stream with
    | Some t when t = token -> Stream.junk stream
    | _ -> raise Syntax_error

and ( ** ) x y =
  let rec loop prod a b =
    match b with
    | 0 -> prod
    | _ when b land 1 <> 0 -> loop (prod * a mod p) (a * a mod p) (b / 2)
    | _ -> loop prod (a * a mod p) (b / 2)
  in
    loop 1 x y

and parse_expr stream =
  let lhs = parse_term stream in
  match Stream.peek stream with
  | Some PLUS ->
      Stream.junk stream;
      let rhs = parse_expr stream in
      (lhs + rhs) mod p
  | Some MINUS ->
      Stream.junk stream;
      let rhs = parse_expr stream in
      (lhs - rhs + p) mod p
  | _ -> lhs

and parse_term stream =
  let lhs = parse_factor stream in
  match Stream.peek stream with
  | Some MUL ->
      Stream.junk stream;
      let rhs = parse_term stream in
      (lhs * rhs) mod p
  | Some DIV ->
      Stream.junk stream;
      let rhs = parse_term stream in
      let rhs' = rhs ** (p - 2) in
      ((lhs * rhs') mod p + p) mod p
  | _ -> lhs

and parse_factor stream =
  match Stream.peek stream with
  | Some (NUM n) -> Stream.junk stream; n mod p
  | Some PLUS -> Stream.junk stream; parse_factor stream
  | Some MINUS -> Stream.junk stream; ~-(parse_factor stream)
  | Some LPAREN ->
      Stream.junk stream;
      let x = parse_expr stream in
      eat stream RPAREN;
      x
  | _ -> raise Syntax_error

let main () =
  let x = stdin |> lexer_of_channel |> parse in
  print_endline @@ string_of_int ((x + p) mod p)

let () = main ()