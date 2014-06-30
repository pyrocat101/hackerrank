(*
   The language is case-INSENSITIVE!

   letter ::= [a-zA-Z]
   ident ::= <letter> ( <digit> | <letter> )*

   kwd  ::= function | is | of | assign | and | to | do | what
   int  ::= <digit>+
   num  ::= <int> [ / <int> ]
   var  ::= <ident>
   func ::= function of <int> : <exp> (, <exp>)*
          | <exp>
   decl ::= <var> is <func> .

   assn ::= Assign <exp> to <var> ( AND <exp> to <var> )* !

   loop ::= do { <exp> } <assn>

   ask ::= what is ( <call> ) ( AND <call> )* ?

   exp   ::= <term> ( ( + | - ) <exp> )?
   term  ::= <value> ( ( * | / ) <term> )?
   value ::= [+ | -] <num> | <call> | \( exp \)
   call ::= <var> ( \[ <exp> \] )*

   program ::= ( decl | assn | loop | ask )*
 *)

(* rational number *)
type num = Ratio of int * int

let rec num_of_string s =
  if String.contains s '/' then
    let len = String.length s in
    let delim = String.index s '/' in
    let numer = String.sub s 0 delim
    and denom = String.sub s (delim + 1) (len - delim - 1) in
    Ratio (int_of_string numer, int_of_string denom) |> simplify
  else
    Ratio ((int_of_string s), 1) |> simplify

and sign x =
  if x < 0 then
    -1
  else if x = 0 then
    0
  else
    1

and string_of_num (Ratio (numer, denom)) =
  if denom = 1 then
    string_of_int numer
  else
    Format.sprintf "%s/%s" (string_of_int numer) (string_of_int denom)

and simplify (Ratio (numer, denom)) =
  if numer = 0 || denom = 0 then
    Ratio (0, 1)
  else
    let sign = (sign numer) * (sign denom) in
    let numer = abs numer in
    let denom = abs denom in
    let divisor = gcd numer denom in
    Ratio (sign * numer / divisor, denom / divisor)

and gcd a b =
  if      a = 0 then b
  else if b = 0 then a
  else if a > b then gcd b (a mod b)
  else               gcd a (b mod a)

(*
  a   c   ad + bc
  - + - = -------
  b   d     b*d
*)
let ( +/ ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * d + b * c, b * d) |> simplify

(*
  a   c   ad - bc
  - - - = -------
  b   d     b*d
*)
let ( -/ ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * d - b * c, b * d) |> simplify

(*
  a   c   ac
  - * - = --
  b   d   bd
*)
let ( */ ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * c, b * d) |> simplify

(*
  a   c   ad
  - / - = --
  b   d   bc
*)
let ( // ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * d, b * c) |> simplify

let minus_num (Ratio (a, b)) = Ratio (-a, b)

let is_integer_num (Ratio (a, b)) = b = 1

let sign_num (Ratio (a, b)) = sign a

let int_of_num (Ratio(a, b)) = a / b

(* interpreter *)
type token = Number of num
           | Identifier of string
           | KwdFunction
           | KwdIs
           | KwdOf
           | KwdAssign
           | KwdAnd
           | KwdTo
           | KwdDo
           | KwdWhat
           | LBrace
           | RBrace
           | LBracket
           | RBracket
           | LParen
           | RParen
           | Colon (* : *)
           | Comma (* , *)
           | Question (* ? *)
           | Bang (* ! *)
           | Period (* . *)
           | OpPlus
           | OpSub
           | OpMul
           | OpDiv

type exp = PlusExp of exp * exp
         | SubExp of exp * exp
         | MulExp of exp * exp
         | DivExp of exp * exp
         | PosExp of exp
         | NegExp of exp
         | NumLiteral of num
         | Call of call

and func = exp array
and call = string * exp list
and assign = (string * exp) list

and stmt = Decl of string * func
         | Assign of assign
         | Loop of exp * assign
         | Ask of call list

type program = stmt list

type value = NumVal of num
           | FuncVal of num * num list

exception Invalid_char of char
exception Syntax_error
exception Runtime_error

let (|>) x f = f x

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
      | '0'..'9' as ch ->
          let buffer = Buffer.create 10 in
          Buffer.add_char buffer ch;
          Some (lex_number stream buffer)
      (* other tokens *)
      | '{' -> Some LBrace
      | '}' -> Some RBrace
      | '[' -> Some LBracket
      | ']' -> Some RBracket
      | '(' -> Some LParen
      | ')' -> Some RParen
      | ':' -> Some Colon
      | ',' -> Some Comma
      | '?' -> Some Question
      | '!' -> Some Bang
      | '.' -> Some Period
      | '+' -> Some OpPlus
      | '-' -> Some OpSub
      | '*' -> Some OpMul
      | '/' -> Some OpDiv
      | _ as ch -> raise (Invalid_char ch)

and lex_number stream buffer =
  match Stream.peek stream with
    | Some ('0'..'9' as ch) ->
        Buffer.add_char buffer ch;
        Stream.junk stream;
        lex_number stream buffer
    | _ -> Number (buffer |> Buffer.contents |> num_of_string)

and lex_ident stream buffer =
  match Stream.peek stream with
    | Some ('A'..'Z' | 'a'..'z' | '0'..'9' as ch) ->
        Buffer.add_char buffer ch;
        Stream.junk stream;
        lex_ident stream buffer
    | _ ->
        let ident = buffer |> Buffer.contents |> String.lowercase in
        begin
          match ident with
            | "function" -> KwdFunction
            | "is"       -> KwdIs
            | "of"       -> KwdOf
            | "assign"   -> KwdAssign
            | "and"      -> KwdAnd
            | "to"       -> KwdTo
            | "do"       -> KwdDo
            | "what"     -> KwdWhat
            | _          -> Identifier ident
        end

(* parser *)
let rec parse stream = parse_stmts stream

and eat stream token =
  match Stream.peek stream with
    | Some t when t = token -> Stream.junk stream
    | _ -> raise Syntax_error

and parse_stmts stream =
  let rec loop () =
    if Stream.peek stream = None then []
    else
      let stmt = match Stream.peek stream with
        | Some (Identifier _) -> parse_decl stream
        | Some KwdAssign -> parse_assign stream
        | Some KwdDo -> parse_loop stream
        | Some KwdWhat -> parse_ask stream
        | _ -> raise Syntax_error
      in
    stmt :: loop ()
  in
  loop ()

and parse_decl stream =
  let name = parse_var stream in
  eat stream KwdIs;
  let func = parse_func stream in
  eat stream Period; (* . *)
  Decl (name, func)

and parse_func stream =
  match Stream.peek stream with
    | Some KwdFunction ->
        Stream.junk stream; (* function *)
        eat stream KwdOf;
        let n = parse_argc stream in
        eat stream Colon; (* : *)
        let args = stream |> parse_argv |> Array.of_list in
          if n + 1 <> Array.length args
          then raise Syntax_error
          else args
    | _ -> [|parse_exp stream|]

and parse_argc stream =
  match Stream.peek stream with
    | Some (Number n) when is_integer_num n && sign_num n >= 0 ->
        Stream.junk stream;
        int_of_num n
    | _ -> raise Syntax_error

and parse_argv stream =
  let rec loop () =
    match Stream.peek stream with
      | Some Comma ->
          Stream.junk stream; (* , *)
          let arg = parse_exp stream in
          arg :: loop ()
      | _ -> []
  in
  let first = parse_exp stream in
  first :: loop ()

and parse_assign stream =
  Assign (parse_assign_body stream)

and parse_assign_body stream =
  let rec loop () =
    match Stream.peek stream with
      | Some KwdAnd ->
          Stream.junk stream; (* and *)
          let rhs = parse_exp stream in
          eat stream KwdTo;
          let lhs = parse_var stream in
          (lhs, rhs) :: loop ()
      | _ -> []
  in
  Stream.junk stream; (* assign *)
  let rhs = parse_exp stream in
  eat stream KwdTo;
  let lhs = parse_var stream in
  let body = (lhs, rhs) :: loop () in
  eat stream Bang;
  body

and parse_var stream =
  match Stream.peek stream with
    | Some (Identifier id) -> Stream.junk stream; id
    | _ -> raise Syntax_error

and parse_loop stream =
  Stream.junk stream; (* do *)
  eat stream LBrace; (* { *)
  let times = parse_exp stream in
  eat stream RBrace; (* } *)
  let body = parse_assign_body stream in
  Loop (times, body)

and parse_ask stream =
  let rec loop () =
    match Stream.peek stream with
      | Some KwdAnd ->
          Stream.junk stream; (* and *)
          let query = parse_call stream in
          query :: loop ()
      | _ -> []
  in
  (* Stream.junk stream; (* what *) *)
  eat stream KwdWhat;
  eat stream KwdIs; (* is *)
  let first = parse_call stream in
  let ask = Ask (first :: loop ()) in
  eat stream Question; (* ? *)
  ask

and parse_exp stream =
  let lhs = parse_term stream in
  let rec loop lhs =
    match Stream.peek stream with
      | Some OpPlus ->
          Stream.junk stream;
          let rhs = parse_term stream in
          loop (PlusExp (lhs, rhs))
      | Some OpSub ->
          Stream.junk stream;
          let rhs = parse_term stream in
          loop (SubExp (lhs, rhs))
      | _ -> lhs
  in
  loop lhs

and parse_term stream =
  let lhs = parse_value stream in
  let rec loop lhs =
    match Stream.peek stream with
      | Some OpMul ->
          Stream.junk stream;
          let rhs = parse_value stream in
          loop (MulExp (lhs, rhs))
      | Some OpDiv ->
          Stream.junk stream;
          let rhs = parse_value stream in
          loop (DivExp (lhs, rhs))
      | _ -> lhs
  in
  loop lhs

and parse_value stream =
  match Stream.peek stream with
    | Some (Identifier id) -> Call (parse_call stream)
    | Some (Number _) -> parse_num_literal stream
    | Some OpPlus ->
        Stream.junk stream; (* + *)
        PosExp (parse_num_literal stream)
    | Some OpSub ->
        Stream.junk stream; (* - *)
        NegExp (parse_num_literal stream)
    | Some LParen ->
        Stream.junk stream; (* ( *)
        let value = parse_exp stream in
        eat stream RParen; (* ) *)
        value
    | _ -> raise Syntax_error

and parse_call stream =
  let rec loop () =
    match Stream.peek stream with
      | Some LBracket ->
          Stream.junk stream; (* [ *)
          let arg = parse_exp stream in
          Stream.junk stream; (* ] *)
          arg :: loop ()
      | _ -> []
  in
  let name = parse_var stream in
  let args = loop () in
  (name, args)

and parse_num_literal stream =
  match Stream.peek stream with
    | Some (Number n) ->
        Stream.junk stream;
        NumLiteral n
    | _ -> raise Syntax_error

(* eval *)
let rec evlis env l =
  List.iter (function
    | Decl (name, func) -> eval_decl env name func
    | Assign pairs -> eval_assign env pairs
    | Loop (times, body) -> eval_loop env times body
    | Ask queries -> eval_ask env queries
  ) l

and eval_decl env name func =
  let value = eval_func env func in
  Hashtbl.replace env name value

and eval_assign env pairs =
  List.iter (function (name, exp) ->
    let value = eval_exp env exp in
    Hashtbl.replace env name value
  ) pairs

and eval_loop env times body =
  match eval_exp env times with
    | NumVal n when is_integer_num n ->
        let times' = int_of_num n in
        for i = 1 to times' do
          eval_assign env body
        done
    | _ -> raise Runtime_error

and eval_ask env queries =
  List.iter (function query ->
    let value = eval_call env query in
    value |> string_of_value |> print_endline
  ) queries

and string_of_value v =
  match v with
    | NumVal n -> string_of_num n
    | FuncVal (k0, ki) ->
        (ki @ [k0]) |> List.map string_of_num |> String.concat ", "

and eval_func env f =
  match f with
    | [|k0|] -> eval_exp env k0
    | _ ->
        let f' = Array.map (eval_num env) f in
        let k0 = f'.(Array.length f' - 1) in
        let ki = Array.sub f' 0 (Array.length f' - 1) |> Array.to_list in
        FuncVal (k0, ki)

and eval_num env exp =
  match eval_exp env exp with
    | NumVal n -> n
    | _ -> raise Runtime_error

and binary_op f l r =
  match (l, r) with
    | (NumVal l, NumVal r) -> NumVal (f l r)
    | _ -> raise Runtime_error

and unary_op f e =
  match e with
    | NumVal e -> NumVal (f e)
    | _ -> raise Runtime_error

and ( +++ ) l r = binary_op ( +/ ) l r
and ( --- ) l r = binary_op ( -/ ) l r
and ( *** ) l r = binary_op ( */ ) l r
and ( /// ) l r = binary_op ( // ) l r

and eval_exp env exp =
  match exp with
    | PlusExp (l, r) -> (eval_exp env l) +++ (eval_exp env r)
    | SubExp  (l, r) -> (eval_exp env l) --- (eval_exp env r)
    | MulExp  (l, r) -> (eval_exp env l) *** (eval_exp env r)
    | DivExp  (l, r) -> (eval_exp env l) /// (eval_exp env r)
    | PosExp e -> unary_op (function x -> x) (eval_exp env e)
    | NegExp e -> unary_op minus_num (eval_exp env e)
    | NumLiteral n -> NumVal n
    | Call c -> eval_call env c

and eval_call env (name, args) =
  let value = Hashtbl.find env name in
  match value with
    | NumVal n when args = [] -> value
    | NumVal _ -> raise Runtime_error
    | FuncVal (k0, ki) ->
        let args' = List.map (eval_num env) args in
        let f' = List.fold_left apply (k0, ki) args' in
        match f' with
          | (k0, []) -> NumVal k0
          | (k0, ki) -> FuncVal (k0, ki)

and apply (k0, ki) x =
  match ki with
    | k :: rest -> (k0 +/ x */ k, rest)
    | _ -> raise Runtime_error

and num_of_value v =
  match v with
    | NumVal n -> n
    | _ -> raise Runtime_error

and make_env () = Hashtbl.create 10

(* lex -> parse -> eval *)
let rec run tokens =
  let ast = parse tokens in
  let env = make_env () in
  evlis env ast

and run_of_channel channel =
  channel |> lexer_of_channel |> run

and run_of_string str =
  str |> lexer_of_string |> run

let main () = run_of_channel stdin

let () = main ()