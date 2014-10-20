open Printf

type token =
  | IDENTIFIER of string
  | COMMA
  | LPAREN
  | RPAREN
  | LET
  | IN
  | FUN
  | EQ
  | ARROW

type exp =
  | Var of string
  | Let of string * exp * exp
  | Fun of string list * exp
  | App of exp * exp list

exception Invalid_char of char
exception Syntax_error
exception Runtime_error of string

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
  (* skip whitespace *)
  | ' ' | '\t' | '\r' | '\n' -> lex stream
  (* identifier and keyword *)
  | ('_' | 'A'..'Z' | 'a'..'z' as ch) ->
      let buffer = Buffer.create 10 in
      Buffer.add_char buffer ch;
      Some (lex_ident stream buffer)
  (* other tokens *)
  | '(' -> Some LPAREN
  | ')' -> Some RPAREN
  | '=' -> Some EQ
  | ',' -> Some COMMA
  | '-' when Stream.peek stream = Some '>' -> (Stream.junk stream; Some ARROW)
  | ch -> raise (Invalid_char ch)

and lex_ident stream buffer =
  match Stream.peek stream with
  | Some ('_' | 'A'..'Z' | 'a'..'z' | '0'..'9' as ch) ->
      Buffer.add_char buffer ch;
      Stream.junk stream;
      lex_ident stream buffer
  | _ ->
      match Buffer.contents buffer with
      | "let" -> LET
      | "in" -> IN
      | "fun" -> FUN
      | ident -> IDENTIFIER ident

(* parser *)
let rec parse stream = parse_exp stream

and eat stream token =
  match Stream.peek stream with
    | Some t when t = token -> Stream.junk stream
    | _ -> raise Syntax_error

and parse_exp stream =
  match Stream.peek stream with
  | Some LET -> parse_let stream
  | Some FUN -> parse_fun stream
  | _ -> parse_simple_exp stream

and parse_simple_exp stream =
  let caller =
    match Stream.peek stream with
    | Some LPAREN ->
        Stream.junk stream;
        let exp = parse_exp stream in
        eat stream RPAREN;
        exp
    | Some (IDENTIFIER id) -> (Stream.junk stream; Var id)
    | _ -> raise Syntax_error
  in
  let rec loop fn =
    match Stream.peek stream with
    | Some LPAREN ->
        Stream.junk stream;
        let params = parse_params stream in
        let exp = App(fn, params) in
        eat stream RPAREN;
        loop exp
    | _ -> fn
  in
    loop caller

and parse_params stream =
  match Stream.peek stream with
  | Some (LET | FUN | LPAREN | IDENTIFIER _) ->
      let param = parse_exp stream in
      if Stream.peek stream = Some COMMA
      then (Stream.junk stream; param :: parse_params stream)
      else [param]
  | _ -> []

and parse_let stream =
  Stream.junk stream;
  let name = parse_ident stream in
  eat stream EQ;
  let value = parse_exp stream in
  eat stream IN;
  let body = parse_exp stream in
  Let(name, value, body)

and parse_ident stream =
  match Stream.peek stream with
  | Some (IDENTIFIER id) -> (Stream.junk stream; id)
  | _ -> raise Syntax_error

and parse_fun stream =
  Stream.junk stream;
  let args = parse_args stream in
  eat stream ARROW;
  let body = parse_exp stream in
  Fun(args, body)

and parse_args stream =
  match Stream.peek stream with
  | Some (IDENTIFIER id) -> (Stream.junk stream; id :: parse_args stream)
  | _ -> []

(* printer for debug: explicit parenthesis reveals precedence *)
let rec string_of_exp = function
  | Var v -> v
  | Let(name, value, body) ->
      sprintf "(let %s = %s in %s)" name (string_of_exp value) (string_of_exp body)
  | Fun(args, body) ->
      sprintf "(fun %s -> %s)" (String.concat " " args) (string_of_exp body)
  | App(fn, params) ->
      sprintf "%s(%s)" (string_of_exp fn) (String.concat ", " (List.map string_of_exp params))

(* destructive-unification based implementation of algorithm W *)

type ty =
  | TConst of string
  | TApp of ty * ty list
  | TFun of ty list * ty
  | TVar of tvar ref

and tvar =
  | Poly of int
  | Bound of ty
  | Unbound of int * level

(* level: nested level of let-expression used by generalization *)
and level = int

module Env = Map.Make(String)
type env = ty Env.t

let id_counter = ref (-1)
let gen_id = fun () -> incr id_counter; !id_counter
let reset_id = fun () -> id_counter := (-1)

let fresh_var level =
  TVar(ref @@ Unbound(gen_id (), level))

let fresh_poly_var () =
  TVar(ref @@ Poly(gen_id ()))

(* printer for type *)
let rec string_of_ty (t: ty) : string =
  (* keep track of poly variables' id -> name *)
  let id_name_map = Hashtbl.create 26 in
  (* assume we only use a to z *)
  let gensym =
    let counter = ref (-1) in
    fun () -> incr counter; char_of_int (97 + !counter) |> String.make 1
  in
  let rec walk = function
    | TConst k -> k
    | TApp(t1, args) ->
        let t1 = walk t1 in
        let args = String.concat ", " (List.map walk args) in
        sprintf "%s[%s]" t1 args
    | TFun([(TFun _) as p], t1) ->
        let lhs = walk p in
        let rhs = walk t1 in
        sprintf "(%s) -> %s" lhs rhs
    | TFun([param], t1) ->
        let lhs = walk param in
        let rhs = walk t1 in
        sprintf "%s -> %s" lhs rhs
    | TFun(params, t1) ->
        let lhs = String.concat ", " @@ List.map walk params in
        let rhs = walk t1 in
        sprintf "(%s) -> %s" lhs rhs
    | TVar {contents = Poly id} ->
        begin try
          Hashtbl.find id_name_map id
        with Not_found ->
          let name = gensym () in
          Hashtbl.add id_name_map id name;
          name
        end
    | TVar {contents = Unbound(id, _)} -> "_" ^ string_of_int id
    | TVar {contents = Bound t} -> walk t
  in
    let s = walk t in
    if Hashtbl.length id_name_map > 0 then
      let vars = Hashtbl.fold (fun _ v l -> v :: l) id_name_map [] in
      let vars = Array.of_list vars in
      Array.sort compare vars;
      let vars = Array.to_list vars in
      sprintf "forall[%s] %s" (String.concat " " vars) s
    else
      s

let print_ty ff t =
  Format.fprintf ff "%s" (string_of_ty t)

let print_exp ff exp =
  Format.fprintf ff "%s" (string_of_exp exp)

let print_env ff env =
  Format.fprintf ff "{ ";
  Env.iter (fun k v ->
    try
      ignore @@ List.find ((=) k) [
        "head";
        "tail";
        "nil";
        "cons";
        "cons_curry";
        "map";
        "map_curry";
        "one";
        "zero";
        "succ";
        "plus";
        "eq";
        "eq_curry";
        "not";
        "true";
        "false";
        "pair";
        "pair_curry";
        "first";
        "second";
        "id";
        "const";
        "apply";
        "apply_curry";
        "choose";
        "choose_curry";
      ]
    with Not_found -> Format.fprintf ff "%s -> %s; " k (string_of_ty v)
  ) env;
  Format.fprintf ff "}"

(* generalize unbound type variable *)
let rec generalize (level: level) (t: ty) : ty =
  match t with
  (* only generalize unbound variables in let-binding expression *)
  | TVar {contents = Unbound(id, lv)} when lv > level -> TVar(ref @@ Poly id)
  | TVar {contents = Bound t'} -> generalize level t'
  | TApp(t1, args) -> TApp(generalize level t1, List.map (generalize level) args)
  | TFun(args, t1) -> TFun(List.map (generalize level) args, generalize level t1)
  | _ -> t

(* replace polymorphic type variable with unbound type variable *)
and instantiate (level: level) (t: ty) : ty =
  (* same poly var should be replaced into same unbound var. *)
  let id_var_map = Hashtbl.create 16 in
  let rec walk t = match t with
    | TVar {contents = Poly id} ->
        begin try Hashtbl.find id_var_map id
        with Not_found ->
          let var = fresh_var level in
          Hashtbl.add id_var_map id var;
          var
        end
    | TVar {contents = Bound t} -> walk t
    | TApp(t1, args) -> TApp(walk t1, List.map walk args)
    | TFun(params, t1) -> TFun(List.map walk params, walk t1)
    | _ -> t
  in
    walk t

(* destructive unification *)
let rec unify (t1: ty) (t2: ty) : unit =
  match t1, t2 with
  | _ when t1 = t2 -> ()
  (* recursive unification *)
  | TApp(x, args), TApp(x', args') -> (unify x x'; List.iter2 unify args args')
  | TFun(params, t), TFun(params', t') -> (List.iter2 unify params params'; unify t t')
  (* either is bounded, unify with bounded value instead *)
  | TVar {contents = Bound t1}, t2
  | t1, TVar {contents = Bound t2} -> unify t1 t2
  (* either one is unbounded, occurs check and update binding *)
  | TVar ({contents = Unbound(id, level)} as v), t
  | t, TVar ({contents = Unbound(id, level)} as v) -> (occurs_check id level t; v := Bound t)
  (* all other cases fail *)
  | _ -> raise @@ Runtime_error(sprintf "cannot unify %s and %s" (string_of_ty t1) (string_of_ty t2))

(* occurence check, raise exception when failed *)
and occurs_check (id: int) (level: level) (t: ty) : unit =
  let rec check = function
    | TVar {contents = Bound t} -> check t
    | TVar {contents = Unbound(id', _)} when id' = id ->
        raise @@ Runtime_error("recursive type")
    (* unify two unbounds: lift the level of the other one *)
    | TVar ({contents = Unbound(id', level')} as v) when level' > level ->
        v := Unbound(id', level)
    | TApp(t1, args) -> check t1; List.iter check args
    | TFun(args, t1) -> List.iter check args; check t1
    | _ -> ()
  in
    check t

(* W *)
let rec w (env: env) (level: level) (exp: exp) : ty =
  match exp with
  (* var *)
  | Var v ->
      begin try
        instantiate level (Env.find v env)
      with Not_found ->
        raise @@ Runtime_error("unbound type variable " ^ v)
      end
  (* abs *)
  | Fun(params, body) ->
      let t_params = List.map (fun _ -> fresh_var level) params in
      let fun_env = List.fold_left2
        (fun env param t_param -> Env.add param t_param env)
        env params t_params
      in
      let t_ret = w fun_env level body in
      TFun(t_params, t_ret)
  (* app *)
  | App(fn, args) ->
      let t_fn = w env level fn in
      let t_args = List.map (w env level) args in
      let arity = List.length args in
      let t_params, t_return = match_fun_type arity t_fn in
      List.iter2 unify t_params t_args;
      t_return
  (* let *)
  | Let(name, value, body) ->
      (* create a deeper-level scope *)
      let t_value = w env (level + 1) value in
      let t_value_poly = generalize level t_value in
      w (Env.add name t_value_poly env) level body

and match_fun_type arity = function
  | TFun(t_params, t_return) ->
      if (List.length t_params <> arity)
      then raise @@ Runtime_error("function arity mismatch")
      else (t_params, t_return)
  | TVar {contents = Bound t} -> match_fun_type arity t
  | TVar ({contents = Unbound(id, level)} as v) ->
      let rec loop = function
        | 0 -> []
        | n -> (fresh_var level) :: loop (n - 1)
      in
      let t_params = loop arity in
      let t_return = fresh_var level in
      v := Bound (TFun(t_params, t_return));
      (t_params, t_return)
  | _ -> raise @@ Runtime_error("application with non-function")


module TypeReader = struct
  (* lexer for type *)
  type token =
    | IDENTIFIER of string
    | LPAREN
    | RPAREN
    | LBRACK
    | RBRACK
    | FORALL
    | ARROW
    | COMMA

  (* lexer for type *)
  let rec lexer_of_channel channel =
    let src = Stream.of_channel channel in
    Stream.from (function _ -> lex src)

  and lexer_of_string str =
    let src = Stream.of_string str in
    Stream.from (function _ -> lex src)

  and lex stream =
    if Stream.peek stream = None then None
    else match Stream.next stream with
    (* skip whitespace *)
    | ' ' | '\t' | '\r' | '\n' -> lex stream
    (* identifier and keyword *)
    | ('_' | 'A'..'Z' | 'a'..'z' as ch) ->
        let buffer = Buffer.create 10 in
        Buffer.add_char buffer ch;
        Some (lex_ident stream buffer)
    (* other tokens *)
    | ',' -> Some COMMA
    | '(' -> Some LPAREN
    | ')' -> Some RPAREN
    | '[' -> Some LBRACK
    | ']' -> Some RBRACK
    | '-' when Stream.peek stream = Some '>' -> (Stream.junk stream; Some ARROW)
    | ch -> raise (Invalid_char ch)

  and lex_ident stream buffer =
    match Stream.peek stream with
    | Some ('_' | 'A'..'Z' | 'a'..'z' | '0'..'9' as ch) ->
        Buffer.add_char buffer ch;
        Stream.junk stream;
        lex_ident stream buffer
    | _ ->
        match Buffer.contents buffer with
        | "forall" -> FORALL
        | ident -> IDENTIFIER ident

  (* parser for type *)
  module Vars = Map.Make(String)
  type vars = ty Vars.t

  let rec parse stream = parse_ty stream Vars.empty

  and eat stream token =
    match Stream.peek stream with
      | Some t when t = token -> Stream.junk stream
      | _ -> raise Syntax_error

  (* WTF! this is not LL(1) at all! *)
  and parse_ty (stream: token Stream.t) (vars: vars) : ty =
    match Stream.peek stream with
    | Some FORALL ->
        Stream.junk stream;
        eat stream LBRACK;
        let args = parse_args stream in
        (* inner poly vars shall shadow outer ones *)
        let vars' = List.fold_left (
          fun vars a -> Vars.add a (fresh_poly_var ()) vars
        ) vars args
        in
        eat stream RBRACK;
        parse_ty stream vars'
    | Some (IDENTIFIER id) ->
        Stream.junk stream;
        let var = parse_ident id vars in
        let t = parse_type_args var stream vars in
        if Stream.peek stream = Some ARROW
        then parse_fun [t] stream vars
        else t
    | Some LPAREN ->
        Stream.junk stream;
        begin match parse_ty_list stream vars with
          | [param] ->
              Stream.junk stream;
              let param = parse_type_args param stream vars in
              if Stream.peek stream = Some ARROW
              then parse_fun [param] stream vars
              else param
          | params ->
              Stream.junk stream;
              parse_fun params stream vars
        end
    | _ -> raise Syntax_error

  and parse_type_args (t: ty) (stream: token Stream.t) (vars: vars) : ty =
    match Stream.peek stream with
    | Some LBRACK ->
        Stream.junk stream;
        let args = parse_ty_list stream vars in
        eat stream RBRACK;
        parse_type_args (TApp(t, args)) stream vars
    | _ -> t

  and parse_fun (t1: ty list) (stream: token Stream.t) (vars: vars) : ty =
    match Stream.peek stream with
    | Some ARROW ->
        Stream.junk stream;
        let t2 = parse_ty stream vars in
        TFun(t1, t2)
    | _ -> raise Syntax_error

  and parse_ty_list (stream: token Stream.t) (vars: vars) : ty list =
    match Stream.peek stream with
    | Some (FORALL | IDENTIFIER _ | LPAREN) ->
        let rec loop stream =
          match Stream.peek stream with
          | Some COMMA ->
              Stream.junk stream;
              let hd = parse_ty stream vars in
              let tl = parse_ty_list stream vars in
              hd :: tl
          | _ -> []
        in
          let hd = parse_ty stream vars in
          let tl = loop stream in
          hd :: tl
    | _ -> []

  and parse_args stream =
    match Stream.peek stream with
    | Some (IDENTIFIER id) -> (Stream.junk stream; id :: parse_args stream)
    | _ -> []

  and parse_ident id vars =
    try Vars.find id vars
    with Not_found -> TConst(id)
end

(* infer *)

let make_env () =
  let assume name ty_str env =
    let open TypeReader in
    let t = parse @@ lexer_of_string ty_str in
    Env.add name t env
  in
    Env.empty
    |> assume "head" "forall[a] list[a] -> a"
    |> assume "tail" "forall[a] list[a] -> list[a]"
    |> assume "nil" "forall[a] list[a]"
    |> assume "cons" "forall[a] (a, list[a]) -> list[a]"
    |> assume "cons_curry" "forall[a] a -> list[a] -> list[a]"
    |> assume "map" "forall[a b] (a -> b, list[a]) -> list[b]"
    |> assume "map_curry" "forall[a b] (a -> b) -> list[a] -> list[b]"
    |> assume "one" "int"
    |> assume "zero" "int"
    |> assume "succ" "int -> int"
    |> assume "plus" "(int, int) -> int"
    |> assume "eq" "forall[a] (a, a) -> bool"
    |> assume "eq_curry" "forall[a] a -> a -> bool"
    |> assume "not" "bool -> bool"
    |> assume "true" "bool"
    |> assume "false" "bool"
    |> assume "pair" "forall[a b] (a, b) -> pair[a, b]"
    |> assume "pair_curry" "forall[a b] a -> b -> pair[a, b]"
    |> assume "first" "forall[a b] pair[a, b] -> a"
    |> assume "second" "forall[a b] pair[a, b] -> b"
    |> assume "id" "forall[a] a -> a"
    |> assume "const" "forall[a b] a -> b -> a"
    |> assume "apply" "forall[a b] (a -> b, a) -> b"
    |> assume "apply_curry" "forall[a b] (a -> b) -> a -> b"
    |> assume "choose" "forall[a] (a, a) -> a"
    |> assume "choose_curry" "forall[a] a -> a -> a"

let infer ty_str =
  let env = make_env () in
  reset_id ();
  lexer_of_string ty_str |> parse |> w env 0 |> generalize (-1) |> string_of_ty

(* tests *)

let test_cases = [
  (* Hindley-Milner *)
  ("id", "forall[a] a -> a");
  ("one", "int");
  ("let x = id in x", "forall[a] a -> a");
  ("let x = fun y -> y in x", "forall[a] a -> a");
  ("fun x -> x", "forall[a] a -> a");
  ("pair", "forall[a b] (a, b) -> pair[a, b]") ;
  ("fun x -> let y = fun z -> z in y", "forall[a b] a -> b -> b");
  ("let f = fun x -> x in let id = fun y -> y in eq(f, id)", "bool");
  ("let f = fun x -> x in let id = fun y -> y in eq_curry(f)(id)", "bool");
  ("let f = fun x -> x in eq(f, succ)", "bool");
  ("let f = fun x -> x in eq_curry(f)(succ)", "bool");
  ("let f = fun x -> x in pair(f(one), f(true))", "pair[int, bool]");
  ("let f = fun x y -> let a = eq(x, y) in eq(x, y) in f", "forall[a] (a, a) -> bool");
  ("let f = fun x y -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f", "forall[a] (a, a) -> bool");
  ("id(id)", "forall[a] a -> a");
  ("choose(fun x y -> x, fun x y -> y)", "forall[a] (a, a) -> a");
  ("choose_curry(fun x y -> x)(fun x y -> y)", "forall[a] (a, a) -> a");
  ("let x = id in let y = let z = x(id) in z in y", "forall[a] a -> a");
  ("cons(id, nil)", "forall[a] list[a -> a]");
  ("cons_curry(id)(nil)", "forall[a] list[a -> a]");
  ("let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2", "list[int -> int]");
  ("cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))", "list[int -> int]");
  ("fun x -> let y = x in y", "forall[a] a -> a");
  ("fun x -> let y = let z = x(fun x -> x) in z in y", "forall[a b] ((a -> a) -> b) -> b");
  ("fun x -> fun y -> let x = x(y) in x(y)", "forall[a b] (a -> a -> b) -> a -> b");
  ("fun x -> let y = fun z -> x(z) in y", "forall[a b] (a -> b) -> a -> b");
  ("fun x -> let y = fun z -> x in y", "forall[a b] a -> b -> a");
  ("fun x -> fun y -> let x = x(y) in fun x -> y(x)", "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b");
  ("fun x -> let y = fun z -> z in y(y)", "forall[a b] a -> b -> b");
  ("fun f -> let x = fun g y -> let _ = g(y) in eq(f, g) in x", "forall[a b] (a -> b) -> (a -> b, a) -> bool");
  ("let const = fun x -> fun y -> x in const", "forall[a b] a -> b -> a");
  ("let apply = fun f x -> f(x) in apply", "forall[a b] (a -> b, a) -> b");
  ("let apply_curry = fun f -> fun x -> f(x) in apply_curry", "forall[a b] (a -> b) -> a -> b");
]

let run_tests () =
  List.iter (fun (ty_str, expected) ->
    let res = infer ty_str in
    if res <> expected
    then printf "\x1b[1;31mFailed:\x1b[0m \x1b[1m%s\x1b[0m, expects \x1b[1;34m%s\x1b[0m, but got \x1b[1;31m%s\x1b[0m\n" ty_str expected res
    else printf "\x1b[1;32mPassed:\x1b[0m %s ==> %s\n" ty_str res
  ) test_cases

(* main *)
let () =
  let exp = stdin |> lexer_of_channel |> parse in
  let env = make_env () in
  reset_id ();
  exp |> w env 0 |> generalize (-1) |> string_of_ty |> print_endline