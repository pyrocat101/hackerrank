(*
 * <variable> ::= "#" <name>
 * <relational-term> ::= "[" <name> ": " <simple-term> <simple-terms> "]"
 * <simple-term> ::= <name> | <variable> | <relational-term>
 * <simple-terms> ::= "" | ", " <simple-term> <simple-terms>
 * <equality-assertion> ::= "<" <simple-term> " = " <simple-term> ">"
 * <non-equality-assertion> ::= "<" <simple-term> " /= " <simple-term> ">"
 * <complex-term> ::= <simple-term> | <equality-assertion> | <non-equality-assertion>
 * <complex-terms> ::= "" | <complex-term> <complex-terms-1>
 * <complex-terms-1> ::= "" | ", " <complex-term> <complex-terms-1>
 * <rule> ::= <simple-term> "." | "{(" <complex-terms> ") => " <simple-term> "}."
 * <query> ::= "(" <complex-terms> ")?"
 * <command> ::= "quit!"
 * <no-op> ::= "" | "%" <comment>
 * <op> ::= <rule> | <query> | <command> | <no-op>
 * <input-line> ::= <op> <EOL>
 *)

open Printf

module S = Stream
module H = Hashtbl

type token =
  | TVar of string
  | TName of string
  | LBracket
  | RBracket
  | Colon
  | Comma
  | LAngle
  | RAngle
  | TEq
  | TNeq
  | Period
  | LBrace
  | RBrace
  | LParen
  | RParen
  | HashRocket
  | Question
  | QuitBang

type op =
  | Clause of rule
  | Query of complex list
  | Command of cmd

and rule =
  | Fact of simple
  | Rule of complex list * simple

and simple =
  | Name of string
  | Var of string
  | Rel of string * simple list

and complex =
  | Simple of simple
  | Eq of simple * simple
  | Neq of simple * simple

and cmd = Quit

(* lexer *)

exception Invalid_char of char

let rec lexer_of_channel (channel: in_channel): token S.t =
  let src = S.of_channel channel in
  S.from (function _ -> lex src)

and lexer_of_string (str: string): token S.t =
  let src = S.of_string str in
  S.from (function _ -> lex src)

and lex (stream: char S.t): token option =
  if S.peek stream = None then None
  else
    match S.next stream with
    (* skip whitespace *)
    | ' ' | '\t' | '\r' | '\n' -> lex stream
    (* variable *)
    | '#' ->
        let buffer = Buffer.create 10 in
        Buffer.add_char buffer '#';
        let token = lex_name stream buffer in
        Some (TVar token)
    (* name *)
    | ('A'..'Z' | 'a'..'z' as ch) ->
        let buffer = Buffer.create 10 in
        Buffer.add_char buffer ch;
        let token = lex_name_tail stream buffer in
        (* LL(1) parser hack *)
        if token = "quit" && S.peek stream = Some '!'
        then (S.junk stream; Some QuitBang)
        else Some (TName token)
    (* comment *)
    | '%' -> (lex_comment stream; lex stream)
    (* brackets *)
    | '[' -> Some LBracket
    | ']' -> Some RBracket
    | ':' -> Some Colon
    | ',' -> Some Comma
    | '<' -> Some LAngle
    | '>' -> Some RAngle
    | '=' -> Some (lex_eq_or_hashrocket stream)
    | '/' -> Some (lex_neq stream)
    | '.' -> Some Period
    | '{' -> Some LBrace
    | '}' -> Some RBrace
    | '(' -> Some LParen
    | ')' -> Some RParen
    | '?' -> Some Question
    | ch -> raise (Invalid_char ch)

and lex_name (stream: char S.t) (buffer: Buffer.t): string =
  match S.next stream with
  | ('A'..'Z' | 'a'..'z' as ch) ->
      Buffer.add_char buffer ch;
      lex_name_tail stream buffer
  | ch -> raise (Invalid_char ch)

and lex_name_tail (stream: char S.t) (buffer: Buffer.t): string =
  match S.peek stream with
  | Some ('A'..'Z' | 'a'..'z' | '0'..'9' | '-' as ch) ->
      S.junk stream;
      Buffer.add_char buffer ch;
      lex_name_tail stream buffer
  | _ -> Buffer.contents buffer

and lex_comment (stream: char S.t): unit =
  match S.peek stream with
  | None -> ()
  | Some '\n' -> S.junk stream
  | Some _ -> (S.junk stream; lex_comment stream)

and lex_eq_or_hashrocket (stream: char S.t): token =
  match S.peek stream with
  | Some '>' -> (S.junk stream; HashRocket)
  | _ -> TEq

and lex_neq (stream: char S.t): token =
  match S.next stream with
  | '=' -> TNeq
  | ch -> raise (Invalid_char ch)

(* parser *)

exception Syntax_Error

let rec parse (stream: token S.t): op list =
  parse_program stream

and eat (stream: token S.t) (token: token): unit =
  match S.peek stream with
  | Some t when t = token -> S.junk stream
  | _ -> raise Syntax_Error

and parse_program (stream: token S.t): op list =
  let rec loop () =
    if S.peek stream = None then []
    else
      let op = match S.peek stream with
      | Some QuitBang -> (S.junk stream; Command Quit)
      | Some LParen -> Query (parse_query stream)
      | Some _ -> Clause (parse_rule stream)
      | _ -> raise Syntax_Error
      in
        op :: loop ()
  in
    loop ()

and parse_query (stream: token S.t): complex list =
  S.junk stream;
  let qs = parse_complex_terms stream in
  (eat stream RParen; eat stream Question; qs)

and parse_rule (stream: token S.t): rule =
  match S.peek stream with
  | Some LBrace ->
      S.junk stream;
      eat stream LParen;
      let body = parse_complex_terms stream in
      eat stream RParen;
      eat stream HashRocket;
      let conclusion = parse_simple stream in
      (eat stream RBrace; eat stream Period; Rule (body, conclusion))
  | _ ->
      let fact = Fact (parse_simple stream) in
      (eat stream Period; fact)

and parse_complex_terms (stream: token S.t): complex list =
  let rec loop () =
    if S.peek stream <> (Some Comma) then []
    else
      (S.junk stream;
       let q = parse_complex stream in
       q :: loop ())
  in
    match S.peek stream with
    | Some (TName _ | TVar _ | LBracket | LAngle) ->
        let q = parse_complex stream in
        q :: loop ()
    | _ -> []

and parse_simple_terms (stream: token S.t): simple list =
  let rec loop () =
    if S.peek stream <> (Some Comma) then []
    else
      (S.junk stream;
       let q = parse_simple stream in
       q :: loop ())
  in
    match S.peek stream with
    | Some (TName _ | TVar _ | LBracket) ->
        let q = parse_simple stream in
        q :: loop ()
    | _ -> []

and parse_simple (stream: token S.t): simple =
  match S.next stream with
  | TName n -> Name n
  | TVar v -> Var v
  | LBracket ->
      let rel = parse_relation stream in
      (eat stream RBracket; rel)
  | _ -> raise Syntax_Error

and parse_complex (stream: token S.t): complex =
  match S.peek stream with
  | Some LAngle ->
      S.junk stream;
      let ass = parse_eq_assertion stream in
      (eat stream RAngle; ass)
  | _ -> Simple (parse_simple stream)

and parse_relation (stream: token S.t): simple =
  match S.next stream with
  | TName n ->
      eat stream Colon;
      Rel (n, parse_simple_terms stream)
  | _ -> raise Syntax_Error

and parse_eq_assertion (stream: token S.t): complex =
  let l = parse_simple stream in
  match S.next stream with
  | TEq -> Eq (l, parse_simple stream)
  | TNeq -> Neq (l, parse_simple stream )
  | _ -> raise Syntax_Error

(* print *)
let rec print_simple (outx: out_channel) = function
  | Name x | Var x -> output_string outx x
  | Rel (name, rel) ->
      fprintf outx "[%s: " name;
      begin match rel with
      | [] -> ()
      | x :: xs ->
          print_simple outx x;
          List.iter (fprintf outx ", %a" print_simple) xs
      end;
      output_string outx "]"

(* evaluator *)

module SS = Set.Make(String)
module Bindings = Map.Make(String)

type clause = complex list * simple list  (* premises => conclusion *)
type bindings = simple Bindings.t

exception Fail

module DB = struct
  let clauses: (string, clause list) H.t  = H.create 32
  let clear (): unit = H.clear clauses
  let add (x: string) (y: clause): unit =
    try
      let l = H.find clauses x in
      H.replace clauses x (y :: l)
    with Not_found -> H.add clauses x [y]
  let find (x: string): clause list =
    try H.find clauses x
    with Not_found -> (H.add clauses x []; [])
end

module G = struct
  let c = ref 0
  let genid (): int = c := !c + 1; !c
end

let mapcat f coll =
  coll |> List.map f |> List.concat

let rec eval = function
  | Clause c -> (eval_clause c; print_endline "Ok.")
  | Query q -> (eval_query q; print_endline "Ready.")
  | Command _ -> (print_endline "Bye."; exit 0)

and eval_clause = function
  | Fact (Rel (name, rel)) -> DB.add name ([], rel)
  | Rule (clauses, Rel (name, rel)) -> DB.add name (clauses, rel)
  | _ -> ()

and eval_query (qs: complex list): unit =
  match prove_all qs Bindings.empty with
  | [] -> print_endline "UNSAT"
  | solutions ->
      let vars = variables_in qs in
      let print_solution b =
        if vars = SS.empty
        then print_endline "SAT"
        else
          (print_endline "SAT:";
           print_endline "=====";
           SS.iter (fun v ->
             printf "%s := %a\n" v print_simple (subst b (Var v))) vars)
      in List.iter print_solution (List.rev solutions)

and variables_in (xs: complex list): SS.t =
  let rec collect_complex vars = function
    | Eq  (x, y) -> collect_simple (collect_simple vars x) y
    | Neq (x, y) -> collect_simple (collect_simple vars x) y
    | Simple s -> collect_simple vars s
  and collect_simple vars = function
    | Var v -> SS.add v vars
    | Rel (_, body) -> List.fold_left collect_simple vars body
    | _ -> vars
  in
    List.fold_left collect_complex SS.empty xs

(* unification *)

and unify (x: simple) (y: simple) (b: bindings): bindings =
  match (x, y) with
  | _ when x = y -> b
  | (Var x, _) -> unify_variable x y b
  | (_, Var y) -> unify_variable y x b
  | (Rel (n1, rel1), Rel (n2, rel2)) when n1 = n2 -> unify_list rel1 rel2 b
  | _ -> raise Fail

and unify_list (x: simple list) (y: simple list) (b: bindings): bindings =
  match (x, y) with
  | ([], []) -> b
  | (x::xs, y::ys) ->
      let b' = unify x y b in
      unify_list xs ys b'
  | _ -> raise Fail

and unify_variable (x: string) (y: simple) (b: bindings): bindings =
  match y with
  | _ when Bindings.mem x b -> unify (Bindings.find x b) y b
  | (Var y) when Bindings.mem y b -> unify_variable x (Bindings.find y b) b
  | _  when occurs x y b -> raise Fail
  | _ -> Bindings.add x y b

and occurs (var: string) (exp: simple) (b: bindings): bool =
  match exp with
  | Var x when x = var -> true
  | Var x when Bindings.mem x b -> occurs var (Bindings.find x b) b
  | Rel (_, body) -> List.exists (fun c -> occurs var c b) body
  | _ -> false

(* recursive substitution *)
and subst (b: bindings) (exp: simple): simple =
  match exp with
  | Name n -> exp
  | Var v when Bindings.mem v b -> subst b (Bindings.find v b)
  | Rel (n, body) -> Rel (n, List.map (subst b) body)
  | _ -> exp

(* reasoning *)
and prove (goal: complex) (b: bindings): bindings list =
  match goal with
  | Eq (x, y) -> (try [unify x y b] with Fail -> [])
  | Simple s -> prove_simple s b
  | Neq _ -> failwith "impossible"

and prove_simple (goal: simple) (b: bindings): bindings list =
  match goal with
  | Name _ | Var _ -> [b]
  | Rel (name, body) ->
      mapcat (fun clause ->
        let (premises, conclusion) = rename clause in
        try prove_all premises (unify goal (Rel (name, conclusion)) b)
        with Fail -> []) (DB.find name)

(* and prove_all (goals: complex list) (b: bindings): bindings list =
  match goals with
  | [] -> [b]
  | Neq (l, r) :: xs ->
      (* filter out bindings that unifies x and y *)
      let bs = prove_all xs b in
      List.filter (prove_neq l r) bs
  | x :: xs -> mapcat (prove_all xs) (prove x b)

and prove_neq (x: simple) (y: simple) (b: bindings): bool =
  try (ignore (unify x y b); false)
  with Fail -> true
 *)

and prove_all (goals: complex list) (b: bindings): bindings list =
  (* Inequality is checked each time after proving a goal. *)
  let rec walk goals neqs b =
    match goals with
    | [] -> [b]
    | x :: xs ->
        let bs = prove x b in
        let bs' = List.filter (prove_neqs neqs) bs in
        mapcat (walk xs neqs) bs'
  and partition_neq goals =
    match goals with
    | [] -> [], []
    | Neq (l, r) :: xs ->
        let neqs, goals = partition_neq xs in
        ((l, r) :: neqs, goals)
    | x :: xs ->
        let neqs, goals = partition_neq xs in
        (neqs, x :: goals)
  in
    let neqs, goals = partition_neq goals in
    walk goals neqs b

(* prove whether bindings satisfies every non-equality clauses. *)
and prove_neqs (neqs: (simple * simple) list) (b: bindings): bool =
  match neqs with
  | [] -> true
  | (x, y) :: xs when prove_neq x y b -> prove_neqs xs b
  | _ -> false

and prove_neq (x: simple) (y: simple) (b: bindings): bool =
  match (x, y) with
  | _ when x = y -> false
  | (Var x, _) -> prove_neq_variable x y b
  | (_, Var y) -> prove_neq_variable y x b
  | (Rel (n1, xs), Rel (n2, ys)) when n1 = n2 -> prove_neq_list xs ys b
  | _ -> true

and prove_neq_variable (x: string) (y: simple) (b: bindings): bool =
  match y with
  | _ when Bindings.mem x b -> prove_neq (Bindings.find x b) y b
  | (Var y) when Bindings.mem y b -> prove_neq_variable x (Bindings.find y b) b
  | _ -> false

and prove_neq_list (xs: simple list) (ys: simple list) (b: bindings): bool =
  match (xs, ys) with
  | ([], []) -> false
  | (x::xs, y::ys) -> prove_neq x y b || prove_neq_list xs ys b
  | _ -> true

and rename (c: clause): clause =
  let id = G.genid () in
  let (premises, conclusion) = c in
  (List.map (rename_complex id) premises,
   List.map (rename_simple id) conclusion)

and rename_simple (id: int) (rule: simple): simple =
  match rule with
  | Name _ -> rule
  | Var v -> Var (v ^ string_of_int id)
  | Rel (name, body) ->
      let body' = List.map (rename_simple id) body in
      Rel (name, body')

and rename_complex (id: int) (rule: complex): complex =
  match rule with
  | Simple s -> Simple (rename_simple id s)
  | Eq (x, y) -> Eq (rename_simple id x, rename_simple id y)
  | Neq (x, y) -> Neq (rename_simple id x, rename_simple id y)

(* main *)
let main () =
  let l = lexer_of_channel stdin in
  let ast = parse l in
  List.iter eval ast

(* let () = main () *)