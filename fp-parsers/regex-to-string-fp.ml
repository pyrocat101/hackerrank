type maybe = True of string
           | Maybe
           | False

type regex = Alt of regex * regex * maybe array
           | Then of regex * regex * maybe array
           | Star of regex * maybe array
           | Char of char

let make_memo () = Array.make 101 Maybe

exception Syntax_error

let expect s i c =
  if s.[i] = c then
    i + 1
  else
    raise Syntax_error

let rec alt s i =
  let (a, i) = thn s i in
  if s.[i] = '|' then
    let (b, i') = alt s (i + 1) in
    (Alt (a, b, make_memo ()), i')
  else
    (a, i)
and thn s i =
  let (c, i) = star s i in
  match s.[i] with
  | 'A'..'Z' | 'a'..'z' | '(' ->
    let (d, i) = thn s i in
    (Then (c, d, make_memo ()), i)
  | _ -> (c, i)
and star s i =
  let (p, i) = prime s i in
  if s.[i] = '*' then
    (Star (p, make_memo ()), i + 1)
  else
    (p, i)
and prime s i =
  match s.[i] with
  | 'A'..'Z' | 'a'..'z' ->
    (Char s.[i], i + 1)
  | _ ->
    let i' = expect s i '(' in
    let (r, i'') = alt s i' in
    let i''' = expect s i'' ')' in
    (r, i''')
and parse_regex s =
  let s' = s ^ "\000" in
  let (tree, i) = alt s' 0 in
  if s'.[i] = '\000' then
    tree
  else
    raise Syntax_error

let rec solve n exp =
  match exp with
  | Char ch ->
    if n = 1 then
      True (String.make 1 ch)
    else
      False
  | Star (a, memo) ->
    if n = 0 then
      True ""
    else if memo.(n) = Maybe then
      let rec f i =
        if i > n then
          False
        else
          match (solve i a) with
          | True s1 ->
            (match (solve (n - i) exp) with
             | True s2 -> True (s1 ^ s2)
             | _ -> f (i + 1))
          | _ -> f (i + 1)
      in
      memo.(n) <- f 1; memo.(n)
    else memo.(n)
  | Then (a, b, memo) ->
    if memo.(n) = Maybe then
      let rec f i =
        if i > n then
          False
        else
          match (solve i a) with
          | True s1 ->
            (match (solve (n - i) b) with
             | True s2 -> True (s1 ^ s2)
             | _ -> f (i + 1))
          | _ -> f (i + 1)
      in
      memo.(n) <- f 0; memo.(n)
    else memo.(n)
  | Alt (a, b, memo) ->
    if memo.(n) = Maybe then
      match (solve n a) with
      | True s -> memo.(n) <- True s; memo.(n)
      | _      -> memo.(n) <- solve n b; memo.(n)
    else memo.(n)

let main () =
  let n = read_int () in
  let re = read_line () in
  let exp = parse_regex re in
  match (solve n exp) with
  | True s -> print_endline s
  | _ -> print_endline "NIL"

let _ = main ()