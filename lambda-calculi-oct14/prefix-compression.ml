open Printf

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let glue l =
  let buffer = Buffer.create 10 in
  let rec f = function
    | [] -> Buffer.contents buffer
    | x :: xs -> Buffer.add_char buffer x; f xs
  in
   f l

let compress x y =
  let prefix = Buffer.create 10 in
  let rec f x y =
    match x, y with
    | x :: xs, y :: ys when x = y -> Buffer.add_char prefix x; f xs ys
    | _ -> Buffer.contents prefix, glue x, glue y
  in
    f (explode x) (explode y)

let main () =
  let x = read_line () in
  let y = read_line () in
  let prefix, x', y' = compress x y in
  printf "%d %s\n" (String.length prefix) prefix;
  printf "%d %s\n" (String.length x') x';
  printf "%d %s\n" (String.length y') y'

let () = main ()