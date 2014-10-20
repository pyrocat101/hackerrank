let print_bool = function
  | true -> print_endline "True"
  | false -> print_endline "False"

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let full_color s =
  let occurs = Hashtbl.create 4 in
  Hashtbl.add occurs 'R' 0;
  Hashtbl.add occurs 'G' 0;
  Hashtbl.add occurs 'B' 0;
  Hashtbl.add occurs 'Y' 0;
  let count = Hashtbl.find occurs in
  let inc = (fun c -> Hashtbl.replace occurs c (1 + count c)) in
  let rec f = function
  | [] -> count 'R' = count 'G' && count 'Y' = count 'B'
  | x :: xs ->
      inc x;
      abs(count 'R' - count 'G') < 2 && abs(count 'Y' - count 'B') < 2 && f xs
  in
    f @@ explode s

let main () =
  let n = read_int () in
  for i = 1 to n do
    read_line () |> full_color |> print_bool
  done

let () = main ()