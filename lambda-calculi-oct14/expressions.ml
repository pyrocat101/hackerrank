open Scanf

(*

reach[i, j] = (result of numbers 0...i) mod 101 = j?
from[i, j] = if reach[i, j] = true, what operator is used?

reach[_, _] = false
reach[0, xs[0] mod 101] = true

j = {
  j + xs[i] mod 101,
  j - xs[i] mod 101,
  j * xs[i] mod 101,
  j / xs[i] mod 101,
} if reach[i - 1, j] = true
reach[i, j] = true

*)
let solve n xs =
  let reach = Array.make_matrix n 101 false in
  let from = Array.make_matrix n 101 (-1, "?") in
  (* base case *)
  reach.(0).(xs.(0) mod 101) <- true;
  (* inductive case *)
  for i = 1 to n - 1 do
    for j = 0 to 100 do
      if reach.(i - 1).(j) then
        let j' = (j + xs.(i)) mod 101 in
        reach.(i).(j') <- true;
        from.(i).(j') <- (j, "+");
        let j' = (j - xs.(i) + 101) mod 101 in
        reach.(i).(j') <- true;
        from.(i).(j') <- (j, "-");
        let j' = (j * xs.(i)) mod 101 in
        reach.(i).(j') <- true;
        from.(i).(j') <- (j, "*");
    done;
  done;
  assert reach.(n - 1).(0);
  (* recover *)
  let rec recover i j parts =
    if i = 0 then
      (string_of_int xs.(0)) :: parts
    else
      let j', op = from.(i).(j) in
      let parts' = op :: (string_of_int xs.(i)) :: parts in
      recover (i - 1) j' parts'
  in
  let parts = recover (n - 1) 0 [] in
  String.concat "" parts

let main () =
  let n = read_int () in
  let xs = Array.make n 0 in
  for i = 0 to n - 1 do
    xs.(i) <- scanf " %d" (fun x -> x)
  done;
  let exp = solve n xs in
  print_endline exp

let () = main ()