let rec bisect friends m =
  let len = Array.length friends in
  let aux = Array.make len 0 in
  let rec happy k m =
    for i = 0 to len - 1 do
      aux.(i) <- (function (a, h) -> a + (k - 1) * h) friends.(i)
    done;
    Array.fast_sort (-) aux;
    let rec fold_k i acc =
      if i >= k then acc
      else
        let acc' = acc + aux.(i) in
        fold_k (i + 1) acc'
    in
    (fold_k 0 0) <= m
  in
  let rec loop lo hi =
    if lo <= hi then
      let mid = lo + (hi - lo) / 2 in
      if not (happy mid m) then
        loop lo (mid - 1)
      else
        loop (mid + 1) hi
    else lo - 1
  in
  loop 1 len

and read_int_list times =
  if times = 0 then []
  else
    let x = Scanf.scanf " %d" (function x -> x) in
    x :: (read_int_list (times - 1))

and main () =
  let (n, m) = Scanf.scanf "%d %d\n" (fun x y -> (x, y)) in
  let a = 2500000000000001 :: (read_int_list n) in
  let h = 1 :: (read_int_list n) in
  let friends = Array.of_list (List.combine a h) in
  let idx = bisect friends m in
  print_int idx;
  print_char '\n'

let () = main ()