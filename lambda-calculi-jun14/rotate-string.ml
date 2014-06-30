let rec implode str =
  let len = String.length str in
  let arr = Array.make len '0' in
  for i = 0 to len - 1 do
    arr.(i) <- str.[i]
  done;
  arr

and shift_from start arr =
  let len = Array.length arr in
  for i = start to len - 1 do
    print_char arr.(i)
  done;
  for i = 0 to start - 1 do
    print_char arr.(i)
  done

and rotate_str str =
  let arr = implode str in
  let len = Array.length arr in
  for i = 1 to len - 1 do
    shift_from i arr;
    print_char ' '
  done;
  (* identity *)
  for i = 0 to len - 1 do
    print_char arr.(i)
  done;
  print_char '\n'

and main () =
  let n = read_int () in
  for i = 1 to n do
    let s = read_line () in
    rotate_str s
  done

let () = main ()