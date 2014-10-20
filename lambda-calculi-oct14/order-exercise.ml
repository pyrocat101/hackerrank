type sum   = int
type left  = int
type right = int

type data = {
  left_max  : left  * sum;          (* max sum from [x..right_border] *)
  right_max : right * sum;          (* max sum from [left_border..x] *)
  local_max : left  * sum * right;  (* max sum from [x..y] *)
  sum       : sum;                  (* sum of [left_border..right_border] *)
}

type tree = Tree of data * tree * tree | Leaf of data

let min_int = -0x7fffffff

let rec data_of = function
  | Leaf(data)
  | Tree(data, _, _) -> data

and local_max_of = function
  | Leaf {left_max; right_max; local_max; sum}
  | Tree({left_max; right_max; local_max; sum}, _, _) -> local_max

and tree_of arr =
  let rec f lo hi =
    match hi - lo with
    | 0 ->
        let x = arr.(lo) in
        Leaf {
          left_max  = (lo, x);
          right_max = (lo, x);
          local_max = (lo, x, lo);
          sum       = x;
        }
    | _ ->
        let mid = lo + (hi - lo) / 2 in
        let left  = f lo mid in
        let right = f (mid + 1) hi in
        cons left right
  in
    f 0 (Array.length arr - 1)

and cons left right =
  let data = merge (data_of left) (data_of right) in
  Tree(data, left, right)

and merge {
    left_max  = (left_idx1, left_sum1);
    right_max = (right_idx1, right_sum1);
    local_max = (local_left1, local_sum1, local_right1);
    sum       = sum1;
  } {
    left_max  = (left_idx2, left_sum2);
    right_max = (right_idx2, right_sum2);
    local_max = (local_left2, local_sum2, local_right2);
    sum       = sum2;
  } =
  (* new left max *)
  let left_max =
    if left_sum1 < sum1 + left_sum2 then
      (left_idx2, sum1 + left_sum2)
    else
      (left_idx1, left_sum1)
  in
  (* new right max *)
  let right_max =
    if right_sum1 + sum2 >= right_sum2 then
      (right_idx1, right_sum1 + sum2)
    else
      (right_idx2, right_sum2)
  in
  (* new local max *)
  let local_max =
    let local_left', local_sum', local_right' =
      (* compare local max of two sub trees *)
      if local_sum1 > local_sum2 then
        (local_left1, local_sum1, local_right1)
      else
        (local_left2, local_sum2, local_right2)
    in
      if right_sum1 + left_sum2 > local_sum'
          || (right_sum1 + left_sum2 = local_sum'
              && (right_idx1, left_idx2) < (local_left', local_right'))
      then
        (right_idx1, right_sum1 + left_sum2, left_idx2)
      else
        (local_left', local_sum', local_right')
  in
  (* new sum *)
  let sum = sum1 + sum2 in
  {left_max; right_max; local_max; sum;}

and clear root lo hi l r =
  let rec f root lo hi =
    match hi - lo with
    | 0 -> Leaf {
          left_max  = lo, min_int;
          right_max = hi, min_int;
          local_max = lo, min_int, lo;
          sum       = min_int;
        }
    | _ ->
        let mid = lo + (hi - lo) / 2 in
        match root with
        | Tree(data, left, right) ->
            let left' = if mid >= l then f left lo mid else left
            and right' = if mid + 1 <= r then f right (mid + 1) hi else right in
            cons left' right'
        | _ -> assert false
  in
    f root lo hi

let main () =
  let n, k = Scanf.scanf " %d %d" (fun x y -> (x, y)) in
  let arr = Array.make n 0 in
    for i = 0 to n - 1 do
      arr.(i) <- Scanf.scanf " %d" (fun x -> x)
    done;
  let root = tree_of arr in
  let rec ktimes root = function
    | 0 -> ()
    | k ->
        let left_idx, max_sum, right_idx = local_max_of root in
        if max_sum <= 0 then ()
        else
          print_endline @@ string_of_int max_sum;
          let root' = clear root 0 (n - 1) left_idx right_idx in
          ktimes root' (k - 1)
  in
    ktimes root k

let () = main ()