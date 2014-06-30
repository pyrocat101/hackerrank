type node = {
  mutable children: int list;
}

type employee = {
  mutable id: int;
  mutable salary: int;
}

type range = {
  mutable l: int;
  mutable r: int;
}

type tree = Tree of int * tree * tree | Leaf of int

let create_node () = { children = [] }
let create_employee () = { id = -1; salary = -1 }
let create_range () = { l = -1; r = -1 }

let read_tree n =
  (* root node *)
  (* never use index 0 *)
  let tree = Array.init (n + 1) (fun _ -> create_node ()) in
  (* parents *)
  for i = 2 to n do
    let (u, p) = Scanf.scanf " %d %d" (fun x y -> (x, y)) in
    let node = tree.(p) in
    node.children <- (u :: node.children)
  done;
  tree

let tour tree =
  let n = Array.length tree in
  (* id * salary *)
  let euler = Array.init (2 * n + 1) (fun _ -> create_employee ()) in
  let ranges = Array.init (n + 1) (fun _ -> create_range ()) in
  let rec dfs root dfn =
    let dfn = dfn + 1 in
    (* in *)
    euler.(dfn).id <- root;
    ranges.(root).l <- dfn;
    (* visit all children *)
    let dfn = List.fold_left (fun dfn child ->
      dfs child dfn
    ) dfn tree.(root).children in
    (* out *)
    let dfn = dfn + 1 in
    euler.(dfn).id <- root;
    ranges.(root).r <- dfn;
    dfn
  in
  let _ = dfs 1 0 in
  (euler, ranges)

let read_salary euler ranges n =
  let salaries = Array.init n (fun _ ->
    Scanf.scanf " %d" (fun x -> x)
  )
  in
  let unsorted_salaries = Array.copy salaries in
  let mappings = Hashtbl.create n in
  Array.fast_sort compare salaries;
  (* salary -> id mapping *)
  for i = 0 to (n - 1) do
    Hashtbl.add mappings salaries.(i) (i + 1);
  done;
  (* discretization *)
  let salary_to_id = Array.make (n + 1) (-1) in
  for i = 1 to n do
    let s = Hashtbl.find mappings unsorted_salaries.(i - 1) in
    (* discretized salary -> id *)
    salary_to_id.(s) <- i;
    let range = ranges.(i) in
    euler.(range.l).salary <- s;
    euler.(range.r).salary <- s
  done;
  salary_to_id

let size_of_tree t =
  match t with
    | Tree (size, _, _) -> size
    | Leaf size -> size

let rec make_interval_tree l r =
  match r - l + 1 with
    | 1 -> Leaf 0
    | _ ->
        let m = (l + r) / 2 in
        let left = make_interval_tree l m in
        let right = make_interval_tree (m + 1) r in
        let size = (size_of_tree left) + (size_of_tree right) in
        Tree (size, left, right)

and update_interval_tree tree l r salary =
  match tree with
    | Tree (size, left, right) ->
        let m = (l + r) / 2 in
        if salary <= m then
          (* update left *)
          let left' = update_interval_tree left l m salary in
          Tree (size + 1, left', right)
        else
          (* update right *)
          let right' = update_interval_tree right (m + 1) r salary in
          Tree (size + 1, left, right')
    | Leaf size -> Leaf (size + 1)

and kth v1 v2 l r k =
  match (v1, v2) with
    | (Leaf size1, Leaf size2) ->
        (* Printf.printf "l = %d\n" l; *)
        l
    | (Tree (size1, left1, right1),
       Tree (size2, left2, right2)) ->
        let d = (size_of_tree left2) - (size_of_tree left1) in
        let m = (l + r) / 2 in
        if k <= d then
          (* kth left *)
          kth left1 left2 l m k
        else
          (* (k - m)th right *)
          kth right1 right2 (m + 1) r (k - d)
    | _ -> failwith "kth"

let gen_root_history euler itv_tree n =
  let history = Array.make (2 * n + 1) (Leaf (-1)) in
  let rec loop tree idx =
    if idx = (2 * n + 1) then history
    else
      let root = update_interval_tree tree 1 n euler.(idx).salary in
      history.(idx) <- root;
      loop root (idx + 1)
  in
  loop itv_tree 1

let main () =
  let (n, q) = Scanf.scanf " %d %d" (fun x y -> (x, y)) in
  let tree = read_tree n in
  let (euler, ranges) = tour tree in
  let salary_to_id = read_salary euler ranges n in
  let itv_tree = make_interval_tree 1 n in
  let history = gen_root_history euler itv_tree n in
  (* query *)
  let rec loop times d =
    if times = 0 then ()
    else
      let (v, k) = Scanf.scanf " %d %d" (fun x y -> (x, y)) in
      let v = v + d in
      let start = ranges.(v).l in
      let end_ = ranges.(v).r - 1 in
      let d' = if start > end_ then
        begin
          Printf.printf "%d\n" (-1);
          0
        end
      else
        let s = kth history.(start) history.(end_) 1 n (2 * k) in
        let ans = salary_to_id.(s) in
        Printf.printf "%d\n" ans;
        ans
      in
      loop (times - 1) d'
  in
  loop q 0

let () = main ()