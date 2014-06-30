type position = int * int
type faces = int * int * int
type state = position * faces
type direction = Left | Up

let rotate (top, front, right) direction =
  match direction with
    | Left ->
        let top' = right in
        let front' = front in
        let right' = (7 - top) in
        (top', front', right')
    | Up ->
        let top' = front in
        let front' = (7 - top) in
        let right' = right in
        (top', front', right')

let rec search state tbl =
  let (pos, faces) = state in
  let (x, y) = pos in
  let (top, front, right) = faces in
  if Hashtbl.mem tbl state then
    Hashtbl.find tbl state
  else
    (* search for up/left *)
    let u =
      if x = 1 then -1
      else search ((x - 1, y), (rotate faces Up)) tbl in
    let l =
      if y = 1 then -1
      else search ((x, y - 1), (rotate faces Left)) tbl in
    (* cache answer *)
    let ans = max u l in
    (* propogate impossible state *)
    if ans < 0 then
      begin
        Hashtbl.replace tbl state (-1);
        (-1)
      end
    else
      let ans' = ans + top in
      Hashtbl.replace tbl state ans';
      ans'

let final_faces = [
  (1,2,4);
  (1,3,2);
  (1,4,5);
  (1,5,3);

  (2,1,3);
  (2,3,6);
  (2,4,1);
  (2,6,4);

  (3,1,5);
  (3,2,1);
  (3,5,6);
  (3,6,2);

  (4,1,2);
  (4,2,6);
  (4,5,1);
  (4,6,5);

  (5,1,4);
  (5,3,1);
  (5,4,6);
  (5,6,3);

  (6,2,3);
  (6,3,5);
  (6,4,2);
  (6,5,4);
]

let solve final_pos tbl =
  let sums = List.map (function faces ->
    search (final_pos, faces) tbl
  ) final_faces
  in
  List.fold_left max (-1) sums

let main () =
  let n = read_int () in
  let tbl = Hashtbl.create 216 in
  Hashtbl.replace tbl ((1, 1), (1, 2, 4)) 1;
  let rec loop times =
    if times = 0 then ()
    else
      let pos = Scanf.scanf " %d %d" (fun x y -> (x, y)) in
      let sum = solve pos tbl in
      Printf.printf "%d\n" sum;
      loop (times - 1)
  in
  loop n

let () = main ()