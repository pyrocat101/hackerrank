type ast = IncPointer
         | DecPointer
         | IncData
         | DecData
         | Output
         | Input
         | Loop of ast list

let rec parse_op stream =
  match Stream.next stream with
    | '>' -> IncPointer
    | '<' -> DecPointer
    | '+' -> IncData
    | '-' -> DecData
    | '.' -> Output
    | ',' -> Input
    | _ -> raise Stream.Failure

and parse_loop stream =
  Stream.junk stream; (* '[' *)
  let rec loop code =
    match Stream.peek stream with
      | None -> raise (Stream.Error "Unmatch '['")
      | Some ('>' | '<' | '+' | '-' | '.' | ',') -> loop ((parse_op stream) :: code)
      | Some '[' -> loop ((parse_loop stream) :: code)
      | Some ']' -> Stream.junk stream; Loop (List.rev code)
      | _ -> Stream.junk stream; loop code
  in
  loop []

and parse stream =
  let rec loop code =
    match Stream.peek stream with
      | None -> List.rev code
      | Some ('>' | '<' | '+' | '-' | '.' | ',') -> loop ((parse_op stream) :: code)
      | Some '[' -> loop ((parse_loop stream) :: code)
      | Some ']' -> raise (Stream.Error "Unmatched ']'")
      | _ -> Stream.junk stream; loop code
  in
  loop []

exception Process_timeout

let eval program input =
  let data = Array.make 30000 0 in
  let rec eval' program pointer op_count =
    if program = [] then
      (pointer, op_count)
    else if op_count >= 100000 then
      raise Process_timeout
    else match List.hd program with
      | IncPointer ->
          eval' (List.tl program) (pointer + 1) (op_count + 1)
      | DecPointer ->
          eval' (List.tl program) (pointer - 1) (op_count + 1)
      | IncData ->
          data.(pointer) <- (data.(pointer) + 1) mod 256;
          eval' (List.tl program) pointer (op_count + 1)
      | DecData ->
          data.(pointer) <- ((data.(pointer) - 1) mod 256 + 256) mod 256;
          eval' (List.tl program) pointer (op_count + 1)
      | Output ->
          print_char (Char.chr data.(pointer));
          eval' (List.tl program) pointer (op_count + 1)
      | Input ->
          data.(pointer) <- Char.code (Stream.next input);
          eval' (List.tl program) pointer (op_count + 1)
      (*
        [   If the byte pointed by data pointer is zero, then move instruction
            pointer to next matching ']', otherwise move instruction pointer
            to next command.

        ]   If the byte pointed by data pointer is non-zero, then move
            instruction pointer to previous matching '[' command, otherwise to
            next command.
      *)
      | Loop sub_program ->
        if data.(pointer) = 0 then
          eval' (List.tl program) pointer (op_count + 2)
        else
          let rec loop pointer op_count =
            if op_count >= 100000 then
              raise Process_timeout
            else
              let (pointer', op_count') = eval' sub_program pointer op_count in
              if data.(pointer') = 0 then
                (pointer', op_count' + 1)
              else
                loop pointer' (op_count' + 2)
          in
          let (pointer, op_count) = loop pointer (op_count + 1) in
          eval' (List.tl program) pointer op_count
  in
  let _ = eval' program 0 0 in ()

let _ =
  let _ = read_line () in
  let input' = read_line () in
  let input = Stream.of_string (String.sub input' 0 (String.length input' - 1)) in
  let program = parse (Stream.of_channel stdin) in
  try
    eval program input
  with
  | Process_timeout -> print_endline "\nPROCESS TIME OUT. KILLED!!!"