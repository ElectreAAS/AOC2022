module EBR = Eio.Buf_read
open EBR.Syntax
open Extensions

let parse_stacks s =
  let s = s |> List.of_seq |> List.rev in
  let nb_stacks = (String.length (List.hd s) / 4) + 1 in
  let arr = Array.make nb_stacks [] in
  List.iter
    (fun line ->
      for y = 0 to nb_stacks - 1 do
        match line.[(y * 4) + 1] with ' ' -> () | c -> arr.(y) <- c :: arr.(y)
      done)
    s;
  arr

type instruction = { nb : int; src : int; dst : int }

let parse_instruction line =
  Scanf.sscanf line "move %d from %d to %d" (fun nb src dst ->
      { nb; src = src - 1; dst = dst - 1 })

let parse_input =
  let+ lines = EBR.lines in
  let lines =
    Seq.filter
      (fun line -> line <> "" && not (String.starts_with ~prefix:" 1" line))
      lines
  in
  let instrs, stack_info =
    Seq.partition_map
      (fun line ->
        if String.starts_with ~prefix:"move" line then Either.Left line
        else Either.Right line)
      (Seq.memoize lines)
  in
  (parse_stacks stack_info, Seq.map parse_instruction instrs)

let move arr instrs =
  let rec aux instrs =
    match instrs () with
    | Seq.Nil -> arr
    | Seq.Cons ({ nb; src; dst }, rest) ->
        let head, tail = List.split_at nb arr.(src) in
        arr.(dst) <- head @ arr.(dst);
        arr.(src) <- tail;
        aux rest
  in
  aux instrs

let day _ _ =
  let+ stack, instructions = parse_input in
  let new_stack = move stack instructions in
  Array.fold_left
    (fun str col -> Printf.sprintf "%s%c" str (List.hd col))
    "" new_stack
