open Utils.Extensions

let parse_stacks l =
  let nb_stacks = (String.length (List.hd l) / 4) + 1 in
  let arr = Array.make nb_stacks [] in
  List.iter
    (fun line ->
      for y = 0 to nb_stacks - 1 do
        match line.[(y * 4) + 1] with ' ' -> () | c -> arr.(y) <- c :: arr.(y)
      done)
    (List.rev l);
  arr

type instruction = { nb : int; src : int; dst : int }

let parse_instruction line =
  Scanf.sscanf line "move %d from %d to %d" (fun nb src dst ->
      { nb; src = src - 1; dst = dst - 1 })

let parse_input contents =
  let lines =
    String.split_on_char '\n' contents
    |> List.filter (fun line ->
           line <> "" && not (String.starts_with ~prefix:" 1" line))
  in
  let left, right = List.split_on (String.starts_with ~prefix:"move") lines in
  (parse_stacks left, List.map parse_instruction right)

let move arr instrs =
  let rec aux = function
    | [] -> arr
    | { nb; src; dst } :: rest ->
        let head, tail = List.split_at nb arr.(src) in
        arr.(dst) <- head @ arr.(dst);
        arr.(src) <- tail;
        aux rest
  in
  aux instrs

let day _ contents =
  let stack, instructions = parse_input contents in
  let new_stack = move stack instructions in
  Array.fold_left
    (fun str col -> Printf.sprintf "%s%c" str (List.hd col))
    "" new_stack
