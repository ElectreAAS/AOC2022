type instr = NoOp | Add of int

let parse str =
  match Scanf.sscanf_opt str "addx %d" Fun.id with
  | Some n -> Add n
  | None -> NoOp

type state = { register : int; cycle : int; hanging : int option; sum : int }

let is_blessed n = n = 20 || n mod 40 = 20

let rec steps state instr =
  let sum =
    if is_blessed state.cycle then state.sum + (state.cycle * state.register)
    else state.sum
  in
  let cycle = succ state.cycle in
  match state.hanging with
  | Some n ->
      steps { register = state.register + n; cycle; hanging = None; sum } instr
  | None -> (
      match instr with
      | NoOp -> { state with cycle; sum }
      | Add n -> { state with cycle; hanging = Some n; sum })

let day _ contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let final_state =
    List.fold_left
      (fun state line -> steps state (parse line))
      { register = 1; cycle = 1; hanging = None; sum = 0 }
      lines
  in
  final_state.sum |> string_of_int
