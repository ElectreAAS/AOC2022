type instr = NoOp | Add of int

let parse str =
  match Scanf.sscanf_opt str "addx %d" Fun.id with
  | Some n -> Add n
  | None -> NoOp

type state = { register : int; cycle : int; hanging : int option; crt : string }

let rec steps state instr =
  let crt =
    state.crt
    ^ (match ((state.cycle - 1) mod 40) - state.register with
      | -1 | 0 | 1 -> "█"
      | _ -> "░")
    ^ if state.cycle mod 40 = 0 then "\n" else ""
  in
  let cycle = state.cycle + 1 in
  match state.hanging with
  | Some n ->
      steps { register = state.register + n; cycle; hanging = None; crt } instr
  | None -> (
      match instr with
      | NoOp -> { state with cycle; crt }
      | Add n -> { state with cycle; hanging = Some n; crt })

let day _ _ input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  (* FIXME: use Eio better *)
  let final_state =
    Seq.fold_left
      (fun state line -> steps state (parse line))
      { register = 1; cycle = 1; hanging = None; crt = "\n" }
      lines
  in
  final_state.crt
