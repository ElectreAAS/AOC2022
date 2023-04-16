module EBR = Eio.Buf_read
open EBR.Syntax

type move = Rock | Paper | Scissors

let parse_move =
  EBR.map
    (function
      | 'A' -> Rock
      | 'B' -> Paper
      | 'C' -> Scissors
      | s -> invalid_arg (Char.escaped s))
    EBR.any_char

type outcome = Lose | Draw | Win

let parse_outcome =
  EBR.map
    (function
      | 'X' -> Lose
      | 'Y' -> Draw
      | 'Z' -> Win
      | s -> invalid_arg (Char.escaped s))
    EBR.any_char

let points_of_combo left right =
  let outcome = match right with Lose -> 0 | Draw -> 3 | Win -> 6 in
  let shape =
    match (left, right) with
    | Rock, Draw | Paper, Lose | Scissors, Win -> 1
    | Rock, Win | Paper, Draw | Scissors, Lose -> 2
    | Rock, Lose | Paper, Win | Scissors, Draw -> 3
  in
  shape + outcome

let parse_combo =
  let+ move = parse_move and+ outcome = EBR.char ' ' *> parse_outcome in
  points_of_combo move outcome

let parse_fold =
  let rec loop sum =
    let* b = EBR.at_end_of_input in
    if b then EBR.return sum
    else
      let* points = parse_combo <* EBR.char '\n' in
      loop (sum + points)
  in
  loop 0

let day _ _ = EBR.map string_of_int parse_fold
