type move = Rock | Paper | Scissors

let move_of_string = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | s -> invalid_arg s

type outcome = Lose | Draw | Win

let outcome_of_string = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | s -> invalid_arg s

let points_of_combo (left, right) =
  let outcome = match right with Lose -> 0 | Draw -> 3 | Win -> 6 in
  let shape =
    match (left, right) with
    | Rock, Draw | Paper, Lose | Scissors, Win -> 1
    | Rock, Win | Paper, Draw | Scissors, Lose -> 2
    | Rock, Lose | Paper, Win | Scissors, Draw -> 3
  in
  shape + outcome

let parse_combo = function
  | [ x; y ] -> (move_of_string x, outcome_of_string y)
  | l -> invalid_arg (String.concat " " l)

let day _ contents _ =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let points =
    List.fold_left
      (fun sum line ->
        line |> String.split_on_char ' ' |> parse_combo |> points_of_combo
        |> ( + ) sum)
      0 lines
  in
  string_of_int points
