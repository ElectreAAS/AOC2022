type pair = { low : int; high : int }

let pair_of_string str =
  match String.split_on_char '-' str with
  | [ left; right ] -> { low = int_of_string left; high = int_of_string right }
  | _ -> invalid_arg str

let pairs_of_string str =
  match String.split_on_char ',' str with
  | [ left; right ] -> (pair_of_string left, pair_of_string right)
  | _ -> invalid_arg str

let overlap (left, right) =
  match compare left.low right.low with
  | 0 -> true
  | -1 -> right.low <= left.high
  | _ -> left.low <= right.high

let day _ contents _ =
  let lines = String.trim contents |> String.split_on_char '\n' in
  List.fold_left
    (fun acc line -> if overlap @@ pairs_of_string line then acc + 1 else acc)
    0 lines
  |> string_of_int
