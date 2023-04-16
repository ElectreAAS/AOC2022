type pair = { low : int; high : int }

let pair_of_string ic = Scanf.bscanf ic "%d-%d" (fun low high -> { low; high })

let pairs_of_string line =
  Scanf.sscanf line "%r,%r" pair_of_string pair_of_string (fun left right ->
      (left, right))

let overlap (left, right) =
  match compare left.low right.low with
  | 0 -> true
  | -1 -> right.low <= left.high
  | _ -> left.low <= right.high

let day _ _ input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  Seq.fold_left
    (fun acc line -> if overlap @@ pairs_of_string line then acc + 1 else acc)
    0 lines
  |> string_of_int
