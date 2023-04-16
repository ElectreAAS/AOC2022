let array_alldiff a =
  let len = Array.length a in
  try
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        if a.(i) = a.(j) then raise Exit
      done
    done;
    true
  with Exit -> false

let day _ _ input_buffer =
  let line = Eio.Buf_read.take 14 input_buffer in
  let marker_size = 14 in
  let array = Array.init marker_size (String.get line) in
  let rec loop i cursor =
    if array_alldiff array then i
    else (
      array.(cursor) <- Eio.Buf_read.any_char input_buffer;
      loop (i + 1) (succ cursor mod marker_size))
  in
  loop marker_size 0 |> string_of_int
