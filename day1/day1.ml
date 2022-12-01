let day contents =
  let lines = String.split_on_char '\n' contents in
  let _, max =
    List.fold_left
      (fun (acc, most) line ->
        if line = "" then (0, most)
        else
          let current = acc + int_of_string line in
          (current, max current most))
      (0, 0) lines
  in
  string_of_int max
