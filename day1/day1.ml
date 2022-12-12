let day _ contents =
  let lines = String.split_on_char '\n' contents in
  let _, max1, max2, max3 =
    List.fold_left
      (fun (acc, top1, top2, top3) line ->
        match int_of_string_opt line with
        | Some line -> (acc + line, top1, top2, top3)
        | None ->
            if acc > top1 then (0, acc, top1, top2)
            else if acc > top2 then (0, top1, acc, top2)
            else if acc > top3 then (0, top1, top2, acc)
            else (0, top1, top2, top3))
      (0, 0, 0, 0) lines
  in
  string_of_int (max1 + max2 + max3)
