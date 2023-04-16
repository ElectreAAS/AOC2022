let max_snacks (acc, top1, top2, top3) line =
  match int_of_string_opt line with
  | Some n -> (acc + n, top1, top2, top3)
  | None ->
      if acc > top1 then (0, acc, top1, top2)
      else if acc > top2 then (0, top1, acc, top2)
      else if acc > top3 then (0, top1, top2, acc)
      else (0, top1, top2, top3)

let day _ _ input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  let _, max1, max2, max3 =
    Seq.fold_left max_snacks (0, 0, 0, 0) lines |> Fun.flip max_snacks ""
  in
  string_of_int (max1 + max2 + max3)
