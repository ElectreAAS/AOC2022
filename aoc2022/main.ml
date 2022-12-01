let nb_days = 1
let functions = [| Day1.day |]

let run n =
  let contents = Utils.get_input n in
  let before = Mtime_clock.counter () in
  let result = functions.(n - 1) contents in
  let elapsed = Mtime_clock.count before |> Mtime.Span.to_us |> int_of_float in
  Printf.printf "Day %2d done in %3dμs: %s\n" n elapsed result;
  elapsed

let dispatch = function
  | 0 ->
      let total_time = ref 0 in
      for i = 1 to nb_days do
        total_time := !total_time + run i
      done;
      Printf.printf "Total: %dμs\n" !total_time
  | n when n >= 1 && n <= nb_days -> ignore (run n)
  | _ -> ()

let () =
  let num = Sys.argv.(1) |> int_of_string in
  dispatch num