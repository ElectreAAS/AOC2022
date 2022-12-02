let nb_days = 2

let functions =
  [|
    Day1.day;
    Day2.day;
    (* Day3.day; *)
    (* Day4.day; *)
    (* Day5.day; *)
    (* Day6.day; *)
    (* Day7.day; *)
    (* Day8.day; *)
    (* Day9.day; *)
    (* Day10.day; *)
    (* Day11.day; *)
    (* Day12.day; *)
    (* Day13.day; *)
    (* Day14.day; *)
    (* Day15.day; *)
    (* Day16.day; *)
    (* Day17.day; *)
    (* Day18.day; *)
    (* Day19.day; *)
    (* Day20.day; *)
    (* Day21.day; *)
    (* Day22.day; *)
    (* Day23.day; *)
    (* Day24.day; *)
    (* Day25.day; *)
  |]

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