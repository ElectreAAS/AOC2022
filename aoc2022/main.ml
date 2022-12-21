module T = Domainslib.Task

let nb_days = 16

let functions =
  [|
    Day1.day;
    Day2.day;
    Day3.day;
    Day4.day;
    Day5.day;
    Day6.day;
    Day7.day;
    Day8.day;
    Day9.day;
    Day10.day;
    Day11.day;
    Day12.day;
    Day13.day;
    Day14.day;
    Day15.day;
    Day16.day;
  |]

let run display n pool =
  let contents = Utils.get_input n in
  let before = Mtime_clock.counter () in
  let result = functions.(n - 1) display contents pool in
  let elapsed = Mtime_clock.count before |> Mtime.Span.to_ms in
  Printf.printf "\nDay %2d: finished in %7.3fms, with result = %s" n elapsed
    result;
  elapsed

let dispatch display n =
  let pool =
    T.setup_pool ~name:"dispatcher"
      ~num_domains:(Domain.recommended_domain_count () - 1)
      ()
  in
  T.run pool (fun () ->
      match n with
      | 0 ->
          let total_time = ref 0.0 in
          for i = 1 to nb_days do
            total_time := !total_time +. run display i pool
          done;
          Printf.printf "\nTotal: %.3fms\n" !total_time
      | n when n >= 1 && n <= nb_days ->
          ignore (run display n pool);
          print_newline ()
      | _ -> ());
  T.teardown_pool pool

let () =
  let error () =
    print_endline "\nPlease enter the date of the challenge or 0 for all."
  in
  let argc = Array.length Sys.argv in
  if argc < 2 then error ()
  else
    let display = argc = 3 && Sys.argv.(2) = "y" in
    match Sys.argv.(1) |> int_of_string_opt with
    | Some n -> dispatch display n
    | None -> error ()
