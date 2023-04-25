module T = Domainslib.Task

let nb_days = Array.length All.days

let dispatch fs n display pool =
  let run n =
    let (module Day) = All.days.(n) in
    let elapsed, result =
      Utils.with_timer (fun () -> Utils.get_input fs n (Day.day display pool))
    in
    Printf.printf "\nDay %2d: finished in %7.3fms, with result = %s" n elapsed
      result;
    elapsed
  in
  match n with
  | 0 ->
      let total_time = ref 0.0 in
      for i = 0 to nb_days - 1 do
        total_time := !total_time +. run i
      done;
      Printf.printf "\nTotal: %.3fms\n" !total_time
  | n when n >= 1 && n <= nb_days ->
      ignore (run n);
      print_newline ()
  | _ -> ()

let () =
  let error () =
    print_endline "\nPlease enter the date of the challenge or 0 for all."
  in
  let argc = Array.length Sys.argv in
  if argc < 2 then error ()
  else
    let display = argc = 3 && Sys.argv.(2) = "y" in
    match Sys.argv.(1) |> int_of_string_opt with
    | None -> error ()
    | Some n ->
        let pool =
          T.setup_pool ~name:"dispatcher"
            ~num_domains:(Domain.recommended_domain_count () - 1)
            ()
        in
        Eio_main.run (fun env -> dispatch env#fs n display pool);
        T.teardown_pool pool
