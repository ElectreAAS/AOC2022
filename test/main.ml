module T = Domainslib.Task

let all fs pool =
  List.mapi
    (fun i expected ->
      ( Printf.sprintf "Day %d" i,
        `Quick,
        fun () ->
          Utils.get_test fs i (fun input_buffer ->
              let (module Day) = All.days.(i) in
              let result = Day.day false pool input_buffer in
              Alcotest.(check string)
                "puzzle input should be solved!" expected result) ))
    All.expected

let () =
  let pool =
    T.setup_pool ~name:"tester"
      ~num_domains:(Domain.recommended_domain_count () - 1)
      ()
  in
  Eio_main.run (fun env ->
      Alcotest.run "Everything" [ ("Test puzzle input", all env#fs pool) ]);
  T.teardown_pool pool
