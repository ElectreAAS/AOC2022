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
  let pool = T.setup_pool ~name:"tester" ~num_domains:7 () in
  T.run pool (fun () ->
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      Alcotest.run "Everything" [ ("Test puzzle input", all fs ()) ]);
  T.teardown_pool pool
