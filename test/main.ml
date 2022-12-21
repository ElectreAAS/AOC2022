module T = Domainslib.Task

let all pool =
  ("Utils.split_on", [ TestUtils.split_on_test ])
  :: List.map
       (fun (left, right) -> (left, List.map (fun f -> f pool) right))
       [
         ("Day 1", [ Test1.day ]);
         ("Day 2", [ Test2.day ]);
         ("Day 3", [ Test3.day ]);
         ("Day 4", [ Test4.day ]);
         ("Day 5", [ Test5.day ]);
         ("Day 6", [ Test6.day ]);
         ("Day 7", [ Test7.day ]);
         ("Day 8", [ Test8.day ]);
         ("Day 9", [ Test9.day ]);
         ("Day 10", [ Test10.day ]);
         ("Day 11", [ Test11.day ]);
         ("Day 12", [ Test12.day ]);
         ("Day 13", [ Test13.day ]);
         ("Day 14", [ Test14.day ]);
         ("Day 15", [ Test15.day ]);
       ]

let () =
  let pool = T.setup_pool ~name:"tester" ~num_domains:0 () in
  T.run pool (fun () -> Alcotest.run "Everything" (all pool));
  T.teardown_pool pool
