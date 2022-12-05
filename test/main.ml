let days =
  [
    ("Day 1", [ Test1.day ]);
    ("Day 2", [ Test2.day ]);
    ("Day 3", [ Test3.day ]);
    ("Day 4", [ Test4.day ]);
    ("Day 5", [ Test5.day ]);
  ]

let () = Alcotest.run "Everything" days
