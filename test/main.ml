let all =
  [
    ("Utils.split_on", [ TestUtils.split_on_test ]);
    ("Day 1", [ Test1.day ]);
    ("Day 2", [ Test2.day ]);
    ("Day 3", [ Test3.day ]);
    ("Day 4", [ Test4.day ]);
    ("Day 5", [ Test5.day ]);
    ("Day 6", [ Test6.day ]);
    ("Day 7", [ Test7.day ]);
    ("Day 8", [ Test8.day ]);
  ]

let () = Alcotest.run "Everything" all
