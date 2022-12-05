let days =
  [
    ("Day 1", [ Test1.day ]); ("Day 2", [ Test2.day ]); ("Day 3", [ Test3.day ]);
  ]

let () = Alcotest.run "Everything" days
