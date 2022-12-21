let day pool =
  let open Day6 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 6 in
  let result = day false contents pool in
  let expected = "19" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
