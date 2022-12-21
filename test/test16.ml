let day pool =
  let open Day16 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 16 in
  let result = day false contents pool in
  let expected = "1651" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
