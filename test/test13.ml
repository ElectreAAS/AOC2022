let day pool =
  let open Day13 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 13 in
  let result = day false contents pool in
  let expected = "140" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
