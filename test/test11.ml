let day =
  let open Day11 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 11 in
  let result = day false contents in
  let expected = "10605" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
