let day =
  let open Day5 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 5 in
  let result = day false contents in
  let expected = "MCD" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
