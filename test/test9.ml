let day =
  let open Day9 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 9 in
  let result = day false contents in
  let expected = "36" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
