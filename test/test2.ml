let day =
  let open Day2 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 2 in
  let result = day false contents in
  let expected = "12" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
