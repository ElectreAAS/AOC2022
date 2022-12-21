let day =
  let open Day15 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 15 in
  let result = day false contents in
  let expected = "26" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
