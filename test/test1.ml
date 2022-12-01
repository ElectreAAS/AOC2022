let day =
  let open Day1 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 1 in
  let result = day contents in
  let expected = "45000" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()