let day =
  let open Day5 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 5 in
  let result = day contents in
  let expected = "TODO: add actual puzzle input" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
