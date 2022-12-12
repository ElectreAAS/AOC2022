let day =
  let open Day8 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 8 in
  let result = day contents in
  let expected = "21" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
