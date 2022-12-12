let day =
  let open Day4 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 4 in
  let result = day false contents in
  let expected = "4" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
