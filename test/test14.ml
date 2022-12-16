let day =
  let open Day14 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 14 in
  let result = day false contents in
  let expected = "24" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
