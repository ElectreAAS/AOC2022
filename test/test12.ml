let day =
  let open Day12 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 12 in
  let result = day false contents in
  let expected = "31" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
