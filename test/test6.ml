let day =
  let open Day6 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 6 in
  let result = day contents in
  let expected = "7" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
