let day =
  let open Day11 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 11 in
  let result = day false contents in
  let expected = "2713310158" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
