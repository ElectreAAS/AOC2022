let day pool =
  let open Day15 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 15 in
  let result = day false contents pool in
  let expected = "56000011" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
