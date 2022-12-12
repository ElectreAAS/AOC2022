let day =
  let open Day7 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 7 in
  let result = day contents in
  let expected = "24933642" in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
