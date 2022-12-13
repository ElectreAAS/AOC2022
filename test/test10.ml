let day =
  let open Day10 in
  Alcotest.test_case "Test puzzle input" `Quick @@ fun () ->
  let contents = Utils.get_test 10 in
  let result = day false contents in
  let expected =
    {|
██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░
███░░░███░░░███░░░███░░░███░░░███░░░███░
████░░░░████░░░░████░░░░████░░░░████░░░░
█████░░░░░█████░░░░░█████░░░░░█████░░░░░
██████░░░░░░██████░░░░░░██████░░░░░░████
███████░░░░░░░███████░░░░░░░███████░░░░░
|}
  in
  Alcotest.(check string) "puzzle input should be solved!" expected result;
  ()
