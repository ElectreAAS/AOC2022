open Extensions

let split_on_test =
  Alcotest.test_case "Split_on" `Quick @@ fun () ->
  let contents = Utils.get_test 5 in
  let lines = String.split_on_char '\n' contents in
  let result =
    List.split_on (fun line -> String.starts_with ~prefix:"move" line) lines
  in
  let expected =
    ( [ "    [D]    "; "[N] [C]    "; "[Z] [M] [P]"; " 1   2   3 "; "" ],
      [
        "move 1 from 2 to 1";
        "move 3 from 1 to 3";
        "move 2 from 2 to 1";
        "move 1 from 1 to 2";
        "";
      ] )
  in
  Alcotest.(check (pair (list string) (list string)))
    "puzzle input should be solved!" expected result;
  ()
