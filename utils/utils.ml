module Types = struct
  include Types
end

let slurp filename =
  let ic = open_in filename in
  let lines = ref [] in
  let rec read () =
    let line = input_line ic in
    lines := line :: !lines;
    read ()
  in
  try read ()
  with End_of_file ->
    close_in ic;
    String.concat "\n" (List.rev !lines)

let get_input n = Printf.sprintf "day%d/input.txt" n |> slurp
let get_test n = Printf.sprintf "../../../day%d/test.txt" n |> slurp
