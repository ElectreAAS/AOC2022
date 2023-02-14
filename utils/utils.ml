let slurp path =
  let ( / ) = Eio.Path.( / ) in
  Eio_main.run @@ fun env ->
  let path = Eio.Stdenv.fs env / path in
  Eio.Path.load path

let get_input n = Printf.sprintf "day%d/input.txt" n |> slurp
let get_test n = Printf.sprintf "../../../day%d/test.txt" n |> slurp
