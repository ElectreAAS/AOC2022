module EBR = Eio.Buf_read
open EBR.Syntax

let parse =
  let+ contents = EBR.take_all in
  (* FIXME: use Eio better *)
  let len = String.index contents '\n' in
  Array.init len (fun y ->
      Array.init len (fun x -> contents.[x + ((len + 1) * y)]))

let pp forest =
  let open Notty in
  let pp_char c =
    I.char
      ((* Green gradient *)
       let r, g, b =
         match c with
         | '0' -> (233, 247, 239)
         | '1' -> (212, 239, 223)
         | '2' -> (169, 223, 191)
         | '3' -> (125, 206, 160)
         | '4' -> (82, 190, 128)
         | '5' -> (39, 174, 96)
         | '6' -> (34, 153, 84)
         | '7' -> (30, 132, 73)
         | '8' -> (25, 111, 61)
         | _ -> (20, 90, 50)
       in
       A.fg (A.rgb_888 ~r ~g ~b))
      c 1 1
  in
  I.(
    void 0 1
    <-> tabulate (Array.length forest) (Array.length forest.(0)) @@ fun x y ->
        pp_char forest.(x).(y))
  |> Notty_unix.output_image

let is_visible x y forest =
  let len = Array.length forest in
  let self = forest.(x).(y) in
  let rec up n = n < 0 || (forest.(x).(n) < self && up (n - 1)) in
  let rec down n = n >= len || (forest.(x).(n) < self && down (n + 1)) in
  let rec left n = n < 0 || (forest.(n).(y) < self && left (n - 1)) in
  let rec right n = n >= len || (forest.(n).(y) < self && right (n + 1)) in
  up (y - 1) || down (y + 1) || left (x - 1) || right (x + 1)

let scenic_score x y forest =
  let len = Array.length forest in
  let self = forest.(x).(y) in
  let rec up sum n =
    if n < 0 then sum
    else if forest.(x).(n) < self then up (sum + 1) (n - 1)
    else sum + 1
  in
  let rec down sum n =
    if n >= len then sum
    else if forest.(x).(n) < self then down (sum + 1) (n + 1)
    else sum + 1
  in
  let rec left sum n =
    if n < 0 then sum
    else if forest.(n).(y) < self then left (sum + 1) (n - 1)
    else sum + 1
  in
  let rec right sum n =
    if n >= len then sum
    else if forest.(n).(y) < self then right (sum + 1) (n + 1)
    else sum + 1
  in
  up 0 (y - 1) * down 0 (y + 1) * left 0 (x - 1) * right 0 (x + 1)

let day display _ input_buffer =
  let forest = parse input_buffer in
  if display then pp forest;
  let len = Array.length forest in
  let rec loop x y count =
    if x = len - 1 then count
    else
      let this = scenic_score x y forest in
      let x, y = if y = len - 2 then (x + 1, 1) else (x, y + 1) in
      loop x y (max count this)
  in
  string_of_int (loop 1 1 0)
