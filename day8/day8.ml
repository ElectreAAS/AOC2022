let parse contents =
  let len = String.index contents '\n' in
  Array.init len (fun y ->
      Array.init len (fun x -> contents.[x + ((len + 1) * y)]))

let pp forest =
  let pp_char c =
    Printf.printf "\x1b[38;2;%sm%c"
      (* Green gradient *)
      (match c with
      | '0' -> "233;247;239"
      | '1' -> "212;239;223"
      | '2' -> "169;223;191"
      | '3' -> "125;206;160"
      | '4' -> "82;190;128"
      | '5' -> "39;174;96"
      | '6' -> "34;153;84"
      | '7' -> "30;132;73"
      | '8' -> "25;111;61"
      | _ -> "20;90;50")
      c
  in
  print_newline ();
  Array.iter
    (fun r ->
      Array.iter pp_char r;
      print_newline ())
    forest;
  Printf.printf "\x1b[0m"

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

let day display contents _ =
  let forest = parse contents in
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
