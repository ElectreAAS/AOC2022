type path = (int * int) list

let parse (minx, maxx, maxy) line =
  let chunks = String.split_on_char '-' line in
  let pathize (minx, maxx, maxy) str =
    let no_arr =
      if String.starts_with ~prefix:">" str then
        String.sub str 2 (String.length str - 2)
      else str
    in
    Scanf.sscanf no_arr "%d,%d" (fun x y ->
        ((min minx x, max maxx x, max maxy y), (x, y)))
  in
  List.fold_left_map pathize (minx, maxx, maxy) chunks

type cell = Rock | Sand | Air
type state = { mutable grid : cell array array; mutable pour : int }

let sand_pour = 500

let init_state (minx, maxx, maxy) (paths : path list) =
  let width = maxx - minx + 1 in
  let height = maxy + 3 in
  let grid = Array.make_matrix width height Air in
  let rec draw_path = function
    | [] | [ _ ] -> ()
    | (lx, ly) :: (rx, ry) :: rest when lx = rx ->
        let down, up = if ly < ry then (ly, ry) else (ry, ly) in
        for y = down to up do
          grid.(lx - minx).(y) <- Rock
        done;
        draw_path ((rx, ry) :: rest)
    | (lx, ly) :: (rx, ry) :: rest ->
        assert (ly = ry);
        let down, up = if lx < rx then (lx, rx) else (rx, lx) in
        for x = down to up do
          grid.(x - minx).(ly) <- Rock
        done;
        draw_path ((rx, ry) :: rest)
  in
  List.iter draw_path paths;
  for x = 0 to width - 1 do
    grid.(x).(maxy + 2) <- Rock
  done;
  { grid; pour = sand_pour - minx }

let pp state =
  let width = Array.length state.grid - 1 in
  let height = Array.length state.grid.(0) - 1 in
  print_string "\n┌";
  for _ = 0 to width + 2 do
    print_string "─"
  done;
  print_string "┐\n│";
  for x = 0 to width + 2 do
    if x = state.pour then print_string "⇣" else print_char ' '
  done;
  print_string "│\n";
  for y = 0 to height do
    print_string "│ ";
    for x = 0 to width do
      let str =
        match state.grid.(x).(y) with Rock -> "█" | Air -> " " | Sand -> "░"
      in
      print_string str
    done;
    print_string " │\n"
  done;
  print_string "│";
  for _ = 0 to width + 2 do
    print_char ' '
  done;
  print_string "│\n└";
  for _ = 0 to width + 2 do
    print_string "─"
  done;
  Printf.printf "┘\n%!"

let expand_size = 128

let expand state is_left =
  let width = Array.length state.grid in
  let height = Array.length state.grid.(0) in
  let initialize x =
    if if is_left then x >= expand_size else x < width then
      Array.copy state.grid.(if is_left then x - expand_size else x)
    else Array.make height Air
  in
  let grid = Array.init (width + expand_size) initialize in
  for x = 0 to width + expand_size - 1 do
    grid.(x).(height - 1) <- Rock
  done;
  state.grid <- grid;
  if is_left then (
    state.pour <- state.pour + expand_size;
    expand_size)
  else width - 1

let run state =
  let rec pour_from x y path =
    match
      ( state.grid.(x - 1).(y + 1),
        state.grid.(x).(y + 1),
        state.grid.(x + 1).(y + 1) )
    with
    | exception Invalid_argument _ ->
        let new_x = expand state (x = 0) in
        let new_path =
          if x = 0 then List.map (fun (x, y) -> (x + expand_size, y)) path
          else path
        in
        pour_from new_x y new_path
    | _, Air, _ -> pour_from x (y + 1) ((x, y) :: path)
    | Air, _, _ -> pour_from (x - 1) (y + 1) ((x, y) :: path)
    | _, _, Air -> pour_from (x + 1) (y + 1) ((x, y) :: path)
    | _ -> (x, y) :: path
  in
  let rec loop n path =
    let x, y, xs =
      match path with [] -> (state.pour, 0, []) | (x, y) :: xs -> (x, y, xs)
    in
    match pour_from x y xs with
    | [] -> failwith "Path should never be empty after pouring"
    | (x, 0) :: _ when x = state.pour -> n
    | (x, y) :: xs ->
        state.grid.(x).(y) <- Sand;
        loop (n + 1) xs
  in
  loop 1 []

let day display contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let bounds, paths =
    List.fold_left_map parse (max_int, min_int, min_int) lines
  in
  let state = init_state bounds paths in
  let res = run state |> string_of_int in
  if display then pp state;
  res
