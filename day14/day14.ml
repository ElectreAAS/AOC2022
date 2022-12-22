type path = (int * int) list

let parse max_y line =
  let chunks = String.split_on_char '-' line in
  let pathize max_y str =
    let no_arr =
      if String.starts_with ~prefix:">" str then
        String.sub str 2 (String.length str - 2)
      else str
    in
    Scanf.sscanf no_arr "%d,%d" (fun x y -> (max max_y y, (x, y)))
  in
  List.fold_left_map pathize max_y chunks

type cell = Rock | Sand | Air
type state = { grid : cell array array; middle : int }

let sand_pour = 500

let init_state max_y (paths : path list) =
  let width = (2 * max_y) + 5 in
  let middle = width / 2 in
  let height = max_y + 3 in
  let grid = Array.make_matrix width height Air in
  let rec draw_path = function
    | [] | [ _ ] -> ()
    | (lx, ly) :: (rx, ry) :: rest when lx = rx ->
        let down, up = if ly < ry then (ly, ry) else (ry, ly) in
        for y = down to up do
          grid.(lx + middle - sand_pour).(y) <- Rock
        done;
        draw_path ((rx, ry) :: rest)
    | (lx, ly) :: (rx, ry) :: rest ->
        assert (ly = ry);
        let down, up = if lx < rx then (lx, rx) else (rx, lx) in
        for x = down to up do
          grid.(x + middle - sand_pour).(ly) <- Rock
        done;
        draw_path ((rx, ry) :: rest)
  in
  List.iter draw_path paths;
  for x = 0 to width - 1 do
    grid.(x).(max_y + 2) <- Rock
  done;
  { grid; middle }

let pp state =
  let open Colours in
  let width = Array.length state.grid - 1 in
  let height = Array.length state.grid.(0) - 1 in
  print_string "\n┌";
  for _ = 0 to width + 2 do
    print_string "─"
  done;
  print_string "┐\n│";
  for x = -1 to width + 1 do
    if x = state.middle then print_string "⇣" else print_char ' '
  done;
  print_string "│\n";
  for y = 0 to height do
    print_string "│ ";
    for x = 0 to width do
      match state.grid.(x).(y) with
      | Rock -> Printf.printf "%s█" green
      | Air -> print_char ' '
      | Sand ->
          Printf.printf "%s%s" gold
            (match (state.grid.(x - 1).(y), state.grid.(x + 1).(y)) with
            | Air, Air -> "◬"
            | Air, _ -> "◢"
            | _, Air -> "◣"
            | _, _ -> "█")
    done;
    Printf.printf "%s │\n" reset
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

let run state =
  let rec pour_from x y path =
    match
      ( state.grid.(x - 1).(y + 1),
        state.grid.(x).(y + 1),
        state.grid.(x + 1).(y + 1) )
    with
    | _, Air, _ -> pour_from x (y + 1) ((x, y) :: path)
    | Air, _, _ -> pour_from (x - 1) (y + 1) ((x, y) :: path)
    | _, _, Air -> pour_from (x + 1) (y + 1) ((x, y) :: path)
    | _ -> (x, y) :: path
  in
  let rec loop n path =
    let x, y, xs =
      match path with [] -> (state.middle, 0, []) | (x, y) :: xs -> (x, y, xs)
    in
    match pour_from x y xs with
    | [] -> failwith "Path should never be empty after pouring"
    | (x, 0) :: _ when x = state.middle ->
        state.grid.(x).(0) <- Sand;
        n
    | (x, y) :: xs ->
        state.grid.(x).(y) <- Sand;
        loop (n + 1) xs
  in
  loop 1 []

let day display contents _ =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let max_y, paths = List.fold_left_map parse min_int lines in
  let state = init_state max_y paths in
  let res = run state |> string_of_int in
  if display then pp state;
  res
