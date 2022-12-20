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
type state = { grid : cell array array; pour : int }

let sand_pour = 500

let init_state (minx, maxx, maxy) (paths : path list) =
  let width = maxx - minx + 1 in
  let height = maxy + 1 in
  let grid = Array.init width (fun _ -> Array.init height (Fun.const Air)) in
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
  print_string "┘\n"

let loop state =
  let rec pour_opt x y =
    match
      ( state.grid.(x - 1).(y + 1),
        state.grid.(x).(y + 1),
        state.grid.(x + 1).(y + 1) )
    with
    | exception Invalid_argument _ -> None
    | _, Air, _ -> pour_opt x (y + 1)
    | Air, _, _ -> pour_opt (x - 1) (y + 1)
    | _, _, Air -> pour_opt (x + 1) (y + 1)
    | _ -> Some (x, y)
  in
  let rec aux n =
    match pour_opt state.pour 0 with
    | Some (x, y) ->
        state.grid.(x).(y) <- Sand;
        aux (n + 1)
    | None -> n
  in
  aux 0

let day display contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let bounds, paths =
    List.fold_left_map parse (max_int, min_int, min_int) lines
  in
  let state = init_state bounds paths in
  let res = loop state |> string_of_int in
  if display then pp state;
  res
