open Extensions

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
  let open Notty in
  let grid_w = Array.length state.grid in
  let grid_h = Array.length state.grid.(0) in
  let pipe = I.string A.empty "│" in
  let left_wall = vmul (I.hpad 0 1 pipe) grid_h in
  let right_wall = vmul (I.hpad 1 0 pipe) grid_h in
  let flat = hmul (I.string A.empty "─") (grid_w + 2) in
  let top_left = I.string A.empty "┌" in
  let top_right = I.string A.empty "┐" in
  let top = I.(top_left <|> flat <|> top_right) in
  let with_arr =
    I.(pipe <|> hsnap (grid_w + 2) (string A.empty "⇣") <|> pipe)
  in
  let main =
    I.tabulate grid_w grid_h @@ fun x y ->
    match state.grid.(x).(y) with
    | Rock -> I.string (A.fg A.green) "█"
    | Air -> I.void 1 0
    | Sand ->
        I.string (A.fg A.gold)
          (match (state.grid.(x - 1).(y), state.grid.(x + 1).(y)) with
          | Air, Air -> "◬"
          | Air, _ -> "◢"
          | _, Air -> "◣"
          | _, _ -> "█")
  in
  let empty_line = I.(pipe <|> void (grid_w + 2) 0 <|> pipe) in
  let bot_left = I.string A.empty "└" in
  let bot_right = I.string A.empty "┘" in
  let bot = I.(bot_left <|> flat <|> bot_right) in
  I.(
    void 0 1 <-> top <-> with_arr
    <-> (left_wall <|> main <|> right_wall)
    <-> empty_line <-> bot)
  |> Notty_unix.output_image

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

let day display _ input_buffer =
  let lines = Eio.Buf_read.lines input_buffer |> List.of_seq in
  (* FIXME: use Eio better *)
  let max_y, paths = List.fold_left_map parse min_int lines in
  let state = init_state max_y paths in
  let res = run state |> string_of_int in
  if display then pp state;
  res
