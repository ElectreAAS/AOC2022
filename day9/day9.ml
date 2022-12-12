module PairSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type state = { head : int * int; tail : int * int; visited : PairSet.t }
type move = Up | Down | Left | Right

let parse_move str =
  Scanf.sscanf str "%c %d" (fun c n ->
      ((match c with 'U' -> Up | 'D' -> Down | 'L' -> Left | _ -> Right), n))

(* ·846·
 * 80006
 * 20T01
 * 70005
 * ·735·*)
let post_head state =
  let hx, hy = state.head and tx, ty = state.tail in
  let new_x, new_y =
    match (hx - tx, hy - ty) with
    | (1 | 0 | -1), (1 | 0 | -1) -> (tx, ty) (* 0 *)
    | 2, 0 -> (tx + 1, ty) (* 1 *)
    | -2, 0 -> (tx - 1, ty) (* 2 *)
    | 0, 2 -> (tx, ty + 1) (* 3 *)
    | 0, -2 -> (tx, ty - 1) (* 4 *)
    | 2, 1 | 1, 2 -> (tx + 1, ty + 1) (* 5 *)
    | 2, -1 | 1, -2 -> (tx + 1, ty - 1) (* 6 *)
    | -2, 1 | -1, 2 -> (tx - 1, ty + 1) (* 7 *)
    | -2, -1 | -1, -2 -> (tx - 1, ty - 1) (* 8 *)
    | _ -> (tx, ty)
  in
  {
    head = state.head;
    tail = (new_x, new_y);
    visited = PairSet.add (new_x, new_y) state.visited;
  }

let rec step state move n =
  if n = 0 then state
  else
    let x, y = state.head in
    let head =
      match move with
      | Up -> (x, y - 1)
      | Down -> (x, y + 1)
      | Left -> (x - 1, y)
      | Right -> (x + 1, y)
    in
    let state' = { state with head } in
    let new_state = post_head state' in
    step new_state move (n - 1)

let day _display contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let final_state =
    List.fold_left
      (fun state line ->
        let move, n = parse_move line in
        step state move n)
      { head = (0, 0); tail = (0, 0); visited = PairSet.empty }
      lines
  in
  PairSet.cardinal final_state.visited |> string_of_int
