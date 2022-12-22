open Extensions

module PairSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type state = { chain : (int * int) array; visited : PairSet.t }

let pp state =
  let minx, maxx, miny, maxy =
    Array.fold_left
      (fun (minx, maxx, miny, maxy) (x, y) ->
        (min minx x, max maxx x, min miny y, max maxy y))
      (max_int, min_int, max_int, min_int)
      state.chain
  in
  let minx, maxx, miny, maxy =
    PairSet.fold
      (fun (x, y) (minx, maxx, miny, maxy) ->
        (min minx x, max maxx x, min miny y, max maxy y))
      state.visited (minx, maxx, miny, maxy)
  in
  print_newline ();
  for y = miny to maxy do
    for x = minx to maxx do
      let str =
        match Array.findi_opt (( = ) (x, y)) state.chain with
        | Some 0 -> "H"
        | Some i -> string_of_int i
        | None ->
            if (x, y) = (0, 0) then "s"
            else if PairSet.mem (x, y) state.visited then "▒"
            else " "
      in
      print_string str
    done;
    print_newline ()
  done;
  ()

type move = Up | Down | Left | Right

let parse_move str =
  Scanf.sscanf str "%c %d" (fun c n ->
      ((match c with 'U' -> Up | 'D' -> Down | 'L' -> Left | _ -> Right), n))

let char_of_move = function
  | Up -> 'U'
  | Down -> 'D'
  | Left -> 'L'
  | Right -> 'R'

(* 88466
 * 80006
 * 20T01
 * 70005
 * 77355 *)
let rec post_head state i =
  let hx, hy = state.chain.(i) and tx, ty = state.chain.(i + 1) in
  let new_x, new_y =
    match (hx - tx, hy - ty) with
    | (1 | 0 | -1), (1 | 0 | -1) -> (tx, ty) (* 0 *)
    | 2, 0 -> (tx + 1, ty) (* 1 *)
    | -2, 0 -> (tx - 1, ty) (* 2 *)
    | 0, 2 -> (tx, ty + 1) (* 3 *)
    | 0, -2 -> (tx, ty - 1) (* 4 *)
    | 2, 1 | 1, 2 | 2, 2 -> (tx + 1, ty + 1) (* 5 *)
    | 2, -1 | 1, -2 | 2, -2 -> (tx + 1, ty - 1) (* 6 *)
    | -2, 1 | -1, 2 | -2, 2 -> (tx - 1, ty + 1) (* 7 *)
    | -2, -1 | -1, -2 | -2, -2 -> (tx - 1, ty - 1) (* 8 *)
    | dx, dy ->
        failwith
          (Printf.sprintf "Unknown movement of (%d,%d) from knot %d" dx dy i)
  in
  if (new_x, new_y) = (tx, ty) then state
  else (
    state.chain.(i + 1) <- (new_x, new_y);
    if i = 8 then
      {
        chain = state.chain;
        visited = PairSet.add (new_x, new_y) state.visited;
      }
    else post_head state (i + 1))

let rec step state move n =
  if n = 0 then state
  else
    let x, y = state.chain.(0) in
    let head =
      match move with
      | Up -> (x, y - 1)
      | Down -> (x, y + 1)
      | Left -> (x - 1, y)
      | Right -> (x + 1, y)
    in
    state.chain.(0) <- head;
    let new_state = post_head state 0 in
    step new_state move (n - 1)

let day display contents _ =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let final_state =
    List.fold_left
      (fun state line ->
        let move, n = parse_move line in
        step state move n)
      {
        chain =
          [|
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
            (0, 0);
          |];
        visited = PairSet.singleton (0, 0);
      }
      lines
  in
  if display then pp final_state;
  PairSet.cardinal final_state.visited |> string_of_int
