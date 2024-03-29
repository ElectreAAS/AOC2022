module EBR = Eio.Buf_read
open EBR.Syntax

type sensor = { x : int; y : int; bx : int; by : int }

let parse line =
  Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
    (fun x y bx by -> { x; y; bx; by })

let dist { x; y; bx; by } = abs (x - bx) + abs (y - by)

type interval = Mono of int * int | Poly of (int * int) list

let pp i =
  let rec pp_list = function
    | [] -> print_newline ()
    | [ (x, y) ] -> Printf.printf "[%d … %d]\n" x y
    | (x, y) :: xs ->
        Printf.printf "[%d … %d] ∪ " x y;
        pp_list xs
  in
  match i with
  | Mono (x, y) -> Printf.printf "[%d … %d]\n" x y
  | Poly l ->
      print_string "\nFound a suitable interval: ";
      pp_list l

let rec merge left right =
  let rec aux (low, high) = function
    | [] -> [ (low, high) ]
    | (x, y) :: xs -> (
        match merge (Mono (low, high)) (Mono (x, y)) with
        | Mono (a, b) -> aux (a, b) xs
        | Poly ((a, b) :: _) when (a, b) = (low, high) ->
            (low, high) :: (x, y) :: xs
        | Poly _ -> (x, y) :: aux (low, high) xs)
  in
  match (left, right) with
  | Mono (llow, lhigh), Mono (rlow, rhigh) -> (
      match compare llow rlow with
      | 0 -> Mono (llow, max lhigh rhigh)
      | -1 ->
          if lhigh >= rlow - 1 then Mono (llow, max lhigh rhigh)
          else Poly [ (llow, lhigh); (rlow, rhigh) ]
      | _ ->
          if rhigh >= llow - 1 then Mono (rlow, max lhigh rhigh)
          else Poly [ (rlow, rhigh); (llow, lhigh) ])
  | Mono (low, high), Poly l | Poly l, Mono (low, high) -> (
      match aux (low, high) l with [ (x, y) ] -> Mono (x, y) | l -> Poly l)
  | Poly l, Poly _ ->
      List.fold_left
        (fun i pair ->
          match (i, pair) with
          | Mono _, (x, y) -> merge i (Mono (x, y))
          | Poly l, (x, y) -> (
              match aux (x, y) l with [ (x, y) ] -> Mono (x, y) | l -> Poly l))
        right l

let difference big small =
  let rec aux (low, high) small =
    match small with
    | Mono (x, y) when x = low && y = high -> None
    | Mono (x, y) when x = low -> Some (y + 1, high)
    | Mono (x, y) when y = high -> Some (low, x - 1)
    | Mono _ -> failwith "Can this happen?"
    | Poly l ->
        List.fold_left
          (fun big (x, y) -> Option.bind big (fun big -> aux big (Mono (x, y))))
          (Some (low, high))
          l
  in
  match aux big small with Some (x, y) -> Mono (x, y) | None -> Poly []

let single_opt = function
  | (Mono (x, y) | Poly [ (x, y) ]) when x = y -> Some x
  | _ -> None

let cant_be sensor target size =
  let { x; y; _ } = sensor in
  let manhattan = dist sensor in
  let dy = abs (target - y) in
  let no_go_dist = manhattan - dy in
  if no_go_dist < 0 then None
  else
    let low = x - no_go_dist and high = x + no_go_dist in
    if low > size || high < 0 then None else Some (max 0 low, min size high)

let parse_sensors =
  let+ lines = EBR.lines in
  let len = ref 0 in
  let sensors =
    Seq.map
      (fun x ->
        incr len;
        parse x)
      lines
  in
  (!len, List.of_seq sensors)

module T = Domainslib.Task

let day display pool =
  let+ len, sensors = parse_sensors in
  let size = if len < 20 then 20 else 4_000_000 in
  let search () =
    T.parallel_find pool ~start:0 ~finish:size ~body:(fun y ->
        let interval =
          List.fold_left
            (fun i s ->
              match cant_be s y size with
              | None -> i
              | Some (a, b) -> merge i (Mono (a, b)))
            (Poly []) sensors
        in
        difference (0, size) interval
        |> single_opt
        |> Option.map (fun x ->
               if display then pp interval;
               (x, y)))
  in
  let x, y = search |> T.run pool |> Option.get in
  (x * 4_000_000) + y |> string_of_int
