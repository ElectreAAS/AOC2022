type cell = {
  content : char;
  shortest_path_cost : int option;
  heuristic_distance : int;
}

let code { content; _ } =
  match content with 'S' -> 97 | 'E' -> 122 | c -> Char.code c

let parse contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let width = String.length @@ List.hd lines in
  let height = List.length lines in
  let hill =
    Array.init width (fun _ ->
        Array.make height
          {
            content = ' ';
            shortest_path_cost = None;
            heuristic_distance = max_int;
          })
  in
  let start = ref None in
  let finish = ref None in
  List.iteri
    (fun y line ->
      String.iteri
        (fun x c ->
          let cell =
            match (c, !finish) with
            | 'S', Some (fx, fy) ->
                start := Some (x, y);
                {
                  content = c;
                  shortest_path_cost = Some 0;
                  heuristic_distance = max 25 (abs (fx - x) + abs (fy - y));
                }
            | 'S', None ->
                start := Some (x, y);
                {
                  content = c;
                  shortest_path_cost = Some 0;
                  heuristic_distance = 25;
                }
            | 'E', _ ->
                finish := Some (x, y);
                {
                  content = c;
                  shortest_path_cost = None;
                  heuristic_distance = 0;
                }
            | _, Some (fx, fy) ->
                {
                  content = c;
                  shortest_path_cost = None;
                  heuristic_distance =
                    max
                      (Char.code 'z' - Char.code c)
                      (abs (fx - x) + abs (fy - y));
                }
            | _, None ->
                {
                  content = c;
                  shortest_path_cost = None;
                  heuristic_distance = Char.code 'z' - Char.code c;
                }
          in

          hill.(x).(y) <- cell)
        line)
    lines;
  let start = Option.get !start in
  let fx, fy = Option.get !finish in
  let rec fill_heuristic x y =
    if x = width then fill_heuristic 0 (y + 1)
    else
      let curr = hill.(x).(y) in
      match curr.content with
      | 'E' -> (hill, start)
      | 'S' ->
          hill.(x).(y) <-
            { curr with heuristic_distance = max 25 (abs (fx - x) + fy - y) };
          fill_heuristic (x + 1) y
      | _ ->
          hill.(x).(y) <-
            {
              curr with
              heuristic_distance =
                max curr.heuristic_distance (abs (fx - x) + fy - y);
            };
          fill_heuristic (x + 1) y
  in
  fill_heuristic 0 0

let pp hill =
  let print_cell { heuristic_distance; _ } =
    Printf.printf "%2d" heuristic_distance
  in
  let width = Array.length hill in
  let height = Array.length hill.(0) in
  print_newline ();
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      print_cell hill.(x).(y)
    done;
    print_newline ()
  done

let a_star hill start =
  let width = Array.length hill in
  let height = Array.length hill.(0) in
  let bin_nb = 6 in
  let bins = Array.init bin_nb (fun _ -> Queue.create ()) in
  let cursor = ref 0 in
  Queue.add start bins.(0);

  let neighbors x y =
    List.filter_map
      (fun (nx, ny) ->
        if nx < 0 || ny < 0 || nx = width || ny = height then None
        else if code hill.(x).(y) - code hill.(nx).(ny) >= -1 then Some (nx, ny)
        else None)
      [ (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y) ]
  in

  (* This function assumes we will always have a next node to visit.
     This is guaranteed by the A* algorithm as long as the end node is accessible. *)
  let rec take_next () =
    match Queue.take_opt bins.(!cursor) with
    | Some next -> next
    | None ->
        cursor := (!cursor + 1) mod bin_nb;
        take_next ()
  in
  let rec loop () =
    let x, y = take_next () in
    let curr = hill.(x).(y) in
    if curr.heuristic_distance = 0 then
      (* This is the final cell! *)
      Option.get curr.shortest_path_cost
    else (
      List.iter
        (fun (nx, ny) ->
          let neigh = hill.(nx).(ny) in
          let proposed_dist = Option.get curr.shortest_path_cost + 1 in
          match neigh.shortest_path_cost with
          | Some cost when proposed_dist >= cost -> ()
          | _ ->
              (* Found a better path! *)
              hill.(nx).(ny) <-
                { neigh with shortest_path_cost = Some proposed_dist };
              let fscore_diff =
                neigh.heuristic_distance - curr.heuristic_distance + 1
              in
              assert (fscore_diff >= 0 && fscore_diff < bin_nb);
              let target_bin = (!cursor + fscore_diff) mod bin_nb in
              Queue.add (nx, ny) bins.(target_bin))
        (neighbors x y);
      loop ())
  in
  loop ()

let day display contents =
  let hill, start = parse contents in
  if display then pp hill;
  a_star hill start |> string_of_int
