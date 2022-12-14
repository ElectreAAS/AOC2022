type cell = { content : char; shortest_path_cost : int option }

let code c = match c with 'S' -> 0 | 'E' -> 25 | c -> Char.code c - 97

let parse contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let width = String.length @@ List.hd lines in
  let height = List.length lines in
  let hill =
    Array.init width (fun _ ->
        Array.make height { content = ' '; shortest_path_cost = None })
  in
  let start = ref None in
  List.iteri
    (fun y line ->
      String.iteri
        (fun x c ->
          let shortest_path_cost =
            if c = 'E' then (
              start := Some (x, y);
              Some 0)
            else None
          in
          let cell = { content = c; shortest_path_cost } in
          hill.(x).(y) <- cell)
        line)
    lines;
  (hill, Option.get !start)

let pp hill =
  let print_cell { content; _ } = Printf.printf "%c" content in
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
  (* Max difference between letters. In practice this doesn't get above 3. *)
  let bin_nb = 25 in
  let bins = Array.init bin_nb (fun _ -> Queue.create ()) in
  let cursor = ref 0 in
  Queue.add start bins.(0);

  let neighbors x y =
    List.filter_map
      (fun (nx, ny) ->
        if nx < 0 || ny < 0 || nx = width || ny = height then None
        else if code hill.(nx).(ny).content - code hill.(x).(y).content >= -1
        then Some (nx, ny)
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
    if curr.content = 'a' || curr.content = 'S' then
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
              let fscore_diff = code neigh.content - code curr.content + 1 in
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
