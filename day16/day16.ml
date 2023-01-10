let time_limit = 26

type bitmap = BMP of int
type id = ID of int

module Graph = Map.Make (struct
  type t = id

  let compare = compare
end)

module Adj = struct
  type t = int * id

  let compare = compare
end

module AdjSet = Set.Make (Adj)

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let bmp_add (ID x) (BMP bmp) = BMP (bmp lor (1 lsl x))
let bmp_mem (ID x) (BMP bmp) = bmp land (1 lsl x) > 0
let bmp_disjoint (BMP b1) (BMP b2) = b1 land b2 = 0

let adj_of_bitmap (BMP bitmap) =
  let rec loop set n =
    let shifted = bitmap lsr n in
    if shifted = 0 then set
    else if shifted land 1 > 0 then loop (AdjSet.add (1, ID n) set) (n + 1)
    else loop set (n + 1)
  in
  loop AdjSet.empty 0

let print_to_dot graph name =
  let oc = open_out (Printf.sprintf "graph_%s.dot" name) in
  Printf.fprintf oc "strict graph {\n";
  Graph.iter
    (fun (ID name) (flow, neighbours) ->
      Printf.fprintf oc "    %d [label=\"%d %d\" style=filled color=%s];\n" name
        name flow
        (if name = 0 then "gold"
        else if flow = 0 then "grey"
        else "forestgreen");
      AdjSet.iter
        (fun (w, ID dst) ->
          Printf.fprintf oc "    %d -- %d [label=%d];\n" name dst w)
        neighbours)
    graph;
  Printf.fprintf oc "}";
  close_out oc

let parse contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let substs = Hashtbl.create (List.length lines) in
  Hashtbl.add substs "AA" (ID 0);
  let cursor = ref 1 in
  List.iter
    (fun line ->
      Scanf.sscanf line "Valve %s has flow rate=%d" (fun name flow ->
          if flow <> 0 then (
            Hashtbl.add substs name (ID !cursor);
            incr cursor)))
    lines;
  let graph =
    List.fold_left
      (fun map line ->
        Scanf.sscanf line
          "Valve %s has flow rate=%d; tunnel%_s lead%_s to valve%_s %s@\n"
          (fun name flow destinations ->
            let id =
              match Hashtbl.find_opt substs name with
              | Some id -> id
              | None ->
                  let i = ID !cursor in
                  Hashtbl.add substs name i;
                  incr cursor;
                  i
            in
            let neighbours =
              String.split_on_char ',' destinations
              |> List.fold_left
                   (fun bitmap neigh ->
                     let neigh = String.trim neigh in
                     let bit =
                       match Hashtbl.find_opt substs neigh with
                       | Some bit -> bit
                       | None ->
                           let i = ID !cursor in
                           Hashtbl.add substs neigh i;
                           incr cursor;
                           i
                     in
                     bmp_add bit bitmap)
                   (BMP 0)
            in
            Graph.add id (flow, neighbours) map))
      Graph.empty lines
  in
  graph

let prune graph =
  let weighted =
    Graph.map (fun (flow, neighs) -> (flow, adj_of_bitmap neighs)) graph
  in
  let rec loop graph =
    let broken_valves =
      Graph.filter (fun name (flow, _) -> flow = 0 && name <> ID 0) graph
    in
    if Graph.is_empty broken_valves then graph
    else
      (* Choose a valve worth 0 at random. *)
      let rm_name, (_, rm_neighs) = Graph.choose broken_valves in
      let new_g =
        (* Create a new graph without this valve. *)
        AdjSet.fold
          (fun (w, dst) g ->
            (* Update all neighbours of the chosen valve to point to their metas. *)
            Graph.update dst
              (function
                | None ->
                    failwith
                      (Printf.sprintf "Couldn't find %d in intermediate graph"
                         (let (ID dst) = dst in
                          dst))
                | Some (flow, neighs) ->
                    let neighs' =
                      neighs
                      (* We remove the chosen valve from this valve's neighbours *)
                      |> AdjSet.remove (w, rm_name)
                      (* We add the neighbours of the chosen valve *)
                      |> AdjSet.union
                           (rm_neighs
                           (* Excluding itself *)
                           |> AdjSet.remove (w, dst)
                           |> AdjSet.map
                                (* Without forgetting to increment the weight of the edges *)
                                (fun (old_w, id) -> (old_w + w, id)))
                    in
                    Some (flow, neighs'))
              g)
          rm_neighs
          (* Here we remove the useless valve at the toplevel *)
          (Graph.remove rm_name graph)
      in
      loop new_g
  in
  loop weighted

let to_adj_matrix graph =
  let nb_nodes = Graph.cardinal graph in
  let grid = Array.make_matrix nb_nodes nb_nodes None in
  Graph.iter
    (fun (ID x) (flow, neighbours) ->
      grid.(x).(x) <- Some flow;
      AdjSet.iter (fun (w, ID y) -> grid.(x).(y) <- Some w) neighbours)
    graph;
  grid

let fill_all_pairs arr =
  let nb_nodes = Array.length arr in
  let vertices = Seq.ints 0 |> Seq.take nb_nodes |> IntSet.of_seq in
  for start_node = 0 to nb_nodes - 1 do
    (* We start Dijkstra here, so once for each node. *)
    let rec loop u dist_u q =
      for v = 0 to nb_nodes - 1 do
        match arr.(u).(v) with
        | Some d_uv when IntSet.mem v q -> (
            let alt = dist_u + d_uv in
            match arr.(start_node).(v) with
            | Some prev when alt >= prev -> ()
            | _ -> arr.(start_node).(v) <- Some alt)
        | _ -> ()
      done;
      if not @@ IntSet.is_empty q then
        let new_u, new_dist_u =
          IntSet.fold
            (fun v (curr, dist) ->
              match arr.(start_node).(v) with
              | Some dist_v when dist_v < dist -> (v, dist_v)
              | _ -> (curr, dist))
            q (-1, max_int)
        in
        loop new_u new_dist_u (IntSet.remove new_u q)
    in
    loop start_node 0 (IntSet.remove start_node vertices)
  done;
  for src = 1 to nb_nodes - 1 do
    arr.(src).(0) <- None
  done

type state = {
  position : int;
  time : int;
  score : int;
  flow : int;
  opened : bitmap;
}

let all_opened opened arr =
  let n = Array.length arr in
  let bmp = BMP ((1 lsl n) - 2) in
  opened = bmp

let next_states graph ({ position; time; score; flow; opened } as state)
    is_small =
  let to_wait = time_limit - time in
  let just_wait =
    { state with time = time_limit; score = score + (flow * to_wait) }
  in

  if all_opened opened graph then (* No more valves to open *)
    [ just_wait ]
  else
    let rec filter_neighbours l dst =
      if position = dst || bmp_mem (ID dst) opened then
        filter_neighbours l (dst + 1)
      else if dst >= Array.length graph then
        match l with
        | [] ->
            (* No more openable valves reachable in time *)
            [ just_wait ]
        | _ when is_small -> just_wait :: l
        | _ -> l
      else
        match graph.(position).(dst) with
        | Some w when time + w + 1 < time_limit ->
            filter_neighbours
              ({
                 position = dst;
                 time = time + w + 1;
                 score = score + (flow * (w + 1));
                 flow = flow + Option.get graph.(dst).(dst);
                 opened = bmp_add (ID dst) opened;
               }
              :: l)
              (dst + 1)
        | _ -> filter_neighbours l (dst + 1)
    in
    filter_neighbours [] 0

let brute graph init =
  let q = Queue.create () in
  Queue.add init q;
  let results = Hashtbl.create 3205 in
  let is_small = Array.length graph < 10 in

  let disjoint () =
    let s = Hashtbl.to_seq results in
    let rec loop res s1 s2 =
      match (Seq.uncons s1, Seq.uncons s2) with
      | None, _ -> res
      | Some (_, xs), None -> loop res xs xs
      | Some ((bmp1, score1), _), Some ((bmp2, score2), ys) ->
          if bmp_disjoint bmp1 bmp2 then loop (max res (score1 + score2)) s1 ys
          else loop res s1 ys
    in
    loop 0 s s
  in

  let rec aux () =
    match Queue.take_opt q with
    | None -> disjoint ()
    | Some st ->
        if st.time = time_limit then
          match Hashtbl.find_opt results st.opened with
          | None -> Hashtbl.add results st.opened st.score
          | Some score -> Hashtbl.replace results st.opened (max st.score score)
        else List.iter (Fun.flip Queue.add q) (next_states graph st is_small);
        aux ()
  in
  aux ()

let day display contents _pool =
  let graph = contents |> parse |> prune in
  if display then (
    print_to_dot graph "pruned";
    Printf.printf "\nGraph was printed in a graph_pruned.dot file");
  let matrix = to_adj_matrix graph in
  fill_all_pairs matrix;
  let init = { position = 0; time = 0; score = 0; flow = 0; opened = BMP 0 } in
  brute matrix init |> string_of_int
