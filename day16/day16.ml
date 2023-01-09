let time_limit = 26

type bitmap = BMP of int
type id = ID of int

module Graph = Map.Make (struct
  type t = id

  let compare = compare
end)

module Adj = struct
  type t = { w : int; dst : id }

  let compare = compare
end

module AdjSet = Set.Make (Adj)

let bmp_add (ID x) (BMP bmp) = BMP (bmp lor (1 lsl x))
let bmp_mem (ID x) (BMP bmp) = bmp land (1 lsl x) > 0
let bmp_singleton (ID x) = BMP (1 lsl x)
let bmp_disjoint (BMP b1) (BMP b2) = b1 land b2 = 0

let adj_of_bitmap (BMP bitmap) =
  let rec loop set n =
    let shifted = bitmap lsr n in
    if shifted = 0 then set
    else if shifted land 1 > 0 then
      loop (AdjSet.add { w = 1; dst = ID n } set) (n + 1)
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
        (fun { w; dst = ID dst } ->
          Printf.fprintf oc "    %d -- %d [label=%d];\n" name dst w)
        neighbours)
    graph;
  Printf.fprintf oc "}";
  close_out oc

let pp graph =
  Printf.fprintf stdout "{\n";
  Graph.iter
    (fun (ID name) (flow, neighbours) ->
      Printf.fprintf stdout "  %d: %d, #{" name flow;
      AdjSet.iter
        (fun { w; dst = ID dst } -> Printf.fprintf stdout " (%d, %d)" w dst)
        neighbours;
      Printf.fprintf stdout " }\n")
    graph;
  Printf.fprintf stdout "}\n%!"

let parse contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  let substs = Hashtbl.create (List.length lines) in
  let cursor = ref 0 in
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
  (graph, Hashtbl.find substs "AA")

let prune graph aa_id =
  let weighted =
    Graph.map (fun (flow, neighs) -> (flow, adj_of_bitmap neighs)) graph
  in
  let rec loop n graph =
    (* print_to_dot graph (string_of_int n); *)
    let broken_valves =
      Graph.filter (fun name (flow, _) -> flow = 0 && name <> aa_id) graph
    in
    if Graph.is_empty broken_valves then graph
    else
      (* Choose a valve worth 0 at random. *)
      let rm_name, (_, rm_neighs) = Graph.choose broken_valves in
      let new_g =
        (* Create a new graph without this valve. *)
        AdjSet.fold
          (fun { w; dst } g ->
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
                      |> AdjSet.remove { w; dst = rm_name }
                      (* We add the neighbours of the chosen valve *)
                      |> AdjSet.union
                           (rm_neighs
                           (* Excluding itself *)
                           |> AdjSet.remove { w; dst }
                           |> AdjSet.map
                                (* Without forgetting to increment the weight of the edges *)
                                (fun e -> { e with w = w + e.w }))
                    in
                    Some (flow, neighs'))
              g)
          rm_neighs
          (* Here we remove the useless valve at the toplevel *)
          (Graph.remove rm_name graph)
      in
      loop (n + 1) new_g
  in
  loop 0 weighted

type state = {
  position : id;
  time : int;
  score : int;
  flow : int;
  opened : bitmap;
  visited : bitmap;
  trace : string;
}

let next_states graph
    ({ position; time; score; flow; opened; visited; trace } as state) =
  if Graph.for_all (fun name (f, _) -> f = 0 || bmp_mem name opened) graph then
    (* No more valves to open, just wait *)
    let to_wait = time_limit - time in
    [
      {
        state with
        time = time_limit;
        score = score + (flow * to_wait);
        trace = Printf.sprintf "%s\nWaited %d minutes" trace to_wait;
      };
    ]
  else
    let this_flow, neighbours = Graph.find position graph in
    let filtered_neighbours =
      AdjSet.fold
        (fun { w; dst } l ->
          if bmp_mem dst visited || time + w >= time_limit then l
          else
            {
              state with
              score = score + (flow * w);
              time = time + w;
              position = dst;
              visited = bmp_add dst visited;
              trace =
                Printf.sprintf "%s\nMoved to valve %d" trace
                  (let (ID dst) = dst in
                   dst);
            }
            :: l)
        neighbours []
    in
    if this_flow = 0 || bmp_mem position opened then
      match filtered_neighbours with
      | [] ->
          (* No more openable valves reachable in time, just wait *)
          let to_wait = time_limit - time in
          [
            {
              state with
              time = time_limit;
              score = score + (flow * to_wait);
              trace =
                Printf.sprintf
                  "%s\nCouldn't reach anymore valves, waited %d minutes" trace
                  to_wait;
            };
          ]
      | _ -> filtered_neighbours
    else
      {
        state with
        score = score + flow;
        time = time + 1;
        opened = bmp_add position opened;
        flow = flow + this_flow;
        visited = bmp_singleton position;
        trace =
          Printf.sprintf "%s\nOpened valve %d" trace
            (let (ID pos) = position in
             pos);
      }
      :: filtered_neighbours

let brute graph init =
  let q = Queue.create () in
  Queue.add init q;
  let results = Hashtbl.create 5000 in

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
        if st.time = time_limit then (
          (match Hashtbl.find_opt results st.opened with
          | None -> Hashtbl.add results st.opened st.score
          | Some score -> Hashtbl.replace results st.opened (max st.score score));
          aux ())
        else (
          List.iter (fun s -> Queue.add s q) (next_states graph st);
          aux ())
  in
  aux ()

let day _display contents _pool =
  let big_graph, aa_id = parse contents in
  let graph = prune big_graph aa_id in
  let init =
    {
      position = aa_id;
      time = 0;
      score = 0;
      flow = 0;
      opened = BMP 0;
      visited = bmp_singleton aa_id;
      trace = "";
    }
  in
  brute graph init |> string_of_int
