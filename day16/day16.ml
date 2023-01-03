let time_limit = 30

module Str = struct
  type t = string

  let compare = compare
end

module Graph = Map.Make (Str)
module StrSet = Set.Make (Str)

module AdjSet = Set.Make (struct
  type t = int * string

  let compare = compare
end)

let print_to_dot graph name =
  let oc = open_out (Printf.sprintf "graph_%s.dot" name) in
  Printf.fprintf oc "strict graph {\n";
  Graph.iter
    (fun name (flow, neighbours) ->
      Printf.fprintf oc "    %s [label=\"%s %d\" style=filled color=%s];\n" name
        name flow
        (if name = "AA" then "gold"
        else if flow = 0 then "grey"
        else "forestgreen");
      AdjSet.iter
        (fun (w, dst) ->
          Printf.fprintf oc "    %s -- %s [label=%d];\n" name dst w)
        neighbours)
    graph;
  Printf.fprintf oc "}";
  close_out oc

let pp graph =
  Printf.fprintf stdout "{\n";
  Graph.iter
    (fun name (flow, neighbours) ->
      Printf.fprintf stdout "  %s: %d, #{" name flow;
      AdjSet.iter
        (fun (w, dst) -> Printf.fprintf stdout " (%d, %s)" w dst)
        neighbours;
      Printf.fprintf stdout " }\n")
    graph;
  Printf.fprintf stdout "}\n%!"

let parse contents =
  let lines = String.trim contents |> String.split_on_char '\n' in
  List.fold_left
    (fun map line ->
      Scanf.sscanf line
        "Valve %s has flow rate=%d; tunnel%_s lead%_s to valve%_s %s@\n"
        (fun name flow destinations ->
          let neighbours =
            String.split_on_char ',' destinations
            |> List.fold_left
                 (fun s name -> StrSet.add (String.trim name) s)
                 StrSet.empty
          in
          Graph.add name (flow, neighbours) map))
    Graph.empty lines

let prune graph =
  let weighted =
    Graph.map
      (fun (flow, neighs) ->
        ( flow,
          StrSet.fold (fun n adj -> AdjSet.add (1, n) adj) neighs AdjSet.empty
        ))
      graph
  in
  let rec loop n graph =
    (* print_to_dot graph (string_of_int n); *)
    let broken_valves =
      Graph.filter (fun name (flow, _) -> flow = 0 && name <> "AA") graph
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
                      (Printf.sprintf "Couldn't find %s in intermediate graph"
                         dst)
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
                                (fun (w', dst') -> (w' + w, dst')))
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

let maximal_flow graph = Graph.fold (fun _ (flow, _) sum -> flow + sum) graph 0

type state = {
  position : string;
  time : int;
  score : int;
  flow : int;
  opened : StrSet.t;
  visited : StrSet.t;
  trace : string;
}

let next_states graph
    ({ position; time; score; flow; opened; visited; trace } as state) =
  if Graph.for_all (fun name (f, _) -> f = 0 || StrSet.mem name opened) graph
  then
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
        (fun (w, dst) l ->
          if StrSet.mem dst visited || time + w >= time_limit then l
          else
            {
              state with
              score = score + (flow * w);
              time = time + w;
              position = dst;
              visited = StrSet.add dst visited;
              trace = Printf.sprintf "%s\nMoved to valve %s" trace dst;
            }
            :: l)
        neighbours []
    in
    if this_flow = 0 || StrSet.mem position opened then
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
        opened = StrSet.add position opened;
        flow = flow + this_flow;
        visited = StrSet.singleton position;
        trace = Printf.sprintf "%s\nOpened valve %s" trace position;
      }
      :: filtered_neighbours

let brute display graph init =
  let q = Queue.create () in
  Queue.add init q;

  let rec aux max_score trace =
    match Queue.take_opt q with
    | None -> (max_score, trace)
    | Some st ->
        if st.time = time_limit then
          if st.score > max_score then aux st.score st.trace
          else aux max_score trace
        else (
          List.iter (fun s -> Queue.add s q) (next_states graph st);
          aux max_score trace)
  in
  let score, trace = aux 0 "" in
  if display then print_endline trace;
  score

let day display contents _pool =
  let graph = parse contents |> prune in
  let init =
    {
      position = "AA";
      time = 0;
      score = 0;
      flow = 0;
      opened = StrSet.empty;
      visited = StrSet.singleton "AA";
      trace = "";
    }
  in
  brute display graph init |> string_of_int
