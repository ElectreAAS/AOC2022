open Extensions

type node = Directory of int * string * node array | File of int * string

let pp = function
  | File _ ->
      invalid_arg
        "Don't call this function on anything other than the root please"
  | Directory (size, _, children) ->
      let open Notty in
      let rec loop offset node is_last =
        let pipe = I.string A.(fg green) (if is_last then "└──" else "├──") in
        match node with
        | File (size, name) ->
            I.(offset <|> pipe <|> strf ~attr:A.(fg gold) " %s %d" name size)
        | Directory (size, name, children) ->
            let self =
              I.(
                offset <|> pipe
                <|> strf ~attr:A.(fg red) " %s (directory of size %d)" name size)
            in
            let offset =
              I.(
                offset
                <|> string A.(fg green) (if is_last then "    " else "│   "))
            in
            let children_arr =
              Array.mapi
                (fun i child ->
                  loop offset child (i = Array.length children - 1))
                children
            in
            Array.fold_left I.( <-> ) self children_arr
      in
      let root =
        I.(void 0 1 <-> strf ~attr:A.(fg green) "/ (root of size %d)" size)
      in
      let children_arr =
        Array.mapi
          (fun i child -> loop I.empty child (i = Array.length children - 1))
          children
      in
      Array.fold_left I.( <-> ) root children_arr |> Notty_unix.output_image

let parse_node line =
  match Scanf.sscanf_opt line "%d %s" (fun left right -> (left, right)) with
  | Some (size, name) -> File (size, name)
  | None ->
      let name = Scanf.sscanf line "dir %s" Fun.id in
      Directory (0, name, [||])

type state = { pwd : string; tree : node }

let split_prefix path =
  match String.index_opt path '/' with
  | None -> (path, "")
  | Some 0 -> ("/", String.sub path 1 (String.length path - 1))
  | Some n ->
      (String.sub path 0 n, String.sub path (n + 1) (String.length path - n - 1))

let add_suffix suffix = function
  | "/" -> "/" ^ suffix
  | pre -> Printf.sprintf "%s/%s" pre suffix

let rec add_children_to node path (dirsize, childarr) =
  match node with
  | File _ -> invalid_arg "Can't add children to a file"
  | Directory (size, name, children) ->
      if name = path then Directory (dirsize, name, childarr)
      else
        let pre, post = split_prefix path in
        assert (name = pre);
        let next, _ = split_prefix post in
        let i = ref (-1) in
        Array.iteri
          (fun n node ->
            match node with
            | Directory (_, childname, _) when childname = next -> i := n
            | _ -> ())
          children;
        if !i = -1 then
          failwith
            (Printf.sprintf "cd: The directory '%s' doesn't exist in parent %s"
               next path);
        children.(!i) <- add_children_to children.(!i) post (dirsize, childarr);
        Directory (size + dirsize, name, children)

let parse_tree state cmd_output =
  match Scanf.sscanf_opt cmd_output " cd %s" Fun.id with
  | Some "/" -> { state with pwd = "/" }
  | Some ".." ->
      let pwd = String.sub state.pwd 0 (String.rindex state.pwd '/') in
      { state with pwd }
  | Some dir ->
      let pwd = add_suffix dir state.pwd in
      { state with pwd }
  | None ->
      let lines =
        cmd_output |> String.split_on_char '\n' |> List.filter (( <> ) "")
      in
      assert (List.hd lines = " ls");
      let dirsize, children =
        Array.fold_left_map
          (fun sum line ->
            match parse_node line with
            | File (size, _) as node -> (sum + size, node)
            | node -> (sum, node))
          0
          (List.tl lines |> Array.of_list)
      in
      let tree = add_children_to state.tree state.pwd (dirsize, children) in
      { state with tree }

let sum_of_smalls node =
  let rec loop sum = function
    | File _ -> sum
    | Directory (size, _, children) ->
        Array.fold_left loop
          (if size <= 100_000 then size + sum else sum)
          children
  in
  loop 0 node

let size = function File (size, _) -> size | Directory (size, _, _) -> size

let find_smallest_big mark node =
  let rec loop curr = function
    | File _ -> curr
    | Directory (size, _, children) ->
        if size < mark then curr
        else Array.fold_left loop (min curr size) children
  in
  loop max_int node

let day display _ input_buffer =
  let cmds =
    input_buffer |> Eio.Buf_read.take_all |> String.split_on_char '$' |> List.tl
  in
  let state = { pwd = ""; tree = Directory (0, "/", [||]) } in
  let final_state = List.fold_left parse_tree state cmds in
  if display then pp final_state.tree;
  let needed_free_space = 40_000_000 in
  let taken_space = size final_state.tree in
  let space_to_free = taken_space - needed_free_space in
  find_smallest_big space_to_free final_state.tree |> string_of_int
