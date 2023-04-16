let priority c =
  let code = Char.code c in
  if code >= 97 then code - 96 else code - 38

module CharSet = Set.Make (struct
  type t = char

  let compare = compare
end)

let process_line line =
  let set = ref CharSet.empty in
  String.iter (fun c -> set := CharSet.add c !set) line;
  !set

let triple_parse =
  let open Eio.Buf_read in
  let open Syntax in
  let* b = at_end_of_input in
  if b then return None
  else
    let+ l = line and+ m = line and+ r = line in
    Some (l, m, r)

let rec process_triple acc input_buffer =
  match triple_parse input_buffer with
  | Some (x, y, z) ->
      let intersection =
        CharSet.inter
          (CharSet.inter (process_line x) (process_line y))
          (process_line z)
      in
      let new_acc = acc + (priority @@ CharSet.choose intersection) in
      process_triple new_acc input_buffer
  | None -> acc

let day _ _ input_buffer = process_triple 0 input_buffer |> string_of_int
