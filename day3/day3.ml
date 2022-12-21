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

let rec process_triple acc = function
  | x :: y :: z :: rest ->
      let intersection =
        CharSet.inter
          (CharSet.inter (process_line x) (process_line y))
          (process_line z)
      in
      let new_acc = acc + (priority @@ CharSet.choose intersection) in
      process_triple new_acc rest
  | _ -> acc

let day _ contents _ =
  let lines = String.trim contents |> String.split_on_char '\n' in
  process_triple 0 lines |> string_of_int
