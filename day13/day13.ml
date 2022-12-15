open Eio.Buf_read.Syntax

type data = Int of int | List of data list

let rec pp = function
  | Int n -> print_int n
  | List l ->
      print_char '[';
      List.iter
        (fun d ->
          pp d;
          print_char ',')
        l;
      print_char ']'

let parse_data buff =
  let parse_num =
    let+ str =
      Eio.Buf_read.take_while (function '0' .. '9' -> true | _ -> false)
    in
    int_of_string str
  in
  let rec parse_list contents =
    match Eio.Buf_read.peek_char buff with
    | Some '[' ->
        Eio.Buf_read.string "[" buff;
        parse_list (parse_list [] :: contents)
    | Some ']' ->
        Eio.Buf_read.string "]" buff;
        List (List.rev contents)
    | Some '0' .. '9' ->
        let n = parse_num buff in
        parse_list (Int n :: contents)
    | Some ',' ->
        Eio.Buf_read.string "," buff;
        parse_list contents
    | Some c -> failwith (Printf.sprintf "Unknown character %c" c)
    | None -> List contents
  in
  parse_list []

let rec compare_data left right =
  match (left, right) with
  | Int _, List _ -> compare_data (List [ left ]) right
  | List _, Int _ -> compare_data left (List [ right ])
  | Int l, Int r -> compare l r
  | List (l :: lxs), List (r :: rxs) -> (
      match compare_data l r with
      | 0 -> compare_data (List lxs) (List rxs)
      | ord -> ord)
  | List l, List r -> compare l r

let divider_2 = List [ List [ Int 2 ] ]
let divider_6 = List [ List [ Int 6 ] ]

let consume seq =
  let rec aux seq l =
    match Seq.uncons seq with
    | Some ("", rest) -> aux rest l
    | None -> List.fast_sort compare_data l
    | Some (line, rest) ->
        let elem = Eio.Buf_read.of_string line |> parse_data in
        aux rest (elem :: l)
  in
  aux seq [ divider_2; divider_6 ]

let day _display contents =
  let buff = Eio.Buf_read.of_string contents in
  let lines = Eio.Buf_read.lines buff in
  let sorted = consume lines in
  let rec loop prod i = function
    | [] -> failwith "Supposedly can't happen"
    | elem :: rest ->
        if elem = divider_2 then loop i (i + 1) rest
        else if elem = divider_6 then prod * i
        else loop prod (i + 1) rest
  in
  loop 1 1 sorted |> string_of_int
