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

let consume seq =
  let rec aux is_left seq l =
    match Seq.uncons seq with
    | Some ("", rest) -> aux true rest l
    | None -> List.rev l
    | Some (line, rest) -> (
        match (is_left, l) with
        | true, _ ->
            let elem = (Eio.Buf_read.of_string line |> parse_data, Int 0) in
            aux false rest (elem :: l)
        | false, (x, _) :: xs ->
            let elem = (x, Eio.Buf_read.of_string line |> parse_data) in
            aux true rest (elem :: xs)
        | _ -> failwith "Can't be right at this point")
  in
  aux true seq []

type order = Greater | Equal | Lesser

let is_correct (left, right) =
  let rec loop left right =
    match (left, right) with
    | Int l, Int r when l = r -> Equal
    | Int l, Int r when l < r -> Lesser
    | Int _, Int _ -> Greater
    | List (l :: lxs), List (r :: rxs) -> (
        match loop l r with Equal -> loop (List lxs) (List rxs) | ord -> ord)
    | Int _, List _ -> loop (List [ left ]) right
    | List _, Int _ -> loop left (List [ right ])
    | List [], List (_ :: _) -> Lesser
    | List (_ :: _), List [] -> Greater
    | List [], List [] -> Equal
  in
  match loop left right with
  | Lesser -> true
  | Greater -> false
  | Equal -> failwith "Supposedly can't happen"

let day _display contents =
  let buff = Eio.Buf_read.of_string contents in
  let lines = Eio.Buf_read.lines buff in
  let pairs = consume lines in
  let rec loop sum i = function
    | [] -> sum
    | pair :: rest ->
        let new_sum = if is_correct pair then sum + i else sum in
        loop new_sum (i + 1) rest
  in
  loop 0 1 pairs |> string_of_int
