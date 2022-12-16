module EBR = Eio.Buf_read
open EBR.Syntax

type data = Int of int | List of data list

let pp l =
  let rec print_main oc = function
    | Int n -> Printf.fprintf oc "%d" n
    | List l -> Printf.fprintf oc "[%a]" print_contents l
  and print_contents oc = function
    | [] -> ()
    | [ x ] -> print_main oc x
    | x :: xs -> Printf.fprintf oc "%a,%a" print_main x print_contents xs
  in
  print_main stdout l;
  print_newline ()

let parse_data buff =
  let parse_num =
    let+ str = EBR.take_while (function '0' .. '9' -> true | _ -> false) in
    Int (int_of_string str)
  in
  let rec parse_main () =
    EBR.string "[" buff;
    let contents = parse_contents [] in
    EBR.string "]" buff;
    List contents
  and parse_contents so_far =
    match EBR.peek_char buff with
    | Some '[' -> parse_contents (parse_main () :: so_far)
    | Some ']' -> List.rev so_far
    | Some '0' .. '9' ->
        let n = parse_num buff in
        parse_contents (n :: so_far)
    | Some ',' ->
        EBR.string "," buff;
        parse_contents so_far
    | Some c -> failwith (Printf.sprintf "Unknown character %c" c)
    | None -> failwith "Unmatched open bracket '['"
  in
  parse_main ()

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

let day display contents =
  let buff = EBR.of_string contents in
  let lines =
    EBR.seq
      ~stop:(EBR.take_while (( = ) '\n') *> EBR.at_end_of_input)
      parse_data buff
    |> List.of_seq
  in
  if display then List.iter pp lines;
  let sorted = List.fast_sort compare_data (divider_2 :: divider_6 :: lines) in
  let rec loop prod i = function
    | [] -> failwith "We should find elements we added before a sort"
    | elem :: rest ->
        if elem = divider_2 then loop i (i + 1) rest
        else if elem = divider_6 then prod * i
        else loop prod (i + 1) rest
  in
  loop 0 1 sorted |> string_of_int
