type monkey = { items : int list; operation : int -> int; test : int -> int }

let pp { items; _ } =
  let rec loop = function
    | [] -> ()
    | [ x ] -> print_int x
    | x :: xs ->
        Printf.printf "%d, " x;
        loop xs
  in
  loop items;
  print_newline ()

let parse_list str =
  String.split_on_char ',' str
  |> List.map (fun str -> String.trim str |> int_of_string)

let parse_op opchar operand old =
  let op =
    match opchar with '+' -> ( + ) | '-' -> ( - ) | '*' -> ( * ) | _ -> ( / )
  in
  match int_of_string_opt operand with Some n -> op old n | None -> op old old

let parse_test divis cons alt x = if x mod divis = 0 then cons else alt

let parse contents =
  let ic = Scanf.Scanning.from_string contents in
  let rec loop monkeys =
    try
      let monkey =
        Scanf.bscanf ic
          " Monkey %_d: Starting items: %s@\n\
          \ Operation: new = old %c %s Test: divisible by %d If true: throw to \
           monkey %d If false: throw to monkey %d "
          (fun items opchar operand divis cons alt ->
            {
              items = parse_list items;
              operation = parse_op opchar operand;
              test = parse_test divis cons alt;
            })
      in
      loop (monkey :: monkeys)
    with End_of_file -> monkeys
  in
  loop [] |> List.rev |> Array.of_list

let day display contents =
  let monkeys = parse contents in
  let len = Array.length monkeys in
  let active = Array.make len 0 in
  let round () =
    Array.iteri
      (fun i { items; operation; test } ->
        let rec throw = function
          | [] -> ()
          | item :: xs ->
              let worry = operation item in
              let worry' = worry / 3 in
              let j = test worry' in
              assert (j <> i);
              let target = monkeys.(j) in
              monkeys.(j) <- { target with items = target.items @ [ worry' ] };
              active.(i) <- active.(i) + 1;
              throw xs
        in
        throw items;
        monkeys.(i) <- { items = []; operation; test })
      monkeys
  in
  for i = 1 to 20 do
    round ();
    if display then (
      Printf.printf "\nAfter round %d, monkeys are holding:\n" i;
      Array.iteri
        (fun i m ->
          Printf.printf "Monkey %d: " i;
          pp m)
        monkeys)
  done;
  Array.fast_sort compare active;
  active.(len - 2) * active.(len - 1) |> string_of_int
