module List = struct
  include List

  (** [split_at n l] returns a pair of lists:
      - the first contains the n first elements.
      - the second contains the rest, if any. *)
  let split_at n l =
    let rec aux n l before =
      match (n, l) with
      | _, [] -> (rev before, [])
      | 0, _ -> (rev before, l)
      | n, x :: xs -> aux (n - 1) xs (x :: before)
    in
    aux n l []

  (** [split_on pred l] returns a pair of lists:
    - the first contains the elements at the start of [l], up to the first element that satisfies pred (not included).
    - the second contains the elements that follow (first satisfier included). *)
  let split_on pred l =
    let rec aux l before =
      match l with
      | [] -> (rev before, [])
      | x :: xs -> if pred x then (rev before, l) else aux xs (x :: before)
    in
    aux l []
end

module Array = struct
  include Array

  (** [findi_opt f a] returns the index of the first element of the array [a]
      that satisfies the predicate [f], or [None] if there is no such index. *)
  let findi_opt f a =
    let len = length a in
    let rec loop i =
      if i = len then None else if f a.(i) then Some i else loop (i + 1)
    in
    loop 0
end
