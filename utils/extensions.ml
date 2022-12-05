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
    let _, before, after =
      fold_left
        (fun (seen, before, after) elem ->
          if seen || pred elem then (true, before, elem :: after)
          else (false, elem :: before, after))
        (false, [], []) l
    in
    (rev before, rev after)
end
