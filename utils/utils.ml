let with_timer f =
  let before = Mtime_clock.counter () in
  let result = f () in
  let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count before) /. 1e6 in
  (elapsed, result)

let parallel_for size body =
  let nb_domains = Domain.recommended_domain_count () in
  let chunk_size = size / nb_domains in
  let f n () =
    for i = chunk_size * n to (chunk_size * (n + 1)) - 1 do
      body i
    done
  in
  let arr = Array.init (nb_domains - 1) (fun i -> Domain.spawn (f i)) in
  f (nb_domains - 1) ();
  Array.iter Domain.join arr

let slurp fs path fn =
  let ( / ) = Eio.Path.( / ) in
  let path = fs / path in
  Eio.Path.with_open_in path (fun flow ->
      let buf = Eio.Buf_read.of_flow ~max_size:65536 flow in
      fn buf)

let get_input fs n = Printf.sprintf "day%d/input.txt" n |> slurp fs
let get_test fs n = Printf.sprintf "../../../day%d/test.txt" n |> slurp fs
