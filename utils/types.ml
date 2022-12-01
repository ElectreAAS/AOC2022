type error =
  | Input of string
  | FileSystem of string
  | Parsing of string
  | Logic of string
  | Thread

let pp fmt = function
  | Input s -> Format.fprintf fmt "Input error: %s\n" s
  | FileSystem s -> Format.fprintf fmt "File system error: %s\n" s
  | Parsing s -> Format.fprintf fmt "Error while parsing: %s\n" s
  | Logic s ->
      Format.fprintf fmt "Not as unreachable as you might think: %s\n" s
  | Thread -> Format.fprintf fmt "Joined thread crashed ;(\n"

type 'a result = ('a, error) Result.t

let ( let* ) = Result.bind
let ( >>= ) = Result.bind
let ( >>!= ) = Result.iter
