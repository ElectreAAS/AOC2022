let reset = "\x1b[0m"

(** Returns the ANSI escape sequence to print the colour defined by [r g b] in a terminal.
    [r g b] should all be between 0 and 256. *)
let rgb (r, g, b) = Printf.sprintf "\x1b[38;2;%d;%d;%dm" r g b

(** Returns the ANSI escape sequence to print the colour defined by [n] in a terminal.
    [n] should be between 0 and 256. *)
let simple n = Printf.sprintf "\x1b[38;5;%dm" n

let gold = "\x1b[33m"
let red = "\x1b[38;2;194;17;17m"
let green = "\x1b[38;2;23;130;59m"
let grey = "\x1b[38;5;240m"