module T = Domainslib.Task

module type DAY = sig
  val day : bool -> T.pool -> Eio.Buf_read.t -> string
end

let days : (module DAY) array =
  [|
    (module Day0);
    (module Day1);
    (module Day2);
    (module Day3);
    (module Day4);
    (module Day5);
    (module Day6);
    (module Day7);
    (module Day8);
    (module Day9);
    (module Day10);
    (module Day11);
    (module Day12);
    (module Day13);
    (module Day14);
    (module Day15);
    (module Day16);
  |]

let expected =
  [
    "";
    "45000";
    "12";
    "70";
    "4";
    "MCD";
    "19";
    "24933642";
    "8";
    "36";
    {|
██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░
███░░░███░░░███░░░███░░░███░░░███░░░███░
████░░░░████░░░░████░░░░████░░░░████░░░░
█████░░░░░█████░░░░░█████░░░░░█████░░░░░
██████░░░░░░██████░░░░░░██████░░░░░░████
███████░░░░░░░███████░░░░░░░███████░░░░░
|};
    "2713310158";
    "29";
    "140";
    "93";
    "56000011";
    "1707";
  ]
