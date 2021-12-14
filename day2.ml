open Core
open List

let test_input =
  [ "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2" ]

let input = "public/day2.txt" |> In_channel.read_lines

let parse_line line =
  match String.split ~on:' ' line with
  | [ dir; n ] -> (dir, int_of_string n)
  | _ -> raise (Invalid_argument line)

let calculate_position (x, y) (dir, n) =
  match dir with
  | "forward" -> (x + n, y)
  | "up" -> (x, y - n)
  | "down" -> (x, y + n)
  | _ -> raise (Invalid_argument dir)

let solve directions =
  directions >>| parse_line |> List.fold_left ~init:(0, 0) ~f:calculate_position
  |> fun (x, y) -> x * y

let () = Profile.profile solve test_input |> Printf.printf "Result -> %d\n\n"

let () = Profile.profile solve input |> Printf.printf "Result -> %d\n\n"

let calculate_position' (x, y, aim) (dir, n) =
  match dir with
  | "forward" -> (x + n, y + (aim * n), aim)
  | "up" -> (x, y, aim - n)
  | "down" -> (x, y, aim + n)
  | _ -> raise (Invalid_argument dir)

let solve' directions =
  directions >>| parse_line
  |> List.fold_left ~init:(0, 0, 0) ~f:calculate_position'
  |> fun (x, y, _) -> x * y

let () = Profile.profile solve' input |> Printf.printf "Result -> %d\n\n"
