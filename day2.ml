open Core
open List

let parse_line line =
  let direction = String.split ~on:' ' line in
  match direction with
  | [ "forward"; value ] -> int_of_string value
  | [ "down"; value ] -> int_of_string value
  | [ "up"; value ] -> int_of_string value
  | _ -> 0

let parse_to_command filename = filename |> In_channel.read_lines >>| fun x -> x
