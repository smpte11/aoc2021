open Core
open Fn
open List
open Tuple

let diagnostics =
  [
    "00100";
    "11110";
    "10110";
    "10111";
    "10101";
    "01111";
    "00111";
    "11100";
    "10000";
    "11001";
    "00010";
    "01010";
  ]

let to_list_of_binary l = l >>| String.to_list |> transpose

let to_tuples l = l >>| List.map ~f:(fun x -> (x, 1))

let counter' = Int.Map.of_alist_reduce ~f:(fun x y -> x + y)

let counter = List.map ~f:counter'
(* let reduced =

   let () =
     Map.iteri ~f:(fun ~key ~data -> Printf.printf "%d -> %d\n" key data) reduced *)
