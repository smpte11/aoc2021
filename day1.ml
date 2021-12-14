open Core
open List

let input = "public/day1.txt" |> In_channel.read_lines >>| int_of_string

let measurements = [ 199; 200; 208; 210; 200; 207; 240; 269; 260; 263 ]

let measure l =
  let rec measure' init = function
    | x :: y :: rest -> measure' (init + Bool.to_int (x < y)) (y :: rest)
    | _ -> init
  in
  measure' 0 l

let () =
  Profile.profile measure measurements |> Printf.printf "Result -> %d\n\n"

let () = Profile.profile measure input |> Printf.printf "Result -> %d\n\n"

let measure_sliding l =
  let rec measure' init = function
    | a :: b :: c :: d :: rest ->
        measure'
          (init + Bool.to_int (a + b + c < b + c + d))
          (b :: c :: d :: rest)
    | _ -> init
  in
  measure' 0 l

let () =
  Profile.profile measure_sliding input |> Printf.printf "Result -> %d\n\n"
