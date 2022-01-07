open Core

let input = "public/day3.txt" |> In_channel.read_lines

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

let transpose' l = l |> List.map ~f:String.to_list |> List.transpose_exn

let counter l =
  l |> List.map ~f:(fun x -> (x, 1)) |> Char.Map.of_alist_reduce ~f:( + )

let most_common m =
  m |> counter |> fun x ->
  let zeros = Map.find_exn x '0' in
  let ones = Map.find_exn x '1' in
  if zeros > ones then '0' else if zeros < ones then '1' else '1'

let least_common m =
  m |> counter |> fun x ->
  let zeros = Map.find_exn x '0' in
  let ones = Map.find_exn x '1' in
  if zeros > ones then '1' else if zeros < ones then '0' else '0'

let bin_to_number bin = int_of_string ("0b" ^ bin)

let gamma_rate l =
  l |> transpose' |> List.map ~f:most_common |> String.of_char_list
  |> bin_to_number

let epsilon_rate l =
  l |> transpose' |> List.map ~f:least_common |> String.of_char_list
  |> bin_to_number

let () =
  let gamma = gamma_rate diagnostics in
  let epsilon = epsilon_rate diagnostics in
  Printf.printf "Part 1 - Test input %d\n" (gamma * epsilon)

let () =
  let gamma = gamma_rate input in
  let epsilon = epsilon_rate input in
  Printf.printf "Part 1 - Real input %d\n" (gamma * epsilon)

let locate selector l =
  let rec aux i' l' =
    match (i', l') with
    | _, [ x ] -> x
    | index, xs ->
        let m = xs |> List.map ~f:(fun x -> List.nth_exn x index) |> selector in
        let rem =
          List.filter
            ~f:(fun x ->
              let i = List.nth_exn x index in
              Char.equal i m)
            xs
        in
        aux (index + 1) rem
  in
  aux 0 l

let o2_gen l =
  l |> List.map ~f:String.to_list |> locate most_common |> String.of_char_list
  |> bin_to_number

let co2_scrub l =
  l |> List.map ~f:String.to_list |> locate least_common |> String.of_char_list
  |> bin_to_number

let () =
  let o2 = o2_gen diagnostics in
  let co2 = co2_scrub diagnostics in
  Printf.printf "Part 2 - Test input -> %d\n" (o2 * co2)

let () =
  let o2 = o2_gen input in
  let co2 = co2_scrub input in
  Printf.printf "Part 2 - Real input -> %d\n" (o2 * co2)