let measure l =
  let rec measure' init = function
    | [] | _::[] -> init
    | x::y::rest -> measure' (init + Bool.to_int(x < y)) (y::rest) in
  measure' 0 l
 g g
