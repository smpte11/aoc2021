open Core

module List = struct
  include List

  let traverseOptionM f l =
    let ( >>= ) x f = Option.bind x ~f in
    let init = Option.return [] in
    let f head tail =
      f head >>= fun h ->
      tail >>= fun t -> Option.return (h :: t)
    in

    List.fold_right l ~init ~f

  let sequenceOptionM x = traverseOptionM Fn.id x
end

module Option = struct
  include Option

  let product x y =
    match (x, y) with
    | Some x, Some y -> Some (x * y)
    | Some x, None -> Some x
    | None, Some y -> Some y
    | _ -> None

  let ( |*| ) x y = product x y
end