type t = string

let of_string s = s

let count_nucleotides t =
  let count = fun t e ->
    match t with
      | Ok (a, c, g, t) ->
        begin match e with
          | 'A' | 'a' -> Ok (a + 1, c, g, t)
          | 'C' | 'c' -> Ok (a, c + 1, g, t)
          | 'G' | 'g' -> Ok (a, c, g + 1, t)
          | 'T' | 't' -> Ok (a, c, g, t + 1)
          | _ -> Error ("invalid DNA character: " ^ Char.escaped e)
        end
      | Error _ as err -> err
  in
  Core.String.fold ~f:count ~init:(Ok (0, 0, 0, 0)) t
