type t = string

exception InvalidDNA

let of_string s = s
let to_string t = t

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

let reverse_compliment t =
  let buf = Buffer.create (String.length t) in
  let compliment = function
    | 'A' | 'a' -> Buffer.add_char buf 'T'
    | 'T' | 't' -> Buffer.add_char buf 'A'
    | 'C' | 'c' -> Buffer.add_char buf 'G'
    | 'G' | 'g' -> Buffer.add_char buf 'C'
    | _ -> raise InvalidDNA
  in
  try
    String.iter compliment (Core.String.rev t);
    Ok (Buffer.contents buf)
  with
  | InvalidDNA -> Error "Invalid DNA"
