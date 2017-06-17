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

let gc_content t =
  let count_gc = fun acc e ->
    match e with
      | 'G' | 'g' | 'C' | 'c' -> acc +. 1.
      | _ -> acc
  in
  let count = Core.String.fold ~f:count_gc ~init:0. t
  in count /. float_of_int (String.length t) *. 100.

let highest_gc = function
  | ((title, str)::ft) ->
      let highest = fun ((_at, ac) as acc) (et, es) ->
        let ec = gc_content es in
        if ec > ac then (et, ec) else acc
      in
      Some (List.fold_left highest (title, gc_content str) ft)
  | _ -> None

