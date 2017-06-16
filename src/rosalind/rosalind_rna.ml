type t = string

let of_string s = s
let to_string t = t

let of_dna d =
  let s = Rosalind_dna.to_string d in
  let buf = Buffer.create (String.length s) in
  let conv = function
    | 'T' | 't' -> Buffer.add_char buf 'U'
    | _ as e -> Buffer.add_char buf e
  in
  Core.String.iter ~f:conv s;
  Buffer.contents buf
