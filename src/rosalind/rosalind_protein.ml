type t = string

let of_string s = s
let to_string t = t

let codon_table =
  let mtable = Core.Hashtbl.of_alist
    ~hashable:Core.String.hashable
    [ "UUU", Some 'F' ; "CUU", Some 'L' ; "AUU", Some 'I' ; "GUU", Some 'V'
    ; "UUC", Some 'F' ; "CUC", Some 'L' ; "AUC", Some 'I' ; "GUC", Some 'V'
    ; "UUA", Some 'L' ; "CUA", Some 'L' ; "AUA", Some 'I' ; "GUA", Some 'V'
    ; "UUG", Some 'L' ; "CUG", Some 'L' ; "AUG", Some 'M' ; "GUG", Some 'V'
    ; "UCU", Some 'S' ; "CCU", Some 'P' ; "ACU", Some 'T' ; "GCU", Some 'A'
    ; "UCC", Some 'S' ; "CCC", Some 'P' ; "ACC", Some 'T' ; "GCC", Some 'A'
    ; "UCA", Some 'S' ; "CCA", Some 'P' ; "ACA", Some 'T' ; "GCA", Some 'A'
    ; "UCG", Some 'S' ; "CCG", Some 'P' ; "ACG", Some 'T' ; "GCG", Some 'A'
    ; "UAU", Some 'Y' ; "CAU", Some 'H' ; "AAU", Some 'N' ; "GAU", Some 'D'
    ; "UAC", Some 'Y' ; "CAC", Some 'H' ; "AAC", Some 'N' ; "GAC", Some 'D'
    ; "UAA", None     ; "CAA", Some 'Q' ; "AAA", Some 'K' ; "GAA", Some 'E'
    ; "UAG", None     ; "CAG", Some 'Q' ; "AAG", Some 'K' ; "GAG", Some 'E'
    ; "UGU", Some 'C' ; "CGU", Some 'R' ; "AGU", Some 'S' ; "GGU", Some 'G'
    ; "UGC", Some 'C' ; "CGC", Some 'R' ; "AGC", Some 'S' ; "GGC", Some 'G'
    ; "UGA", None     ; "CGA", Some 'R' ; "AGA", Some 'R' ; "GGA", Some 'G'
    ; "UGG", Some 'W' ; "CGG", Some 'R' ; "AGG", Some 'R' ; "GGG", Some 'G'
    ]
  in
  match mtable with
  | `Ok table -> table
  | _ -> assert false (* should be impossible *)

exception Stop_codon
exception Invalid_codon of string
exception No_stop_cordon

let partition n l =
  let break_counter = ref n in
  let break = fun _x _y ->
    decr break_counter;
    if !break_counter = 0 then
      begin
        break_counter := n;
        true
      end
    else
      false
  in
  Core.List.group ~break l

let of_rna rna =
  let open Core.Option in
  let srna = Rosalind_rna.to_string rna in
  let char_list = Core.String.to_list srna in
  let grouped = List.map Core.String.of_char_list (partition 3 char_list) in
  let buf = Buffer.create 3333 in (* input RNA is at most 10kbp *)
  let encode  = fun elem ->
    match Core.Hashtbl.find codon_table elem with
    | Some (Some c) -> Buffer.add_char buf c
    | Some None -> raise Stop_codon
    | None -> raise (Invalid_codon elem)
  in
  try
    List.iter encode grouped;
    raise No_stop_cordon
  with
  | Stop_codon -> Ok (Buffer.contents buf)
  | Invalid_codon err -> Error ("Invalid_codon: " ^ err)
  | No_stop_cordon -> Error ("Invalid protein: had no stop cordon")
