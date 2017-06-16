let common =
  Core.Command.Spec.(
    empty
    +> flag "-f" (optional file) ~doc:"file read from file instead of stdin"
  )

let read_line_f f =
  (fun mf () ->
      let line = match mf with
      | Some f ->
          Core.In_channel.with_file f ~f:(fun inc -> input_line inc)
      | None -> input_line stdin
      in
      f line)

let dna_count =
  Core.Command.basic ~summary:"Count nucleotides in a DNA string"
    common
    (read_line_f
      (fun line ->
        Rosalind.DNA.of_string line
        |> Rosalind.DNA.count_nucleotides
        |> fun r -> match r with
          | Ok (a, g, c, t) -> Printf.printf "%d %d %d %d\n" a g c t
          | Error msg ->
              Printf.fprintf stderr "%s\n" msg;
              exit 1
          ))

let dna_compliment =
  Core.Command.basic ~summary:"Obtains the reverse compliment of a DNA string"
    common
    (read_line_f
      (fun line ->
        Rosalind.DNA.of_string line
        |> Rosalind.DNA.reverse_compliment
        |> fun r -> match r with
          | Ok dna -> Printf.printf "%s\n" (Rosalind.DNA.to_string dna)
          | Error msg ->
              Printf.fprintf stderr "%s\n" msg;
              exit 1
      ))

let dna =
  Core.Command.group ~summary:"DNA operations"
    [ "count", dna_count
    ; "compliment", dna_compliment
    ]


let rna =
  Core.Command.basic ~summary:"Convert a DNA string to RNA"
    common
    (read_line_f
      (fun line ->
        Rosalind.DNA.of_string line
        |> Rosalind.RNA.of_dna
        |> fun r -> Printf.printf "%s\n" (Rosalind.RNA.to_string r)))

let command =
  Core.Command.group ~summary:"Manipulate genetic data"
    ["dna", dna ; "rna", rna]

let () = Core.Command.run command
