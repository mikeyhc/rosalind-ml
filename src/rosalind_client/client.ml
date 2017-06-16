let common =
  Core.Command.Spec.(
    empty
    +> flag "-f" (optional file) ~doc:"file read from file instead of stdin"
  )

let dna =
  Core.Command.basic ~summary:"Count nucleotides in a DNA string"
    Core.Command.Spec.(common)
    (fun mf () ->
      let line = match mf with
      | Some f ->
          Core.In_channel.with_file f ~f:(fun inc -> input_line inc)
      | None -> input_line stdin
      in
      Rosalind.DNA.of_string line
      |> Rosalind.DNA.count_nucleotides
      |> fun r -> match r with
          | Ok (a, g, c, t) -> Printf.printf "%d %d %d %d\n" a g c t
          | Error msg ->
              Printf.fprintf stderr "%s\n" msg;
              exit 1
      )

let rna =
  Core.Command.basic ~summary:"Convert a DNA string to RNA"
    Core.Command.Spec.(common)
    (fun mf () ->
      let line = match mf with
      | Some f ->
          Core.In_channel.with_file f ~f:(fun inc -> input_line inc)
      | None -> input_line stdin
      in
      Rosalind.DNA.of_string line
      |> Rosalind.RNA.of_dna
      |> fun r -> Printf.printf "%s\n" (Rosalind.RNA.to_string r))

let command =
  Core.Command.group ~summary:"Manipulate genetic data"
    ["dna", dna ; "rna", rna]

let () = Core.Command.run command
