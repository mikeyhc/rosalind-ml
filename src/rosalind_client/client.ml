(* TODO: allow DNA based functions to take DNA strings commandline args *)

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

let read_two_line_f f =
  (fun mf () ->
    let read_two_lines = fun chan ->
      (* use this method because tuple evaluation has undefined ordering *)
      let a = input_line chan in
      (a, input_line chan)
    in
    let (a, b) = match mf with
    | Some f ->
        Core.In_channel.with_file f ~f:read_two_lines
    | None -> read_two_lines stdin
    in
    f a b)


let rec read_fasta_file' chan line =
  if line.[0] != '>' then
    begin
      Printf.fprintf stderr "invalid fasta file (expected '>', got '%c')\n"
        line.[0];
      exit 1
    end
  else
    let title = String.sub line 1 ((String.length line) - 1) in
    let l = ref (input_line chan) in
    let b = Buffer.create 1000 in (* Strings are at most 1 kbp *)
    try
      begin
        while !l.[0] != '>' do
          Buffer.add_string b !l;
          l := input_line chan
        done;
        (title, Buffer.contents b)::(read_fasta_file' chan !l)
      end
    with
    | End_of_file -> [(title, Buffer.contents b)]


let read_fasta_file chan =
  let str_pairs = read_fasta_file' chan (input_line chan) in
  List.map (fun (title, s) -> (title, Rosalind.DNA.of_string s)) str_pairs

let read_fasta_f f =
  (fun mf () ->
    let fasta = match mf with
    | Some f ->
        Core.In_channel.with_file f ~f:read_fasta_file
    | None -> read_fasta_file stdin
    in
    f fasta)

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

let gc_content =
  Core.Command.basic ~summary:"Obtains the GC content of a DNA string"
    common
    (read_line_f
      (fun line ->
        Rosalind.DNA.of_string line
        |> Rosalind.DNA.gc_content
        |> Printf.printf "%f\n"))

let highest_gc =
  Core.Command.basic ~summary:("Finds the DNA string with the highest GC "
                             ^ "content")
    common
    (read_fasta_f
      (fun fasta ->
          Rosalind.DNA.highest_gc fasta
          |> function
            | Some (title, amount) ->
                Printf.printf "%s\n%f\n" title amount
            | None ->
                Printf.fprintf stderr "no strings provided\n";
                exit 1))

let dna =
  Core.Command.group ~summary:"DNA operations"
    [ "count", dna_count
    ; "compliment", dna_compliment
    ; "gc-content", gc_content
    ; "highest-gc", highest_gc
    ]


let rna_transcribe =
  Core.Command.basic ~summary:"Convert a DNA string to RNA"
    common
    (read_line_f
      (fun line ->
        Rosalind.DNA.of_string line
        |> Rosalind.RNA.of_dna
        |> fun r -> Printf.printf "%s\n" (Rosalind.RNA.to_string r)))

let rna =
  Core.Command.group ~summary:"RNA operations"
    [ "transcribe", rna_transcribe ]

let protein_from_rna =
  Core.Command.basic ~summary:"Convert an rna string into a protein"
    common
    (read_line_f
      (fun line ->
        Rosalind.RNA.of_string line
        |> Rosalind.Protein.of_rna
        |> function
          | Ok protein -> Printf.printf "%s\n"
            (Rosalind.Protein.to_string protein)
          | Error err ->
              Printf.fprintf stderr "%s\n" err;
              exit 1))


let protein =
  Core.Command.group ~summary:"Protein operations"
    [ "from-rna", protein_from_rna ]

let fib_n =
  Core.Command.basic ~summary:("Calculate the fib sequence for N generations "
                             ^ "where each generation has M offspring and "
                             ^ "comes of breeding age after 1 iteration")
    Core.Command.Spec.(
      empty
      +> anon ("n" %: int)
      +> anon ("m" %: int))
    (fun n m () ->
      Printf.printf "%d\n" (Rosalind.Num.fib_n n m))

let dominant_allele =
  Core.Command.basic ~summary:("Calculate the probability of an of "
                             ^ "the dominant allele being present in "
                             ^ "the offspring of K homozygous dominant, "
                             ^ "M hetrozygous and N hetrozygous recessive")
    Core.Command.Spec.(
      empty
      +> anon ("k" %: int)
      +> anon ("m" %: int)
      +> anon ("n" %: int))
    (fun k m n () ->
      Printf.printf "%f\n" (Rosalind.Num.dominant_allele k m n))

let num =
  Core.Command.group ~summary:"Numeric operations"
   [ "fib-n", fib_n
   ; "dominant", dominant_allele
   ]

let hamming =
  Core.Command.basic ~summary:("Calculate the hamming distance between two "
                             ^ "strings")
    common
    (read_two_line_f
      (fun a b ->
        Rosalind.Str.hamming_exn a b
        |> Printf.printf "%d\n"))

let motif =
  Core.Command.basic ~summary:"Find the occurances of a motif within a string"
    common
    (read_two_line_f
      (fun a b ->
        let data = (Rosalind.Str.motif a b) in
        List.iter (Printf.printf "%d ") data;
        print_newline ()))

let str =
  Core.Command.group ~summary:"String operations"
  [ "hamming", hamming
  ; "motif", motif
  ]

let command =
  Core.Command.group ~summary:"Manipulate genetic data"
    [ "dna", dna
    ; "rna", rna
    ; "protein", protein
    ; "num", num
    ; "str", str
    ]

let () = Core.Command.run command
