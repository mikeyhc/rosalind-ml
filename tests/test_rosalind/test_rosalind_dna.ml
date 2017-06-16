open OUnit2

let nucleotides_test1 test_ctxt =
  assert_equal (Ok (20, 12, 17, 21))
    (Rosalind.DNA.count_nucleotides
      (Rosalind.DNA.of_string
        ("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATT"
         ^ "AAAAAAAGAGTGTCTGATAGCAGC")))

let reverse_compliment_test1 test_ctxt =
  assert_equal (Ok (Rosalind.DNA.of_string "ACCGGGTTTT"))
    (Rosalind.DNA.reverse_compliment (Rosalind.DNA.of_string "AAAACCCGGT"))

let nucleotides =
  ["nucleotides_test1" >:: nucleotides_test1]

let reverse_compliment =
  ["reverse_compliment_test1" >:: reverse_compliment_test1]

let suite =
  "suite" >:::
    List.flatten(
      [ nucleotides
      ; reverse_compliment
      ])

let () = run_test_tt_main suite
