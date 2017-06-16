open OUnit2

let rna_trascribe_test1 test_ctxt =
  assert_equal (Rosalind.RNA.of_string "GAUGGAACUUGACUACGUAAAUU")
    (Rosalind.RNA.of_dna
      (Rosalind.DNA.of_string "GATGGAACTTGACTACGTAAATT"))

let transcribes =
    ["rna_trascribe_test1" >:: rna_trascribe_test1]

let suite =
  "suite" >:::
    transcribes

let () = run_test_tt_main suite
