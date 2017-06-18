open OUnit2

let rna_to_protein_test1 test_ctxt =
  assert_equal (Ok (Rosalind.Protein.of_string "MAMAPRTEINSTRING"))
    (Rosalind.Protein.of_rna
      (Rosalind.RNA.of_string
        "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"))

let rna_to_protein =
  ["rna_to_protein" >:: rna_to_protein_test1
  ]

let suite =
  "suite" >:::
    rna_to_protein

let () = run_test_tt_main suite
