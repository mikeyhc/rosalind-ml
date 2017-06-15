open OUnit2

let nucleotides_test1 test_ctxt =
  assert_equal (Ok (20, 12, 17, 21))
    (Rosalind.DNA.count_nucleotides
      (Rosalind.DNA.of_string
        ("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATT"
         ^ "AAAAAAAGAGTGTCTGATAGCAGC")))

let nucleotides =
    ["nucleotides_test1" >:: nucleotides_test1]

let suite =
  "suite" >:::
    nucleotides

let () = run_test_tt_main suite
