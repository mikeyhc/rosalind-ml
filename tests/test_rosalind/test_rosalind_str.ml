open OUnit2

let hamming_test1 test_ctxt =
  assert_equal 7 (Rosalind.Str.hamming_exn "GAGCCTACTAACGGGAT"
                                           "CATCGTAATGACGGCCT")

let hamming =
  ["hamming_test1" >:: hamming_test1]

let motif_test1 test_ctxt =
  assert_equal [2;4;10] (Rosalind.Str.motif "GATATATGCATATACTT" "ATAT")

let motif =
  ["motif_test1" >:: motif_test1]

let suite =
  "suite" >:::
    List.flatten
      [ hamming
      ; motif
      ]

let () = run_test_tt_main suite
