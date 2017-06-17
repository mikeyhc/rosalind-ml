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

let highest_gc_test1 test_ctxt =
  let v =
    (Rosalind.DNA.highest_gc
      (List.map (fun (x, y) -> (x, Rosalind.DNA.of_string y))
        [ ("Rosalind_6404", "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCT"
                          ^ "GAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG")
        ; ("Rosalind_5959", "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAG"
                          ^ "GCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC")
        ; ("Rosalind_0808", "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACG"
                          ^ "CTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT")
        ]))
  in
  match v with
  | Some (name, content) ->
    Printf.printf "%s %f\n" name content;
    assert (name = "Rosalind_0808" && content >= 60.919539
                                   && content <= 60.919541)
  | None -> assert false

let gc_content_test1 test_ctxt =
  let content = Rosalind.DNA.gc_content
    (Rosalind.DNA.of_string ("CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAAC"
      ^ "GCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"))
  in
  assert (content >= 60.919539 && content <= 60.919541)

let nucleotides =
  ["nucleotides_test1" >:: nucleotides_test1]

let reverse_compliment =
  ["reverse_compliment_test1" >:: reverse_compliment_test1]

let gc_content =
  [ "gc_content_test1" >:: gc_content_test1
  ; "highest_gc_test1" >:: highest_gc_test1
  ]

let suite =
  "suite" >:::
    List.flatten(
      [ nucleotides
      ; reverse_compliment
      ; gc_content
      ])

let () = run_test_tt_main suite
