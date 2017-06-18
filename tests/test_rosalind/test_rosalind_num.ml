open OUnit2

let fib_n_test1 test_ctxt =
  assert_equal 19 (Rosalind.Num.fib_n 5 3)

let dominant_allele_test1 test_ctxt =
  let prob = Rosalind.Num.dominant_allele 2 2 2
  in assert (prob >= 0.78332 && prob <= 0.78334)

let fib_n =
  ["fib_n_test1" >:: fib_n_test1]

let dominant_allele =
  ["dominant_allele_test1" >:: dominant_allele_test1]


let suite =
  "suite" >:::
    List.flatten
      [ fib_n
      ; dominant_allele
      ]

let () = run_test_tt_main suite
