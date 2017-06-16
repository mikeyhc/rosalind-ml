open OUnit2

let fib_n_test1 test_ctxt =
  assert_equal 19 (Rosalind.Num.fib_n 5 3)

let fib_n =
  ["fib_n_test1" >:: fib_n_test1]

let suite =
  "suite" >:::
    fib_n

let () = run_test_tt_main suite
