let fib_n n m =
  let current, last = (ref 1), (ref 0) in
  for i = 1 to (n - 1) do
    let old_last = !last in
    last := !current;
    current := old_last * m + !current
  done;
  !current

