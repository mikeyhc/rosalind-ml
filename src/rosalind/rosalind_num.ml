let fib_n n m =
  let current, last = (ref 1), (ref 0) in
  for i = 1 to (n - 1) do
    let old_last = !last in
    last := !current;
    current := old_last * m + !current
  done;
  !current

let dominant_allele k m n =
  let k = float_of_int k in
  let m = float_of_int m in
  let n = float_of_int n in
  let total = k +. m +. n in
  k /. total
  +. (m /. total /. 2.)
  +. (m /. total /. 2.) *. (k /. (total -. 1.))
  +. (m /. 2. /. total) *. ((m -. 1.) /. 2. /. (total -. 1.))
  +. (n /. total) *. (m /. 2. /. (total -. 1.))
  +. (n /. total) *. (k /. (total -. 1.))
