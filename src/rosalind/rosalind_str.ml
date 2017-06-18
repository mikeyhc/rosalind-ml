let hamming_exn a b =
  let counter = ref 0 in
  let f = fun i c -> if c != b.[i] then begin incr counter; c end else c in
  let _s = Core.String.mapi ~f a in
  !counter
