let hamming_exn a b =
  let counter = ref 0 in
  let f = fun i c -> if c != b.[i] then begin incr counter; c end else c in
  let _s = Core.String.mapi ~f a in
  !counter

let motif str motif =
  let l = ref [] in
  for i = 0 to (String.length str - String.length motif - 1) do
    if String.sub str i (String.length motif) = motif then
      l := (i + 1)::!l
  done;
  List.rev !l
