type t

val of_string : string -> t
val count_nucleotides : t -> ((int * int * int * int), string) result
