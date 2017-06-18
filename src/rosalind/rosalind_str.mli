(** This module contains general numeric functions

    @author mikeyhc *)

(** Returns the hamming distance between two strings.
    Assumes the strings are the same length, will except otherwise

    @param A the first string
    @param B the second string
    @return the hamming distance between the two strings *)
val hamming_exn : string -> string -> int
