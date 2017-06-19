(** This module contains general numeric functions

    @author mikeyhc *)

(** Returns the hamming distance between two strings.
    Assumes the strings are the same length, will except otherwise

    @param a the first string
    @param b the second string
    @return the hamming distance between the two strings *)
val hamming_exn : string -> string -> int

(** Searches a string for a motif (i.e. a subsring), returns all locations
    of the motif.

    TODO: currently this does a linear search, this is fine due to the small
          size of the rosalind datasets, if this is used for bigger datasets
          it would be worth implementing something a bit smarter

    @param str the string to search
    @param motif the motif to search for
    @return a list with all locations the motif occured *)
val motif : string -> string -> int list
