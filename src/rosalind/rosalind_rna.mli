(** This module represents a nucleotide encoding of a RNA strand

    @author mikeyhc <mikeyhc@atmosia.net> *)

(** Type representing a RNA strand *)
type t

(** Convert a string to a RNA object, no character validity checking
    is performed

    @param s string representing the RNA object
    @return a RNA object *)
val of_string : string -> t

(** Convert a RNA object to a string, no character validity checking
    is performed

    @param t the RNA object
    @return a string of the object *)
val to_string : t -> string

(** Converts a DNA object to a RNA object, no validity checking is
    performed

    @param dna the input DNA object
    @return an RNA object *)
val of_dna : Rosalind_dna.t -> t
