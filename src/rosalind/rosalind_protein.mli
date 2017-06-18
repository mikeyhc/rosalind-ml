(** This module represents a protein string

    @author mikeyhc *)

(** Type representing a protein *)
type t

(** Convert a string into a protein object, no character validity checking
    is performed

    @param s string representing the protein strand
    @return a protein strand object *)
val of_string : string -> t

(** Convert a protein object into a string, no character validity checking
    is performed

    @param t the protein object
    @return a string of the object *)
val to_string : t -> string

(** Convert an RNA object into a protein object

    @param rna the RNA object, at most 10kbp
    @return a protein object in an Ok, or if an invalid amino acid was
            encounter a Error with the a string representation of the
            issue *)
val of_rna : Rosalind_rna.t -> (t, string) result
