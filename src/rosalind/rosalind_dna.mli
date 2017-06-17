(** This module represents a nucleotide encoding of a DNA strand

    @author mikeyhc *)

(** Type representing a DNA strand *)
type t

(** Convert a string to a DNA object, no character validity checking
    is performed

    @param s string representing the DNA strand
    @return a DNA object *)
val of_string : string -> t

(** Convert a DNA object to a string, no character validity checking
    is performed

    @param t the DNA object
    @return a string of the object *)
val to_string : t -> string

(** Counts the occurance of each nucleotide in the DNA strand

    @param t the DNA object to count nucleotides in
    @return if the DNA is valid a tuple with the counts of each of
            nucleotide will be returned in an Ok of the form
            (a * c * g * t), otherwise an Error with a string description
            of the error will be returned *)
val count_nucleotides : t -> ((int * int * int * int), string) result

(** Returns the reverse compliment of the given DNA strand

    @param t the DNA object to reverse and compliment
    @return if the DNA is valid a new DNA object with the reverse compliment
            will be returned, otherwise an Error with a string description
            of the error will be returned *)
val reverse_compliment : t -> (t, string) result

(** Returns the GC content of a given DNA strand

    @param t the DNA string to calculate the GC content of
    @return  the GC content of the strand *)
val gc_content : t -> float

(* Returns the FASTA pair with the highest GC content

   @param fasta_list a list of FASTA strings
   @return the name and GC content of the string with the highest GC
           content *)
val highest_gc: (string * t) list -> (string * float) option
