(** This module contains general numeric functions

    @author mikeyhc *)

(** Returns the value of the recurance relationship after N iterations
    where F_(N-1) * M is generated instead in the normal fibonacci
    sequence

    @param N the number of iterations (<= 40)
    @param M the number each F_(N-1) generation produces (<= 5)
    @return the recurance relation for (N, M) *)
val fib_n : int -> int -> int

(** Returns the probability of the dominant allele being present
    in the offspring on K homozygous dominant, M hetrozygous and
    N homozygous recessive

    @param K the number of hetrozygous dominant
    @param M the number of homozygous
    @param N the number of hetrozygous recessive
    @return the probability that two randomly selected organisms will
            produce and individual posessing a dominant allele *)
val dominant_allele : int -> int -> int -> float
