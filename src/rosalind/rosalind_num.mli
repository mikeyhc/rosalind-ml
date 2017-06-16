(** This module contains general numeric functions

    @author mikeyhc *)

(** Returns the value of the recurance relationship after N iterations
    where F_(N-1) * M is generated instead in the normal fibonacci
    sequence

    @param N the number of iterations (<= 40)
    @param M the number each F_(N-1) generation produces (<= 5)
    @return the recurance relation for (N, M) *)
val fib_n : int -> int -> int
