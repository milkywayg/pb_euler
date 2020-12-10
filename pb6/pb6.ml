(* #require "batteries" *)
open Batteries
open BatInt


let rec sum_of_square n_max = 
   match n_max with
   |1 -> 1
   |n -> n*n+(sum_of_square (n-1))

let diff_of_sums n_max=
    (n_max*(n_max+1)/2)**2-sum_of_square n_max


diff_of_sums 100;;




