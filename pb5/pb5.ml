(* #require "batteries" *)
open Batteries

let primes=[2;3;5;7;11;13;17;19;23;29;31;37;41;43;47]

let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let rec decomp_in_primes (x:int) prim lst_dec = 
    match prim with
    | [] -> lst_dec
    | p::rem_p -> 
            let tmp_lst, tmp_x, tmp_prim =
                if lst_dec=[] then
                    if x mod p=0 then
                        [(p,1)], x/p, prim
                    else [], x, rem_p
                else if x mod p=0 then (* p divise x, lst_dec cannot be empty *)
                    (* print_list lst_dec;  *)
                    match (List.hd lst_dec) with
                    | (aa,n) -> if aa=p then (p,n+1)::(List.tl lst_dec), x/p, prim
                               else  (p,1)::lst_dec, x/p, prim
                else lst_dec, x, rem_p in  (* p does not divise x *) 
            let lst_dec_rem = decomp_in_primes tmp_x tmp_prim tmp_lst  in 
            lst_dec_rem 


let rec decomp_prime_list lst =
    match lst with
    | []   -> []
    | h::t -> (h, decomp_in_primes h primes [])::decomp_prime_list t

(* let rec range a b = *)
let lst_0_20 = List.of_enum (1--20)

let my_decomp= decomp_prime_list lst_0_20

(* print_list my_decomp *)

(* assumes a>b *)
let rec gcd_tmp a b=
    match b with
    |0 -> a
    |_ -> gcd_tmp b (a mod b)

let gcd a b =
    if a>b then gcd_tmp a b
    else gcd_tmp b a


let rec gcd_l lst=
    if List.length lst = 2 then 
        match lst with
        |a::b::[] -> gcd a b
        |_  -> 0
    else 
        match lst with
        |h::t -> gcd h (gcd_l t)
        |_ -> 0

let rec prod_l lst =
    match lst with
    | a::[] -> a 
    | h::tl -> h*prod_l tl
    | [] -> 1


let lcm a b = 
    abs (a*b/(gcd a b))


(* lets use the fact that lcm is associative *)
let rec lcm_l lst = 
    match lst with
    |[] -> 1
    | h::tl -> lcm h (lcm_l tl)

let result = (lcm_l lst_0_20)

let () =
    print_int result




