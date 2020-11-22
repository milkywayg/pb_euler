let primes=[2;3;5;7;11;13;17;19];

let rec decomp_in_primes x primes lst_dec = 
    match primes with
    | [] -> lst_dec
    | p::rem_p -> 
            let lst_dec_rem = decomp_in_primes x rem_p lst_dec in 
            if x mod p=0 then (* p divise x *)
                    if lst_dec=[] then (p,1)::lst_dec_rem
                    else
                        match List.hd lst_dec with
                        | p,n -> (p,n+1)::List.tl@lst_dec_rem
                        | _   -> (p,1)::List.tl@lst_dec_rem
            else (* p does not divise x *)
                lst_dec_rem;;









