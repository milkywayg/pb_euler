let primes=[2;3;5;7;11;13;17;19];

let rec decomp_in_primes x primes lst_dec = 
    match primes with
    | [] -> lst_dec
    | p::rem_p -> 
            let tmp_lst=
                if x mod p=0 then (* p divise x, lst_dec cannot be empty *)
                    match List.hd lst_dec with
                    | p,n -> (p,n+1)::(List.tl lst_dec)
                    | _   -> (p,1)::lst_dec
                else lst_dec in  (* p does not divise x *) 
            let lst_dec_rem = decomp_in_primes x rem_p lst_dec in 
            lst_dec_rem@tmp_lst;;











