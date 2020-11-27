let primes=[2;3;5;7;11;13;17;19];;


let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;


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
            lst_dec_rem ;;


