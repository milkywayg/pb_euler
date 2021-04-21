(* is m divisible by one of the list elements *)
let rec div_by_list m lst =
    (* let max_calc = sqrt(float_of_int(m)) in *)
    (* match List.rev lst with *)
    match  lst with
    |[] ->  false
    |h::tl -> if (m mod h=0)  then true
    (* else if (h > (int_of_float(max_calc))+1) then false *)
    else div_by_list m tl;;


(* (* this is not efficient probably due to list reverse *) *)
(* let rec div_by_list m lst =  *)
(*     let max_calc = sqrt(float_of_int(m)) in *)
(*     (* print_float max_calc; print_char ' '; *) *)
(*     match List.rev lst with *)
(*     |[] ->  false *)
(*     |h::tl -> if (m mod h=0)  then true *)
(*     else if (Float.compare (float_of_int(h))  max_calc) = 1 then false *)
(*     else div_by_list m (List.rev tl);; *)


let last_elm_in_list lst = 
    List.hd  (List.rev lst)

(* let find_nth_prime n_max= *)
    (* let rec aux n_pr n_fr lst_pr= *)
    (*     let last_in_lst_pr = List.hd lst_pr in *)
    (*     if ((List.length lst_pr)=n_pr) then last_in_lst_pr *)
    (*     else  *)
    (*         let nxt_in_lst = (last_in_lst_pr+2) in *)
    (*         if not ((div_by_list (last_in_lst_pr+1) lst_pr)) then  (*nxt element after the list is not divided by the list *) *)
    (*             aux n_pr nxt_in_lst ((nxt_in_lst-1)::lst_pr) *)
    (*         else  *)
    (*             aux n_pr nxt_in_lst lst_pr in *)
    (* aux n_max 3 [2]; *)

let ad a b =a+b;;



let find_nth_prime n_max=
    let rec aux n_pr n_fr lst_pr=
        let last_in_lst_pr = List.hd lst_pr in
        if ((List.length lst_pr)=n_pr) then last_in_lst_pr
        else 
            if not ((div_by_list n_fr lst_pr)) then  (*nxt element after the list is not divided by the list *)
                aux n_pr (n_fr+2)  (n_fr::lst_pr)
            else 
                aux n_pr (n_fr+2) lst_pr in
    aux n_max 3 [2];;

(* let result = find_nth_prime 10001;; *)
(*  *)
(* let ()= print_int result *)
(*  *)

(* let sum_prime_till p_max= *)
    (* let psum= ref 0 in  *)
    (* let idx = ref 0 in *)
    (* let cur_prime = ref 0 in  *)
    (* while (!cur_prime <= p_max) do *)
    (*     idx  := !idx+1; *)
    (*     (* print_int !cur_prime; *) *)
    (*     psum := !psum + !cur_prime; *)
    (*     cur_prime := find_nth_prime !idx;  *)
    (*     (* print_int !psum; *) *)
    (* done ; !psum *)
    (* (* done; let res = !psum in res *) *)
(* [> let res = !psum in res <] *)




(* let sum_prime_till2 p_max =  *)
(*     let rec build_p_list n_max p_list p_sum= *)
(*         if n_max=2 then 2::[], 2  *)
(*         else if (n_max mod 2 =0) then build_p_list (n_max-1) p_list p_sum *)
(*         else (*n_max odd *) *)
(*             if not div_by_list n_max p_list then  *)
(*             let tmp_list, tmp_p_sum = build_p_list (n_max-1) p_list p_sum  in *)
(*             n_max::tmp_list, (tmp_p_sum+n_max) *)



let sum_prime_till2 p_max = 
    let rec build_p_list n_num p_list p_sum=
        if (n_num <p_max) then 
            if (n_num mod 2 =0) then build_p_list (n_num+1) p_list p_sum
            else (*n_num odd *)
                if not (div_by_list n_num p_list) then 
                    let tmp_list, tmp_p_sum = build_p_list (n_num+2) (n_num::p_list) (n_num+p_sum) in 
                    tmp_list, tmp_p_sum
                else build_p_list (n_num+1) p_list p_sum
        else p_list, p_sum in
    build_p_list 3 [2] 2;;

let int_sqrt num =
    int_of_float(sqrt(float_of_int (num)))

let rec is_prime n_num p_list =
    match p_list with
    |[] -> false
    |h::tl -> if h > int_sqrt(n_num) then true
              else if (n_num mod h=0) then false
              else is_prime n_num tl

let sum_prime_till3 p_max = 
    let rec build_p_list n_num p_list p_sum=
        if (n_num <p_max) then 
            if (n_num mod 2 =0) then build_p_list (n_num+1) p_list p_sum
            else (*n_num odd *)
                if (is_prime n_num p_list) then 
                    let tmp_list, tmp_p_sum = build_p_list (n_num+2) (p_list@[n_num]) (n_num+p_sum) in 
                    tmp_list, tmp_p_sum
                else build_p_list (n_num+2) p_list p_sum
        else p_list, p_sum in
    build_p_list 8 [2;3;5;7] 17;;


let ()= 
    let tmp_l, ps=(sum_prime_till3 2000000) in
    print_int ps;;


















