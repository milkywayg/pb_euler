(* is m divisible by one of the list elements *)
let rec div_by_list m lst = 
    (* let max_calc = sqrt(float_of_int(m)) in *)
    (* match List.rev lst with *)
    match  lst with
    |[] ->  false
    |h::tl -> if (m mod h=0)  then true
    (* else if (h > (int_of_float(max_calc))+1) then false *)
    else div_by_list m tl;;


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

let result = find_nth_prime 10001;;

let ()= print_int result





