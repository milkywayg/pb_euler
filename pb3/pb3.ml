let has_divisor n1=
    let is_div n x =
        if (x=1) then false else
            if (n mod x)=0 then true
            else false in
    let rec is_div_list n l= 
        match l with
        |[]    -> false
        |x::l1  -> if is_div n x then true else is_div_list n l1 in
    let max_lst=int_of_float(sqrt(float_of_int(n1))) in
    let chk_l = 2::List.init max_lst (fun x -> 2*x+1) in
    is_div_list n1 chk_l;;

let largest_div n1=
    let is_div n x =
        if (x=1) then false else
            if (n mod x)=0 then true
            else false in
    let rec smallest_div_list n l= 
        match l with
        |[]     -> n
        |x::l1  -> if is_div n x then x else smallest_div_list n l1 in
    let largest_div_list n l =
        let sm_div = smallest_div_list n l in n/sm_div in 
    let max_lst=int_of_float(sqrt(float_of_int(n1))) in
    let chk_l = 2::List.init max_lst (fun x -> 2*x+1) in
    largest_div_list n1 chk_l;;


let rec largest_prime_div n=
    let bg = largest_div n in
    if (has_divisor bg) then largest_prime_div bg else bg;;


let ans = largest_prime_div 600851475143 ;;

let () =
    Printf.printf "%d\n" ans;;


