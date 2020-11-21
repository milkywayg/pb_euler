
let rec list_car ch = match ch with
| "" -> []
| ch -> (String.get ch 0 ) :: (list_car (String.sub ch 1 ( (String.length ch)-1) ) )  ;;

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf;;

let rev str = 
    string_of_chars (List.rev (list_car (str)));;

let is_palindrome str = 
    if (compare str (rev str))==0 then true
    else false;;


let min_val=100;;

let rec first_enc_paldr_prod_l x min_val = 
    let is_palindrome_prod x y =
        if is_palindrome (string_of_int (x*y)) then true
        else false in
    let rec is_largest_palindrome_prod_r y =
        if y<min_val then false, 0 
        else if is_palindrome_prod x y then true, y
        else is_largest_palindrome_prod_r (y-1) in
    let exist_paldm_for_x, val_y  = is_largest_palindrome_prod_r x in
    if exist_paldm_for_x then x, val_y, (x*val_y)
    else if x<min_val then 0, 0, 0
    else first_enc_paldr_prod_l (x-1) min_val;;


(* let rec largest_palindrome_prod_l x min_val = *)
    (* let nx, nyi , p  = first_enc_paldr_prod_l x min_val in *)
    (* if p > first_enc_paldr_prod_l (x-1) min_val then  *)
(*  *)




(* The working code is below *)
let is_palindrome_prod x y =
    if is_palindrome (string_of_int (x*y)) then true
    else false ;;

(* retuns the largest palindrome where the lft operand is x and min right operand *)
(* is min_val *)
let rec largest_palindrome_prod_l x  min_val= 
    let rec is_largest_palindrome_prod_r y =
        if y<min_val then false, 0 
        else if is_palindrome_prod x y then true, y
        else is_largest_palindrome_prod_r (y-1) in
    let exist_paldm_for_x, val_y  = is_largest_palindrome_prod_r x in
    if exist_paldm_for_x then true, val_y, x*val_y
    else false, 0, 0;;

let rec largest_pldr_prod x min_val mx_x mx_y mx_prd = 
    let is_pl_x, v_y, p = largest_palindrome_prod_l x min_val in
    if x=min_val then mx_x, mx_y, mx_prd 
    else if is_pl_x &&  p>mx_prd then
        largest_pldr_prod (x-1) v_y x v_y p
    else
        largest_pldr_prod (x-1) min_val mx_x mx_y mx_prd;;

largest_pldr_prod 999 100 0 0 0;;


(* utop # largest_pldr_prod 999 100 0 0 0;; *)
(* - : int * int * int = (993, 913, 906609) *)























