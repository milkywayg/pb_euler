
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







