let	rec	member	(x:int)	=	function	
    []->false
    |h::t->	if	x=h	then
        true
    else
        member x t;;

member 4 [1;2;4;6];;



