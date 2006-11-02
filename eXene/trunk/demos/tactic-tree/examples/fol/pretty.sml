(* modified from L. Paulson's source *) 

signature PRETTY = 
  sig
   type T
   val blo : int * T list -> T
   val str : string -> T
   val brk : int -> T
   val format  : T * int -> string list 
   end;


structure Pretty : PRETTY =
  struct
  datatype T = 
      Block of T list * int * int 	(*indentation, length*)
    | String of string
    | Break of int;			(*length*)

  (*Add the lengths of the expressions until the next Break; if no Break then
    include "after", to account for text following this block. *)
  fun breakdist (Block(_,_,len)::sexps, after) = len + breakdist(sexps, after)
    | breakdist (String s :: sexps, after) = size s + breakdist (sexps, after)
    | breakdist (Break _ :: sexps, after) = 0
    | breakdist ([], after) = after;

  fun format (sexp, margin) =
   let val space = ref margin
       val current_line = ref [] 
       val current_text = ref [] 

       fun out_put s = (current_line := s :: (!current_line);
			space := !space - size s)

       fun blanks 0 = "" | blanks n = " " ^  blanks(n-1)

       fun make_line ([],s) = s
         | make_line (a::l,s) = make_line(l,a^s)
 
       fun newline () = (current_text := make_line (!current_line,"") :: !current_text ;  
			 current_line := [] ; 
			 space := margin)

       fun printing ([], _, _) = ()
	 | printing (sexp::sexps, blockspace, after) =
	  (case sexp of
	       Block(bsexps,indent,len) =>
		  printing(bsexps, !space-indent, breakdist(sexps,after))
	     | String s => out_put s
	     | Break len => 
		 if len + breakdist(sexps,after) <= !space 
		 then out_put(blanks len)
		 else (newline();  out_put(blanks(margin-blockspace)));
	    printing (sexps, blockspace, after))
   in  printing([sexp], margin, 0); newline(); rev (!current_text) end;

  fun length (Block(_,_,len)) = len
    | length (String s) = size s
    | length (Break len) = len;

  val str = String  and  brk = Break;

  fun blo (indent,sexps) =
    let fun sum([], k) = k
	  | sum(sexp::sexps, k) = sum(sexps, length sexp + k)
    in  Block(sexps,indent, sum(sexps,0))  end;
  end;


