structure PP :> PP =
struct

   type indent = int
   type col    = int
   type mode   = string
   datatype tok = STRING | NUM | SYM | TOK | SPACE | NEWLINE
   type state = string list * indent list * mode list * col * tok
   type pp = state -> state

   val tabspace = 3

   infix ++
   val blanks = "                                                          "
   fun f ++ g = g o f
   fun nop S = S
   fun sp (b,i,m,c,SPACE)   = (b,i,m,c,SPACE)
     | sp (b,i,m,c,NEWLINE) = (b,i,m,c,NEWLINE)
     | sp (b,i,m,c,t)       = (" "::b,i,m,c+1,SPACE)
   fun space (b,i,m,c,SPACE)   = (b,i,m,c,SPACE)
     | space (b,i,m,c,NEWLINE) = (b,i,m,c,NEWLINE)
     | space (b,i,m,c,SYM)     = (b,i,m,c,SYM)
     | space (b,i,m,c,t)       = (" "::b,i,m,c+1,SPACE)
   fun ! s S = let val (b,i,m,c,t) = space S
               in  (s::b,i,m,size s+c,TOK) end
   fun !! s (b,i,m,c,t) = (s::b,i,m,size s+c,SYM) 
   fun bool false = ! "false"
     | bool true  = ! "true"
   fun string s S = let val (b,i,m,c,t) = space S
                        val s = "\""^String.toString s^"\"" 
                    in  (s::b,i,m,size s+c,STRING) end
   fun char s S = let val (b,i,m,c,t) = space S
                      val s = "#\""^Char.toString s^"\""
                  in  (s::b,i,m,size s+c,STRING) end
   fun num s S = let val (b,i,m,c,t) = space S
                 in  (s::b,i,m,size s+c,NUM) end
   fun int n S = num (Int.toString n) S
   fun real r S = num (Real.toString r) S
   fun word w S = num ("0wx"^Word32.toString w) S
   fun tab' offset ((b,i,m,c,t) : state) = 
       let val at = (case i of i::_ => i |  _ => 0) + offset
           val n = at - c
       in if n <= 0 then (b,i,m,c,t)
          else ((String.substring(blanks,0,n) handle 
                Subscript => blanks)::b,i,m,at,SPACE)
       end
   val tab = tab' 0
   fun indent (b,[],m,c,t) = (b,[tabspace],m,c,t)
     | indent (b,i as (x::_),m,c,t) = (b,x+tabspace::i,m,c,t)
   fun settab (b,i,m,c,t) = (b,c::i,m,c,t)
   fun unindent (b,_::i,m,c,t) = (b,i,m,c,t)
   fun setmode mode (b,i,m,c,t) = (b,i,mode::m,c,t)
   fun unsetmode (b,i,_::m,c,t) = (b,i,m,c,t)
   fun select f (b,i,m as mode::_,c,t) = f mode (b,i,m,c,t)
   fun nl (b,i,m,c,t) = ("\n"::b,i,m,0,NEWLINE)
   fun nl' max (b,i,m,c,t) = if c >= max then tab(nl(b,i,m,c,t))
                             else (b,i,m,c,t)
   fun seq (l,sep,r) pps = 
   let fun f [] = nop
         | f [a] = a
         | f(a::b) = a ++ sep ++ f b 
   in  l ++ f pps ++ r end
   fun concat pps = foldr op++ nop pps
   fun block pp = indent ++ pp ++ unindent
   fun line pp  = tab ++ pp ++ nl
   fun paren pp = !! "(" ++ pp ++ !! ")"
   fun group(l,r) pp = settab ++ !! l ++ settab ++ pp ++ 
                       unindent ++ tab ++ !! r ++ unindent
   fun text pp = let val (b,_,_,_,_) = pp([],[],["pretty"],0,NEWLINE)
                 in  String.concat(rev b) end

end
