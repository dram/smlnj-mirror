(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *)
(* contmap.sml *)

(**********************************************************************
 * CONVENTIONS on the input to ContMap module:                        *
 *   The cexp into the contmap function is directly from the cpsopt   *
 *   module. We assume that every escaped functions have either one   *
 *   or two arguments and the one-arguments excaped functions are     *
 *   definitely continuations.                                        *
 **********************************************************************)

signature CONTMAP = sig
   val contmap : CPS.function -> (CPS.lvar -> bool) 
                                 * (CPS.lvar -> bool * CPS.lvar list)
end

structure ContMap : CONTMAP = 
struct
    
open CPS Access SortedList

val error = ErrorMsg.impossible

fun warning(s : string) = 
       Control.Print.say ("warning: Strang Continuation "^s^"   \n")

fun clean l =
    let fun vars(l, VAR x :: rest) = vars(x::l, rest)
          | vars(l, _::rest) = vars(l,rest)
          | vars(l, nil) = rev l
     in vars(nil,l)
    end

fun contmap(fkind,fvar,fargs,ctyl,cexp) = 
let 
    val contset1 = Intset.new()
    fun iscont1 v = Intset.mem contset1 v
    fun contM1 v = Intset.add contset1 v

    fun addcont(nil,nil) = ()
      | addcont(v::r,CNTt::z) = (contM1 v; addcont(r,z))
      | addcont(v::r,_::z) = addcont(r,z)
      | addcont _ = error "contmap contvars 124"

    fun contvars1 cexp = 
     case cexp  
      of RECORD(_,vl,w,e) => contvars1 e
       | SELECT(i,v,w,_,e) => contvars1 e
       | APP(f,vl) => ()
       | SWITCH(v,c,el) => (app contvars1 el) 
       | BRANCH(_,_,_,e1,e2) => (contvars1 e1; contvars1 e2)
       | SETTER(_,_,e) => contvars1 e
       | LOOKER(_,_,_,_,e) => contvars1 e
       | ARITH(_,_,_,_,e) => contvars1 e
       | PURE(_,_,_,_,e) => contvars1 e
       | FIX(fl,e) => (app contvars1' fl; contvars1 e)
       | _ => (error "contmap contvars1 125")

    and contvars1'(CONT,fv,fa,cl,ce) = (contM1 fv; addcont(fa,cl); 
                                        contvars1 ce)
      | contvars1'(_,fv,fa,cl,ce) = (addcont(fa,cl); contvars1 ce)

    val _ = contvars1'(fkind,fvar,fargs,ctyl,cexp)

    exception EB 
    val ebtable : (bool * lvar list) Intmap.intmap = Intmap.new(32,EB)
    fun ebinfo v = (Intmap.map ebtable v) 
                     handle EB => (true,nil)
    fun ebadd(v,info) = Intmap.add ebtable (v,info)
    fun enterv(VAR v,l) = enter(v,l)
      | enterv(_,l) = l (* error "contmap enterv 123" *)

    fun transform(RECORD(k,vl,w,e)) = 
         let val (eb,free) = transform(e)
             val free' = merge(uniq(clean(map #1 vl)),rmv(w,free))
          in (eb,free')
         end

      | transform(SELECT(i,v,w,_,e)) =
         let val (eb,free) = transform(e)
             val free' = enterv(v,rmv(w,free))
          in (eb,free')
         end

      | transform(APP(VAR f,vl)) = 
         (false,enter(f,uniq(clean(vl))))
      
      | transform(SWITCH(v,c,el)) = 
         let fun f((eb,free),(eb',free')) = (eb orelse eb',merge(free,free'))
             val (eb',free') = foldr f (false, []) (map transform el)
          in (eb',enterv(v,free'))
         end

      | transform(SETTER(i,vl,e)) =
         let val (eb,free) = transform(e)
             val free' = merge(uniq(clean vl),free)
          in (eb,free')
         end

      | transform(LOOKER(i,vl,w,_,e)) =
         let val (eb,free) = transform(e)
             val free' = merge(uniq(clean vl),rmv(w,free))
          in (eb,free')
         end

      | transform(ARITH(i,vl,w,_,e)) =
         let val (eb,free) = transform(e)
             val free' = merge(uniq(clean vl),rmv(w,free))
          in (eb,free')
         end

      | transform(PURE(i,vl,w,_,e)) =
         let val (eb,free) = transform(e)
             val free' = merge(uniq(clean vl),rmv(w,free))
          in (eb,free')
         end

      | transform(BRANCH(i,vl,c,e1,e2)) =
         let val (eb1,free1) = transform(e1)
             val (eb2,free2) = transform(e2)
             val eb = (eb1 orelse eb2) 
             val free = merge(uniq(clean vl),merge(free1,free2))
	  in (eb,free)
         end
          
      | transform(FIX(l,e)) =
         let fun g((eb',free'),(eb,free)) = (eb orelse eb',merge(free,free'))
             val (eb0,free0) = transform(e)
             val (eb',free') = foldr g (eb0,free0) (map transfunc l)
             val free'' = remove(uniq(map #2 l),free')
          in (eb',free'')
         end

      | transform _ =
         error "illformed cexp in the contmap transforming process"

    and transfunc(_,fv,fa,_,ce) = 
         let val (eb,free) = transform(ce)
             val eb' = if (iscont1 fv) then true else eb
             val free' = rmv(fv,remove(uniq(fa),free))
             val _ = ebadd(fv,(eb',free'))
          in (eb',free')
         end

    val _ = transfunc(fkind,fvar,fargs,ctyl,cexp)

 in (iscont1,ebinfo)
end (* contmap *)

end (* ContMap *)

(*
 * $Log: contmap.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:30  george
 *   Version 109.24
 *
 *)
