(* conventions on the input to ContMap module: 
 *   The cexp into the contmap function is directly from the cpsopt module. 
 *   We assume that every escaped functions have either one or two arguments
 *   and the one-arguments excaped functions are definitely continuations.
 *)
signature CONTMAP = sig
   val contmap : CPS.function -> CPS.function * (CPS.lvar -> bool) 
end

structure ContMap : CONTMAP = 
struct
    
open CPS Access SortedList


(* utility functions , may be duplicated with those in other modules .
 *)
val error = ErrorMsg.impossible
fun clean l =
    let fun vars(l, VAR x :: rest) = vars(x::l, rest)
          | vars(l, _::rest) = vars(l,rest)
          | vars(l, nil) = rev l
     in vars(nil,l)
    end
fun sublist test =
    let fun subl(a::r) = if test a then a::(subl r) else subl r
          | subl [] = []
     in  subl
    end
fun divlist test =
    let fun divl(a::r) = let val (t1,t0) = divl r
                          in if (test a) then (a::t1,t0)
                             else (t1,a::t0)
                         end
          | divl [] = ([],[])
     in divl
    end
fun mixer(v::vl,t::tl) = (v::t)::mixer(vl,tl)
  | mixer(nil,nil) = nil
  | mixer _ = error "grouping known fun info with diff # of args"
fun grouping nil = nil 
  | grouping (vl::nil) = map (fn x => [x]) vl
  | grouping (vl::tl) =  mixer(vl,grouping tl)                        


(* contmap eliminates all strange continuation variables and generates accurate 
 * continuation variable information .
 *)
fun contmap(fvar,fargs,cexp) = 
let val (_,_,known) = FreeMap.freemapClose(cexp)
    val escapes = not o known

    exception Mcont
    val t : (value list list) ref Intmap.intmap = Intmap.new(32,Mcont)
    fun find v = (!(Intmap.map t v)) handle Mcont => nil

    fun add(v,vl) = (let val a = Intmap.map t v
                         val _ = (a := (vl::(!a)))
                      in ()
                     end) handle Mcont => Intmap.add t (v,ref([vl]))

    val contset = Intset.new()
    fun iscont v = Intset.mem contset v
    fun contM v = Intset.add contset v

    fun iscontv (VAR v) = iscont v 
      | iscontv _ = false 


(* cexp -> lvar list, i.e. known function name list. The side-effect is to find all 
 * continuation variables and to put them into a hash table.
 *)
    val rec incinfo = fn (e,l) => (contvars e)@l
    and contvars = 
      fn RECORD(vl,w,e) => contvars e
       | SELECT(i,v,w,e) => contvars e
       | APP(f as (VAR g),vl) => 
           if (escapes g) then 
             (let val k = length(vl)
               in if (k > 2) orelse (k < 1) 
		  then (error "escaped funs have > 2 or < 1 arguments z";
		        nil)    
		  else if k = 1 then (contM g ; nil)
		       else let val VAR c = nth(vl,1)     (* the 2nd arg should be a var *)
			    in (contM c ; nil)
                            end
              end)        
           else (add(g,vl);[g])
       | SWITCH(v,el) => (fold incinfo el) nil
       | PRIMOP(i,vl,wl,el) => (fold incinfo el) nil
       | FIX(fl,e) =>
	   let fun h((fv,fa,ce),a) =
                if escapes fv then 
                 (let val k = length(fa)
                      val kl = contvars ce
                   in if (k > 2) orelse (k < 1) 
		      then (error "escaped funs have > 2, < 1 args z";
			    a)
		      else if k = 1 then (contM fv ; (kl@a))
                           else (contM (nth(fa,1)) ; (kl@a))
                  end)
                else (let val kl = contvars ce
                       in add(fv,(map VAR fa));
		          fv::(kl@a)
                      end)
            in (fold h fl) (contvars e)
           end 

    val knownlabs = 
           contvars (FIX([(fvar,fargs,cexp)],PRIMOP(P.+,[],[],[])))
    val knownlabs = uniq knownlabs
          
(* run the stupid loop to gather all known functions' continuation variable 
 * information . It's expected to be rewritten in the future .
 *)
    local val clicked = ref 0
          fun click () = (inc clicked) 
          fun cpass v = 
           let val infolist = (find v) 
               val newl = grouping infolist
               fun proc vl =
	             case divlist iscont (clean vl)                   
                      of (nil,_) => ()
                       | (_,nil) => ()
                       | (_,vl0) => (click(); (app contM vl0))
            in (app proc newl) 
           end
          fun loop () = let val _ = (app cpass knownlabs) 
                            val k = (!clicked) before (clicked := 0)
                         in if k = 0 then ()
                            else loop ()
                        end
    in val _ = loop ()
    end 

(* Do the transformation , let all continuations become well-behaved so that 
 * we can do calleesave optimizations on them . 
 *)

    fun lookup(v,nil) = NONE
      | lookup(v,(a,b)::tl) = if v = a then (SOME b) else lookup(v,tl)


    fun clookup(v,env) = (case lookup(v,env) of 
                            NONE => v
                          | SOME v' => v')
    fun glookup(VAR v,env) = VAR (clookup(v,env))
      | glookup(x,env) = x
	
    fun substin(v,(env,fl)) = 
         case lookup(v,env) of 
            NONE => (let val v' = dupLvar v
		         val x = mkLvar()
                         val c = mkLvar() 
                         val tmp = (v',[x,c],APP(VAR v,[VAR x]))
		      in ((v,v')::env,tmp::fl)
                     end)
          | SOME v' => (env,fl)

    fun substout(v,(env,fl)) = 
         case lookup(v,env) of 
            NONE => (let val v' = dupLvar v
		         val x = mkLvar()
                         val tmp = (v,[x],APP(VAR v',
                                           [(VAR x),(INT 0)]))
		      in ((v,v')::env,tmp::fl)
                     end)
          | SOME v' => (env,fl)


    fun transform(RECORD(vl,w,e),env) = 
         let val cl = uniq(sublist iscont (clean (map #1 vl)))
             val (env',fl) = (fold substin cl) (env,nil)
             fun g(x,p) = (glookup(x,env'),p)
          in case fl 
              of nil => RECORD((map g vl),w,transform(e,env'))
               | _ => FIX(fl,RECORD((map g vl),w,transform(e,env')))
         end

      | transform(SELECT(i,v,w,e),env) =
         if (iscont w) then 
           (let val (env',fl) = substout(w,(env,nil))
                val w' = clookup(w,env')
             in case fl
                 of nil => SELECT(i,v,w',transform(e,env'))
                  | _ => SELECT(i,v,w',FIX(fl,transform(e,env')))
            end)
         else SELECT(i,v,w,transform(e,env))

      | transform(APP(VAR f,vl),env) = 
         let val cl = sublist iscont (clean vl)
             val k = if iscont f then (length cl) else (length cl)-1  
          in if (k < 1) 
             then APP(VAR f,vl)
             else (let fun sep(nil) = (nil,nil)
                         | sep(hd::tl) = 
                             if iscontv hd then ([hd],tl)
                             else (let val (a,b) = sep tl
                                    in (hd::a,b)
                                   end)
                       val (vl1,vl2) = if iscont f then (nil,vl) 
                                       else sep(List.rev vl)
                       val (vl1,vl2) = (List.rev vl1 , List.rev vl2) 
                       val cl = uniq(sublist iscont (clean vl2))
                       val (env',fl) = (fold substin (uniq cl)) (env,nil)
                       val vl' = (map (fn x => glookup(x,env')) vl2)@vl1
                    in case fl of nil => APP(VAR f,vl')
                                | _ => FIX(fl,APP(VAR f,vl'))
                   end)
         end
      
      | transform(SWITCH(v,el),env) = 
         SWITCH(v,(map (fn x => transform(x,env)) el))

      | transform(PRIMOP(i,vl,wl,el),env) =
         let val cl1 = sublist iscont (clean vl)
             val (env',fl) = (fold substin cl1) (env,nil)
             val cl2 = sublist iscont wl
             val (env'',fl') = (fold substout cl2) (env',nil)
             val vl' = map (fn x => glookup(x,env'')) vl
             val wl' = map (fn x => clookup(x,env'')) wl
             fun h x = case fl' of nil => transform(x,env'')
                                 | _ => FIX(fl',transform(x,env''))
          in case fl of nil => PRIMOP(i,vl',wl',map h el)
                      | _ => FIX(fl,PRIMOP(i,vl',wl',map h el))
         end
          
      | transform(FIX(l,e),env) =
         FIX(map (fn x => transfunc(x,env)) l,transform(e,env))

      | transform(_,env) =
         error "illformed cexp in the contmap transforming process"

    and transfunc((fv,fa,ce),env) = 
         let fun transf(fv,fa,ce) = 
                   let val cl = sublist iscont fa
                       val k = if iscont fv then length(cl) 
                               else length(cl)-1 
                       val cl' = if k < 1 then nil 
                                 else if iscont fv then cl
                                      else (List.tl(List.rev cl))
                       val (env',fl) = (fold substout cl') (env,nil)
                       val fa' = map (fn x => clookup(x,env')) fa 
                    in case fl of nil => (fv,fa',transform(ce,env'))
                                | _ => (fv,fa',FIX(fl,transform(ce,env')))
                   end
          in transf(fv,fa,ce)
         end

    val (fvar',fargs',cexp') = transfunc((fvar,fargs,cexp),nil)

 in ((fvar',fargs',cexp'),iscont)
end (* contmap *)

end (* ContMap *)
