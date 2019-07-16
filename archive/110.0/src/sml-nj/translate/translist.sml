(* Copyright 1996 by Bell Laboratories *)
(* translist.sml *)

(****************************************************************************
 *  This module is a very preliminary implementation of unrolled list       *
 *  optimizations. Only those variables with UrList.list type are           *
 *  optimized.        (zsh, 11/19/93)                                       *
 ****************************************************************************)

signature TRANSLIST = 
sig
  val translist : int * Lambda.lexp -> Lambda.lexp
  val selectLty : LambdaType.lty * int -> LambdaType.lty
  val cvtrfty : LambdaType.lty -> LambdaType.lty

end (* signature TRANSLIST *)

structure TransList : TRANSLIST = 

struct

local open Lambda Symbol 
      structure V = Vector
      structure LT = LambdaType
      structure CGoptions = Control.CG

in 

(****************************************************************************
 *                    MISC UTILITY FUNCTIONS                                *
 ****************************************************************************)

fun error s = ErrorMsg.impossible ("Translist: "^s)
val say = Control.Print.say
fun sayv v = say (LambdaVar.lvarName v)
val ident = fn e => e
fun force(f,e) = f e
fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []
val map2 = ListPair.map
val dupLvar = LambdaVar.dupLvar
val mkLvar = LambdaVar.mkLvar

(***** a parameter that controling the unroll level *****)
val unroll_level = 8  (*** meaning unroll 3 level deep at most ***)

fun LET(x,e1,e2) = APP(FN(x,LT.BOGUS,e2),e1)

val ELISTty = LT.inj(LT.LISTty 2)
val OLISTty = LT.inj(LT.LISTty 1)
val ULISTty = LT.inj(LT.LISTty 0)

fun andf (f,a::r,b::z) = if f(a,b) then andf(f,r,z) else false
  | andf (f,[],[]) = true
  | andf (f,_,_) = false

fun formap(n,f) = 
  if n <= 0 then true else if f(n) then formap(n-1,f) else false

(****************************************************************************
 *                    UNROLLED LIST HEADER                                  *
 ****************************************************************************)

(* the structure of unrolled list *)
val ulistsign = [TAGGED 1 (*TAGGEDREC(1,2)*),TAGGED 2]

val olistSym = varSymbol "OLIST"
val olistlty = LT.injARROW(OLISTty,(* RECORDty[BOXEDty,LISTty 2],*)ULISTty)
val olistrep = TAGGED 1 (* TAGGEDREC (1,2) *)
val olistdcon = (olistSym,olistrep,NONE,olistlty)  (* ZHONG? *)
val olistDcon = DATAcon olistdcon

val elistSym = varSymbol "ELIST"
val elistlty = LT.injARROW(ELISTty,ULISTty)
val elistrep = TAGGED 2
val elistdcon = (elistSym,elistrep,NONE,elistlty) (* ZHONG? *)
val elistDcon = DATAcon elistdcon

(* generate a switch on the unrolled list header *)
fun switch_ulist(e,e1,e2) =
      SWITCH(e,ulistsign,[(olistDcon,e1),(elistDcon,e2)],NONE)

fun OLIST e = CON(olistdcon,e)
fun ELIST e = CON(elistdcon,e)
fun dOLIST e = DECON(olistdcon,e)
fun dELIST e = DECON(elistdcon,e)

(****************************************************************************
 *                    UNROLLED LIST TAIL                                    *
 ****************************************************************************)

(* the structure of even length list *)
val elistsign = [CONSTANT 0,UNTAGGED (*UNTAGGEDREC 3*)]  (* ZHONG? *)

val tnilSym = varSymbol "TNIL"
val tnilrep = CONSTANT 0
val tnillty = ELISTty
val tnildcon = (tnilSym,tnilrep,NONE,tnillty)   (* ZHONG? *)
val tnilDcon = DATAcon tnildcon

val tail2Sym = varSymbol "TAIL2"
val tail2rep = UNTAGGED (*UNTAGGEDREC 3*)  (* ZHONG? *)
val tail2lty = LT.injARROW(LT.injRECORD[LT.injBOXED,LT.injBOXED,ELISTty],ELISTty)
val tail2dcon = (tail2Sym,tail2rep,NONE,tail2lty)  (* ZHONG? *)
val tail2Dcon = DATAcon tail2dcon

(* generate a switch on the unrolled list tail2 *)
fun switch_elist(e,en,et) =
      SWITCH(e,elistsign,[(tnilDcon,en),(tail2Dcon,et)],NONE)

fun TNIL(e) = CON(tnildcon,e)
fun dTNIL(e) = DECON(tnildcon,e)

(* CON the "e1","e2" and "e3" into a cons cell *)
fun TAIL2(e1,e2,e3) = CON(tail2dcon,RECORD[e1,e2,e3])

(* DECON the expression "e" into (u,v,w) *)
fun dTAIL2 (e,u,v,w) le = 
  let val z = mkLvar()
   in LET(z,DECON(tail2dcon,e),
        LET(u,SELECT(0,VAR z),
          LET(v,SELECT(1,VAR z),
            LET(w,SELECT(2,VAR z),le))))
  end

(* the structure of odd length list *)
val tail1Sym = varSymbol "TAIL1"
val tail1rep = TRANSPARENT
val tail1lty = LT.injARROW(LT.injRECORD[LT.injBOXED,ELISTty],OLISTty)
val tail1dcon = (tail1Sym,tail1rep,NONE,tail1lty)  (* ZHONG? *)
val tail1Dcon = DATAcon tail1dcon

fun TAIL1(e1,e2) = CON(tail1dcon,RECORD[e1,e2])
fun dTAIL1 (e,u,v) le =
  let val z = mkLvar()
   in LET(z,DECON(tail1dcon,e),
        LET(u,SELECT(0,VAR z),
          LET(v,SELECT(1,VAR z),le)))
  end

fun OLIST2(e1,e2) = CON(olistdcon,TAIL1(e1,e2))


(****************************************************************************
 *                    ABSTRACT LIST CONSTRUCTOR                             *
 ****************************************************************************)

fun TNIL2(e) = ELIST(TNIL(e))
fun dTNIL2(e) = dTNIL(dELIST e)

fun CONS2 e =
  let val z = mkLvar() and x = mkLvar() and r = mkLvar()
      val a = mkLvar() and b = mkLvar()
      val oe = dTAIL1 (dOLIST(VAR r),a,b) (ELIST(TAIL2(VAR x,VAR a,VAR b)))
      val ee = OLIST2(VAR x,dELIST(VAR r))
   in LET(z,e,LET(x,SELECT(0,VAR z),
                LET(r,SELECT(1,VAR z),switch_ulist(VAR r,oe,ee))))
  end

fun dCONS2 e =
  let val z = mkLvar() and a = mkLvar() and b = mkLvar()
      val oe = dTAIL1 (dOLIST(VAR z),a,b) (RECORD [VAR a, ELIST(VAR b)])
      val v = mkLvar() and w = mkLvar() and x = mkLvar()
      val ee = dTAIL2 (dELIST(VAR z),v,w,x) (RECORD[VAR v,OLIST2(VAR w,VAR x)])
   in LET(z,e,switch_ulist(VAR z,oe,ee))
  end

fun OCONS e = CON(tail1dcon,e)

fun dOCONS e = DECON(tail1dcon,e)

fun ECONS e = 
  let val z = mkLvar() and a = mkLvar() and u = mkLvar() and v = mkLvar()
   in LET(z,e,
        LET(a,SELECT(0,VAR z),
          dTAIL1 (SELECT(1,VAR z),u,v) (TAIL2(VAR a,VAR u,VAR v))))
  end

fun dECONS e = 
  let val z = mkLvar() and a = mkLvar() and b = mkLvar() and v = mkLvar()
   in LET(z,DECON(tail2dcon,e),
        LET(a,SELECT(0,VAR z),
          LET(b,SELECT(1,VAR z),
            LET(v,SELECT(2,VAR z),(RECORD[VAR a,TAIL1(VAR b,VAR v)])))))
  end

(****************************************************************************
 *                    REFINEMENT TYPE UTILITY FUNCTIONS                     *
 ****************************************************************************)

(*** only check the first two list components in a record type ***)
fun candidates t = case LT.out t
   of LT.LISTty 0 => ([OLISTty,ELISTty],2)
    | LT.RECORDty l => 
      let fun f(z,r,n,k) =
           if (n <= 1) then 
             (if (k <= 1) then ([],0)
              else (map (fn x => LT.injRECORD((rev x)@z)) r,k))
           else (case z
                  of [] => if k <= 1 then ([],0)
                           else (map (fn x => LT.injRECORD((rev x))) r,k)
                   | a::y => (case LT.out a
			       of LT.LISTty 0 => 
	                        let val nl = map (fn x => [OLISTty::x,ELISTty::x]) r
        	                    val ny = List.concat nl
                	         in f(y,ny,n div 2,k+k)
                        	end
			      | _ => f(y,map (fn x => (a::x)) r,n,k)))
       in f(l,[nil],unroll_level,1) 
      end          
   | _ => ([],0)

(*** find out candidates and make a header ***)
(* the type of hdr is ((lexp -> lexp) list * lexp -> lexp *)

fun scandidates t = case LT.out t
  of LT.LISTty 0 => 
      let fun hdr (l,e) = 
            let val v = mkLvar()
             in LET(v,e, switch_ulist(VAR v,(List.nth(l,0)) (dOLIST(VAR v)),
                                            (List.nth(l,1)) (dELIST(VAR v))))
            end
       in ([OLISTty,ELISTty],2,hdr)
      end
  | LT.RECORDty l => 
      let val v = mkLvar()

          fun mkhdr(n,hdr,cts) =
            if n < 0 then (hdr,cts)
            else let val x = mkLvar()
                     val nhdr = fn e => LET(x,SELECT(n,VAR v),hdr e)
                  in mkhdr(n-1,nhdr,(VAR x)::cts)
                 end

          val (phdr,contents) = mkhdr((List.length l)-1,ident,[])

          val ll = ListPair.zip (l,contents)

          fun f(z,r,n,k,rank,hdr) =
           if (n <= 1) then 
             (if (k <= 1) then ([],0,hdr)
              else (map (fn x => ((rev x)@z)) r,k,hdr))
           else (case z 
                  of [] => if k <= 1 then ([],0,hdr)
                           else (map (fn x => (rev x)) r,k,hdr)
                   | ((a as (t',q))::y) => 
		     	(case LT.out t'
			  of LT.LISTty 0 =>
			    let val qo = mkLvar()
				val qe = mkLvar()
				val nl = map (fn x => [(OLISTty,VAR qo)::x,
						       (ELISTty,VAR qe)::x]) r
				val ny = List.concat nl
				fun nhdr l = 
				  let fun mkhdr i = 
				       if i >= k then []
				       else ((switch_ulist(q,
						 LET(qo,dOLIST(q),(List.nth(l,2*i))),
						 LET(qe,dELIST(q),(List.nth(l,2*i+1)))))
					       ::(mkhdr (i+1)))
				   in hdr(mkhdr 0)
				  end
			     in f(y,ny,n div 2,k+k,rank+1,nhdr)
			    end
			  | _ => f(y,map (fn x => a::x) r,n,k,rank+1,hdr)))

          val basefn = (fn [e] => e 
                         | _ => error "basefn in translist") 

          val (r,n,hdr) = f(ll,[nil],unroll_level,1,0,basefn)

          val (tl,args) = foldr (fn (z,(u,v)) => let
		  val (z1,z2) = foldr (fn ((a,b),(c,d)) => (a::c,b::d)) ([],[]) z
                    in (z1::u,z2::v)
                   end) ([],[]) r
          val r = map LT.injRECORD tl 
          val nargs = map RECORD args

          fun nhdr l = let val nl = map2 (fn(f,x) => f(x)) (l,nargs)
                        in hdr nl
                       end
       in (r,n,fn (l,e) => LET(v,e,phdr(nhdr l)))
      end          
  | _ => ([],0,fn (l,e) => e)

(* find out the order number of a refinement type *)
fun rfnum t = case LT.out t
  of LT.LISTty k => k
   | LT.RECORDty l => 
      let fun f(z,n,k) = 
            if (n<=1) then k+1
            else (case z
                   of [] => if (n < unroll_level) then k+1 else 0
		    | a::y => case LT.out a
			 of LT.LISTty 0 => 0
			  | LT.LISTty j => f(y,n div 2,k+k+j-1)
			  | _ => f(y,n,k))
       in f(l,unroll_level,0)
      end
   | _ => error "rfnum on a non-refinable type"

(* n should be nevel bigger than unroll_level *)
fun rftynum (t,j,n) = case LT.out t
  of LT.LISTty _ => LT.inj(LT.LISTty j)
   | LT.RECORDty l => 
      let fun f(z,k,n,r) = 
            if (n <= 1) then LT.injRECORD((rev r)@z)
            else (case z 
                   of [] => if (n < unroll_level) then LT.injRECORD(rev r)
                            else error "rftynum on no REFty 23"
		    | a::y => (case LT.out a
				of LT.LISTty 0 =>
	                          let val nn = n div 2
	                              val i = (k div nn) + 1
	                           in f(y,k mod nn,nn,(LT.inj(LT.LISTty i))::r)
	                          end
				 | _ => f(y,k,n,a::r)))

       in if j <= 0 then LT.injRECORD l else f(l,j-1,n,[])
      end
   | _ => error "rfnum on a non-refinable type"

(* check if a type is refinable *)
fun rfable t = case LT.out t
  of LT.LISTty 0 => true
   | LT.RECORDty l => List.exists (fn (LT.LISTty 0) => true | _ => false) (map LT.out l)
   | _ => false

(* check if t1 is a refinement type of the type t2 *)
fun isrfty(u1,u2) = case (LT.out u1, LT.out u2)
 of (LT.LISTty k, LT.LISTty 0) => true
  | (LT.BOTty t1, LT.BOTty t2) => LT.equivLty(t1,t2)
  | (LT.BOTty t1, _) => LT.equivLty(t1,u2)
  | (LT.RECORDty l1, LT.RECORDty l2) => andf(isrfty,l1,l2)
  | (LT.ARROWty(t11,t12), LT.ARROWty(t21,t22)) =>
        (LT.equivLty(t11,t21)) andalso (isrfty(t12,t22))
  | (LT.REFty(t11,z), LT.ARROWty(t21,t22)) =>
        let val t12 = V.sub(z,0)
         in (LT.equivLty(t11,t21)) andalso (isrfty(t12,t22))
        end
  | _ => LT.equivLty(u1,u2)

(* find out the top lambda type of the current refinement type t *)
fun top t = case LT.out t
  of LT.BOTty t' => t'
   | LT.LISTty _ => ULISTty
   | LT.ARROWty(t1,t2) => LT.injARROW(t1,top t2)
   | LT.RECORDty l => LT.injRECORD (map top l)
   | LT.REFty(t1,z) => LT.injARROW(t1,top (V.sub(z,0)))
   | _ => t

(* flatten a refinement type to a BOT-free ML type *)
fun flatten t = case LT.out t 
 of LT.BOTty t' => t'
  | LT.ARROWty(t1,t2) => LT.injARROW(t1,flatten t2)
  | LT.RECORDty l => LT.injRECORD (map flatten l)
  | LT.REFty(t1,z) =>
      let fun g(i) = flatten(V.sub(z,i))
       in LT.inj(LT.REFty(t1,V.tabulate(V.length z,g)))
      end      
  | _ => t

(* apply an arrow type to another refinement type *)
fun apprfty (t,t2) = case (LT.out t, LT.out t2)
 of (LT.BOTty z, _) => (case LT.out z of LT.ARROWty(_,t1) => LT.inj(LT.BOTty t1) | _ => t)
  | (LT.ARROWty(_,t'), LT.BOTty _) => LT.inj(LT.BOTty(top t'))
  | (LT.ARROWty(_,t'), _) => t'
  | (LT.REFty(_,z), LT.BOTty _) => LT.inj(LT.BOTty(top(V.sub(z,0))))
  | (LT.REFty(_,z), _) => V.sub(z,rfnum t2)
  | _ => t

(* reverse the direction of a type, never applied to BOTty *)
fun revrfty t = case LT.out t of LT.ARROWty(t1,t2) => LT.injARROW(t2,t1) | _ => t

val equivrfty = LT.equivLty  (* ??? *)

(* select out an element from a record type *)
fun selrfty(i,t) = case LT.out t
  of LT.RECORDty l => List.nth(l,i)
   | LT.SRECORDty l => List.nth(l,i)
   | LT.GRECty l =>
     let fun g((j,s)::r,i) = if i  = j then s else g(r,i)
           | g([],i) = error "selrfty2 on non-record type"
      in g(l,i)
     end
   | LT.BOTty t => LT.inj(LT.BOTty(selrfty(i,t)))
   | _ => error "selrfty on non-record type"

(* selectout a type *)

fun selectLty (t,i) = 
  let fun f(LT.RECORDty l) = List.nth(l,i)
        | f(LT.GRECty l) = g(l,i) 
        | f(LT.REFty(t1,z2)) =
             let val result = V.sub(z2,i)
                 val argt = rftynum(t1,i,V.length(z2)-1)
              in LT.injARROW(argt,result)
             end
        | f _ = (say "*** "; LT.printLty t; say "\n";    
                 error "selectLty in lambdatype 131")
      and g(nil,_) = (say "*** "; LT.printLty t; say "\n";    
                      error "selectLty in lambdatype 132")
        | g((j,s)::r,i) = if i = j then s else g(r,i)

   in f(LT.out t) 
  end

fun cvtrfty t = case LT.out t
  of LT.LISTty 1 => LT.injRECORD[LT.inj LT.RBOXEDty, LT.injBOXED]
   | LT.REFty(t,z) =>
      let val len = V.length z
          fun h i = LT.injARROW(rftynum(t,i,len-1),V.sub(z,i))
       in LT.injRECORD(map h (fromto(0,len)))
      end
  | LT.RECORDty l => LT.injRECORD(map cvtrfty l)
  | _ => t


(* cons a record type *)
fun consrfty t = case LT.out t
  of LT.RECORDty [_,z] => (case LT.out z of LT.LISTty 0 => z
				      | LT.LISTty 1 => ELISTty
				      | LT.LISTty 2 => OLISTty
				      | LT.BOTty _ => z
				      | _ => error "consrfty on a non list argument")
   | LT.BOTty _ => LT.inj(LT.BOTty ULISTty)
   | _ => error "consrfty on a non list arguments"

(* decons a list *)
fun dcnsrfty t = case LT.out t
  of LT.LISTty 0 => LT.injRECORD[LT.inj LT.RBOXEDty, ULISTty]
   | LT.LISTty 1 => LT.injRECORD[LT.inj LT.RBOXEDty, ELISTty]
   | LT.LISTty 2 => LT.injRECORD[LT.inj LT.RBOXEDty, OLISTty]
   | LT.BOTty _ =>  LT.injRECORD[LT.inj LT.RBOXEDty, t]
   | _ => error "dcnsrfty on a non list arguments"

(* check if t1 is a V.sub-type of another type t2 *)
fun subrfty(t1,t2) = case (LT.out t1, LT.out t2) 
 of (LT.BOTty t1, _) => isrfty(t2,t1)
  | (LT.RECORDty l1,LT.RECORDty l2) => andf(subrfty,l1,l2)
  | (LT.ARROWty(_,t1),LT.ARROWty(_,t2)) => subrfty(t1,t2)
  | (LT.REFty(_,z),LT.ARROWty(_,t)) => 
       formap(V.length z - 1,fn k => subrfty(V.sub(z,k),t))
  | (LT.REFty(_,z1),LT.REFty(_,z2)) =>
       formap(V.length z1 - 1,fn k => subrfty(V.sub(z1,k),V.sub(z2,k)))
  | _ => isrfty(t1,t2)

(* find out the union of two refinement type t1 and t2 *)
fun andrfty(t1,t2) = case (LT.out t1, LT.out t2)
 of (LT.LISTty i, LT.LISTty j) => if i = j then t1 else ULISTty
  | (LT.ARROWty(t11,t12),LT.ARROWty(_,t22)) => LT.injARROW(t11,andrfty(t12,t22))
  | (LT.REFty(t,z),LT.ARROWty(_,t2)) =>
         LT.inj(LT.REFty(t,V.tabulate(V.length z,fn i => andrfty(V.sub(z,i),t2))))
  | (LT.ARROWty(_,t2),LT.REFty(t,z)) =>
         LT.inj(LT.REFty(t,V.tabulate(V.length z,fn i => andrfty(V.sub(z,i),t2))))
  | (LT.REFty(t1,z1),LT.REFty(_,z2)) =>
         LT.inj(LT.REFty(t1,V.tabulate(V.length z1,fn i => andrfty(V.sub(z1,i),V.sub(z2,i)))))
  | (LT.RECORDty t1,LT.RECORDty t2) =>
         let val t = ListPair.map (fn (a,b) => andrfty(a,b)) (t1,t2)
          in LT.injRECORD t
         end
  | _ => if subrfty(t1,t2) then t2 else t1

(* find out the union of a set of refinement types *)
fun foldrfty [] = error "foldrfty in translist.sml"
  | foldrfty (a::r) = foldr andrfty a r

(****************************************************************************
 *                         COERCION FUNCTION                                *
 ****************************************************************************)

(* coerce an expression e from t1 to t2 where t1 is a refinement type of t2 *)
fun coerce(e,t1,t2) =
  if equivrfty(t1,t2) then e
  else (let fun g(LT.LISTty 1,LT.LISTty 0) = (fn e => OLIST(e))
              | g(LT.LISTty 2,LT.LISTty 0) = (fn e => ELIST(e))
              | g(LT.LISTty 0,LT.LISTty 0) = ident
              | g(LT.ARROWty(t11,t12),LT.ARROWty(t21,t22)) =
                  (if equivrfty(t11,t21) then
                     (let val x = mkLvar()
                       in (fn e => FN(x,t21,coerce(APP(e,(VAR x)),t12,t22)))
                      end)
                   else error "non refinement type coercion 123")
              | g(LT.RECORDty l1,LT.RECORDty l2) =
                  (let val bigH = map2 (fn (z1,z2) => 
                                         (fn e => coerce(e,z1,z2))) (l1,l2)
                       val v = mkLvar()
                       val nl = fromto(0,List.length(l1))
                       val base = map (fn i => SELECT(i,VAR v)) nl
                       val re = map2 force (bigH,base)
                    in (fn e => (APP(FN(v,LT.BOGUS,RECORD re),e)))
                   end)
              | g(LT.REFty(t11,z1), LT.ARROWty(t21,t22)) =
                   (if equivrfty(t11,t21)
                    then (let val t12 = V.sub(z1,0)
                              val x = mkLvar()
                           in (fn e => FN(x,t11,
                                coerce(APP(SELECT(0,e),(VAR x)),t12,t22)))
                          end)
                    else error "coercing non-refinement type 138")
              | g(LT.ARROWty(t11,t12),LT.REFty(t21,z2)) =
                   (let val len = V.length z2 
                        val l = fromto(1,len)
                        val barg = mkLvar()
                        fun pass(i,(fl,tl,el)) =
                          let val nf = mkLvar()
                              val argt = rftynum(t21,i,len-1)
                              val t22 = V.sub(z2,i)
                              val nt = LT.injARROW(argt,t22)
                              val arg = mkLvar()
                           in (nf::fl,nt::tl,
                                (FN(arg,argt,coerce(APP(VAR barg,VAR arg),t12,t22)))::el)
                          end
                        val (nfl,ntl,nel) = foldr pass ([],[],[]) l
                        val (_,_,hdr) = scandidates(t21)
                        val bf = mkLvar()
                        val arg = mkLvar()
                        val bel = map (fn v => (fn e => APP(VAR v,e))) nfl
                        val be = FN(arg,t21,hdr(bel,VAR arg))
                        val final = FIX(bf::nfl,(LT.injARROW(t21,V.sub(z2,0)))::ntl,be::nel,
                                   RECORD(map VAR (bf::nfl)))
                     in fn e => LET(barg,e,final)
                    end)
              | g(LT.REFty(t11,z1),LT.REFty(t21,z2)) =
                   (let val len = V.length z1 
                        val l = fromto(1,len)
                        val barg = mkLvar()
                        fun pass(i,(fl,tl,el)) =
                          let val nf = mkLvar()
                              val argt = rftynum(t21,i,len-1)
                              val t22 = V.sub(z2,i)
                              val nt = LT.injARROW(argt,t22)
                              val arg = mkLvar()
                           
                           in (nf::fl,nt::tl,
                             (FN(arg,argt,coerce(APP(SELECT(i,VAR barg),VAR arg),V.sub(z1,i),t22)))::el)
                          end
                        val (nfl,ntl,nel) = foldr pass ([],[],[]) l
                        val (_,_,hdr) = scandidates(t21)
                        val bf = mkLvar()
                        val arg = mkLvar()
                        val bel = map (fn v => (fn e => APP(VAR v,e))) nfl
                        val be = FN(arg,t21,hdr(bel,VAR arg))
                        val final = FIX(bf::nfl,(LT.injARROW(t21,V.sub(z2,0)))::ntl,be::nel,
                                   RECORD(map VAR (bf::nfl)))
                     in fn e => LET(barg,e,final)
                    end)
              | g(t1',t2') = 
                  if equivrfty(LT.inj t1', LT.inj t2') then ident 
                  else (case t2'
                         of LT.BOXEDty => g(t1',LT.out(top(LT.inj t1')))
                          | LT.RBOXEDty => g(t1',LT.out(top(LT.inj t1')))
                          | _ => ((* say "\n\n ";
                                  LT.printLty (LT.inj t1); say "^^^^";
                                  LT.printLty (LT.inj t2); say "^^^^\n";
                                  say "@@@coercing non-refinement types 12"; *)
                                  ident))

         in ((* LT.printLty t1; say "^^^^\n";
             LT.printLty t2; say "^^^^\n";
             MCprint.printLexp e;*) g (LT.out t1, LT.out t2) e)
        end)
                  

(****************************************************************************
 *                           MAIN FUNCTION                                  *
 ****************************************************************************)

fun translist(listcellsz,lexp) = let

(* the refinement type environment *)
exception TENV
val tenv : LT.lty Intmap.intmap = Intmap.new(32,TENV)
val getrfty = Intmap.map tenv
val addrfty = Intmap.add tenv

(****************************************************************************
 *                     FIND OUT THE REFINEMENT TYPE                         *
 ****************************************************************************)
fun g(n,VAR v) = getrfty v
  | g(n,APP(FN(v,_,e1),e2)) = (addrfty(v,g(n,e2)); g(n,e1))
  | g(n,APP(e1,e2)) = apprfty(g(n,e1),g(n,e2))
  | g(n,FN(v,t,e)) = 
      if n <= 1 then (addrfty(v,t); LT.injARROW(t,g(n,e)))
      else (let val (r,k) = candidates(t)
             in if (k<=0) then (addrfty(v,t); LT.injARROW(t,g(n,e)))
                else (let val nn = n div k
                          val nr = map (fn x => (addrfty(v,x); g(nn,e))) r
                          val sm = foldrfty nr
                       in LT.inj(LT.REFty(t,V.fromList(sm::nr)))
                      end)
            end)
  | g(n,FIX(vl,tl,el,e)) =
      let fun check(t,LT.ARROWty(a,z),FN _) =
                 if (rfable(a)) then (LT.inj(LT.BOTty t),true) else (t,false)
            | check(t,_,_) = (t,false)

          fun fixpoint l =
            foldr (fn ((_,t,nt),b) => ((equivrfty(t,nt)) andalso b)) true l

          fun loop nl =
            let val _ = app (fn (v,t,_) => addrfty(v,t)) nl
                fun f(b) =
                  if fixpoint(b) 
                  then  (* reach a fix point, get rid of LT.BOTty please *)
                   (let fun z((v,_),t,nt) = addrfty(v,flatten nt)
                     in app z b
                    end)
                  else 
                   (let fun m(u as (v,x),_,t) = 
                            (addrfty(v,t); (u,t,g(unroll_level,x)))
                     in f(map m b) 
                    end)
             in f(map (fn (v,t,x) => ((v,x),t,g(unroll_level,x))) nl)
            end

          fun noloop nl =
            let val _ = app (fn (v,t,_) => addrfty(v,t)) nl
             in app (fn (v,t,x) => addrfty(v,g(unroll_level,x))) nl
            end

          fun pass1 l =
            let fun h((v,t,e,(nt,b)),(z,flag)) = ((v,nt,e)::z,b orelse flag)
                val (nl,b) = foldr h ([],false) l
             in if b then loop nl else noloop nl
            end

          fun proc(v::r,t::z,e::y,l) = proc(r,z,y,(v,t,e,check(t, LT.out t, e))::l)
            | proc([],[],[],l) = pass1 l
            | proc _ = error "wrong FIX config2 in refine in translist"
       in (proc(vl,tl,el,[]); g(n,e))
      end
  | g(n,SWITCH(e,[LISTCONS,LISTNIL],cl,opp)) = 
      let val t = g(n,e)
          fun h(DATAcon(_, rep, _, _), e) = (rep, g(n,e))
            | h _ = error "unexpected datacon in h in switch"

          val ncl = map h cl
          val (ut,bt) = case (ncl,opp)
            of ([(LISTCONS,t1),(LISTNIL,t2)],_) => (t2,t1)
             | ([(LISTNIL,t1),(LISTCONS,t2)],_) => (t1,t2)
             | ([(LISTNIL,t1)],SOME e2) => (t1,g(n,e2))
             | ([(LISTCONS,t1)],SOME e2) => (g(n,e2),t1)
             | _ => error "strange case ub bb in translist"
       in case LT.out t of LT.LISTty 1 => bt 
                         | LT.LISTty _ => andrfty(ut,bt)
      end
  | g(n,SWITCH(e,rep,cl,opp)) = 
      let val _ = g(n,e)
       in case opp 
           of NONE => foldrfty (map (fn (_,e) => g(n,e)) cl)
            | SOME z => foldrfty ((g(n,z))::(map (fn (_,e) => g(n,e)) cl))
      end
  | g(n,CON((_, LISTCONS, _, _), e)) = consrfty(g(n,e))
  | g(n,CON((_, LISTNIL, _, _), e)) = (g(n,e); ELISTty)
  | g(n,DECON((_, LISTCONS, _, _), e)) = dcnsrfty(g(n,e))
  | g(n,DECON((_, LISTNIL, _, _), e)) = (g(n,e); LT.injINT) (* impossible case *)
  | g(n,CON((_, _, _, t), e)) = apprfty(t,g(n,e))
  | g(n,DECON((_, _, _, t), e)) = apprfty(revrfty t,g(n,e))
  | g(n,RECORD el) = LT.injRECORD (map (fn x => g(n,x)) el)
  | g(n,SRECORD el) = LT.inj(LT.SRECORDty (map (fn x => g(n,x)) el))
  | g(n,VECTOR el) = (app (fn x => (g(n,x); ())) el; LT.injBOXED)
  | g(n,SELECT(i,e)) = selrfty(i,g(n,e))
  | g(n,RAISE(e,t)) = (g(n,e); t)   (** don't know if this is right **)
  | g(n,HANDLE(e1,e2)) = 
       let val t1 = g(n,e1)
           val t2 = g(n,e2)
        in case LT.out t2 of LT.ARROWty(_,t3) => andrfty(t1,t3)
                           | _ => error "non arrow type for exception handler"
       end
  | g(n,WRAP(t,e)) = (g(n,e); LT.injBOXED)
  | g(n,UNWRAP(t,e)) = (g(n,e); t)
  | g(n,EXNF(e,_)) = (g(n,e); LT.injBOXED)
  | g(n,EXNC e) = (g(n,e); LT.injBOXED)
  | g(_,PRIM(_,t)) = t
  | g(_,INT _) = LT.injINT
  | g(_,WORD _) = LT.injINT
  | g(_,INT32 _) = (LT.inj LT.INT32ty)
  | g(_,WORD32 _) = (LT.inj LT.INT32ty)
  | g(_,REAL _) = LT.injREAL
  | g(_,STRING _) = LT.injBOXED
  | g _ = error "unexpected lambda expressions in the function g"

(* gather the refinement type information into the hash table *)
val _ = g(unroll_level,lexp)   

(*
val _ = say "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
val _ = Intmap.app (fn (v,t ) => (sayv v; say ">>>>>>"; LT.printLty t; say "\n")
                     | _ => ()) tenv
val _ = say "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
*)
fun applyexp(e1,e2,t1,t2) = case LT.out t1
   of LT.ARROWty(t0,t1) => 
        let val ne2 = coerce(e2,t2,t0)
         in (APP(e1,ne2),t1)
        end
    | LT.REFty(t1,z) =>
        let val n = rfnum(t2)
            val nt1 = rftynum(t1,n,V.length z - 1)
            val ne2 = coerce(e2,t2,nt1)
         in (APP(SELECT(n,e1),ne2), V.sub(z,n))
        end
    | _ => (APP(e1,e2),apprfty(t1,t2))

fun consexp(e,t) = case LT.out t
 of LT.RECORDty[_,t2] => (case LT.out t2
			 of LT.LISTty 0 => (CONS2 e,ULISTty)
			  | LT.LISTty 1 => (ECONS e,ELISTty)
			  | LT.LISTty 2 => (OCONS e,OLISTty)
			  | _ => error "consexp in translist")
  | _ => error "consexp in translist"

fun dcnsexp(e,t) = case LT.out t
 of LT.LISTty 0 => (dCONS2 e,LT.injRECORD[LT.injBOXED,ULISTty])
  | LT.LISTty 1 => (dOCONS e,LT.injRECORD[LT.injBOXED,ELISTty])
  | LT.LISTty 2 => (dECONS e,LT.injRECORD[LT.injBOXED,OLISTty])
  | _ => error "dcnsexp in translist"


(****************************************************************************
 *  A substitution map, useful for the refinement-type based transformation *
 ****************************************************************************)
exception Rename
val mmm : lexp Intmap.intmap = Intmap.new(32, Rename)
fun subst v = (Intmap.map mmm v) handle Rename => VAR v
val bind = Intmap.add mmm

fun transpath (PATH(i,r)) = PATH(i,transpath r)
  | transpath (LVAR v) = LVAR (case (subst v) of VAR x => x
                                | _ => error "transpath 3721 in translist")
  | transpath _ = error "transpath in translist.sml 745"

fun transrep (EXNFUN p) = EXNFUN(transpath p)
  | transrep (EXNCONST p) = EXNFUN(transpath p)
  | transrep r = r

fun transdcon (DATAcon(s,r,t_op,t)) = DATAcon(s,transrep r,t_op,t)
  | transdcon c = c
  
(****************************************************************************
 *              THE REFINEMENT-TYPE BASED TRANSFORMATION                    *
 ****************************************************************************)
fun m(n,le) = 
  (case le 
    of VAR v => (subst v, getrfty v)
     | APP(FN(v,_,e1),e2) => 
         let val (ne2,nt2) = m(n,e2)
             val _ = addrfty(v,nt2)
             val nv = dupLvar(v)
             val _ = bind(v,VAR nv)
             val (ne1,nt1) = m(n,e1)
          in (APP(FN(nv,LT.BOGUS,ne1),ne2),nt1)
         end
     | APP(e1,e2) => 
         let val (ne1,nt1) = m(n,e1)
             val (ne2,nt2) = m(n,e2)
          in applyexp(ne1,ne2,nt1,nt2) 
         end
     | FN(v,t,e) => 
         if n <= 1 then 
           (let val _ = addrfty(v,t)
                val nv = dupLvar(v)
                val _ = bind(v,VAR nv)
                val (ne,nt) = m(n,e)
             in (FN(nv,t,ne),LT.injARROW(t,nt))
            end)
         else 
           (let val (r,k,hdr) = scandidates(t)
             in if (k<=0) then 
                  (let val _ = addrfty(v,t)
                       val nv = dupLvar(v)
                       val _ = bind(v,VAR nv)
                       val (ne,nt) = m(n,e)
                    in (FN(nv,t,ne),LT.injARROW(t,nt))
                   end)
                else  
                  (let val nn = n div k
                       fun each (x,(vl,tl,ttl,el)) = 
                         let val _ = addrfty(v,x)
                             val nv = dupLvar(v)
                             val _ = bind(v,VAR nv)
                             val nf = mkLvar()
                             val (nx,nt) = m(nn,e)
                          in (nf::vl,nt::tl,(LT.injARROW(x,nt))::ttl,
                              (FN(nv,x,nx))::el)
                         end
                       val (vl,tl,ttl,el) = foldr each ([],[],[],[]) r
                       val resrfty = foldrfty tl
                       val bf = mkLvar()
                       val arg = mkLvar()
                       val bel = map2 (fn (v,t) =>
                                   (fn e => coerce(APP(VAR v,e),t,resrfty)))
                                   (vl,tl)
                       val be = FN(arg,t,hdr(bel,VAR arg))
                       
                       val finalt = LT.inj(LT.REFty(t,V.fromList(resrfty::tl)))
                                             
                       val final = FIX(bf::vl,(LT.injARROW(t,resrfty))::ttl,be::el,
                              RECORD (map VAR (bf::vl)))

                    in (final,finalt)
                   end)
           end)
     | FIX(vl,tl,el,be) =>
         let fun proc(v::r,t::z,e::y,ww,uu) = 
                  let val nt = getrfty v
                   in case LT.out nt 
                       of LT.REFty _ => proc(r,z,y,(nt,(v,t,e))::ww,uu)
                        | LT.ARROWty _ => proc(r,z,y,ww,(v,nt,e)::uu)
                  end
               | proc([],[],[],ww,uu) = (ww,uu)
               | proc _ = error "wrong FIX 243 in refine in translist"

             val (rfl,ufl) = proc(vl,tl,el,[],[])
             val ufl' = map (fn (v,t,e) => (let val w = dupLvar(v)
                                                val _ = addrfty(v,t)
                                                val _ = bind(v,VAR w)
                                             in (w,t,e)
                                            end)) ufl

             val rfl' = 
               let fun loop(t0,(v,_,e)) =
			case LT.out t0
			 of LT.REFty(t,tv) =>
	                         let val w = dupLvar(v)
	                             val (r,k,hdr) = scandidates(t)
	                             fun s(x,(n,vl,tl,rtl)) = 
	                               let val z = mkLvar()
	                                   val x2 = V.sub(tv,n)
	                                   val t = LT.injARROW(x,x2)
	                                   val _ = addrfty(z,t)
	                                in (n-1,z::vl,t::tl,x2::rtl)
	                               end
	                             val (_,vl,ttl,rtl) = 
						foldr s (V.length tv - 1,[],[],[]) r
	                             val _ = bind(v,RECORD(map VAR(w::vl)))
	                          in (w,t,tv,r,vl,hdr,e,k,ttl,rtl)
	                         end
        	         | _ => error "refinement type in loop FIX in 424"
                in map loop rfl
               end

             (*** processing non-refinable functions ***)

             val (ufl,utl,uel) = 
               foldr (fn ((w,t,e),(fl,tl,el)) => 
                      let val (ne,_) = m(unroll_level,e)
                       in (w::fl,t::tl,ne::el)
                      end) ([],[],[]) ufl'

             (*** processing refinable functions ***)

             val (rfl,rtl,rel) = 
               foldr (fn ((w,t,tv,r,vl,hdr,FN(z,_,e),k,ttl,rtl),(fl,tl,el)) =>
                  (let fun each (x,el) = 
                         let val _ = addrfty(z,x)
                             val nz = dupLvar(z)
                             val _ = bind(z,VAR nz)
                             val (ne,nt) = m(unroll_level div k,e)
                          in (FN(nz,x,ne))::el
                         end
                       val ell = foldr each [] r
           
                       val fl' = w::vl
                       val tl' = ((LT.injARROW(t,V.sub(tv,0)))::ttl)
                       val resrfty = V.sub(tv,0)
                       val www = mkLvar()
                       val bel = map2 (fn(v,t) =>
                                   (fn e => coerce(APP(VAR v,e),t,resrfty))) 
                                   (vl, rtl)
                       val el' = (FN(www,t,hdr(bel,VAR www)))::ell
                    in (fl'@fl,tl'@tl,el'@el)
                   end)) (ufl,utl,uel) rfl'

             val (nbe,nbt) = m(n,be)
          in (FIX(rfl,rtl,rel,nbe), nbt)
         end
     | SWITCH(e,[LISTCONS,LISTNIL],cl,opp) =>
         let val (ne,t) = m(n,e)
             fun k(c as DATAcon(_, rep, _, _),x) = 
                      let val (nx,nt) = m(n,x)
                       in (rep,nx,nt)
                      end
               | k _ = error "unexpected datacon in k for switch"
             val ncl = map k cl

             val (ue,be) = case (ncl,opp)
               of ([q1 as (LISTCONS,_,_),q2 as (LISTNIL,_,_)],_) => (q2,q1)
                | ([q1 as (LISTNIL,_,_),q2 as (LISTCONS,_,_)],_) => (q1,q2)
                | ([q1 as (LISTNIL,_,_)],SOME e2) => 
                     (let val (ne2,nt2) = m(n,e2)
                       in (q1,(LISTCONS,ne2,nt2))
                      end)
                | ([q1 as (LISTCONS,_,_)],SOME e2) => 
                     (let val (ne2,nt2) = m(n,e2)
                       in ((LISTNIL,ne2,nt2),q1)
                      end)
                | _ => error "strange case ub bb in translist"

          in case LT.out t 
              of LT.LISTty 1 => 
                   (let val z = mkLvar()
                     in (LET(z,ne,#2 be),#3 be)
                    end)
               | LT.LISTty 2 => 
                   (let val nt = andrfty(#3 be,#3 ue)
                     in (switch_elist(ne,coerce(#2 ue,#3 ue,nt),
                                         coerce(#2 be,#3 be,nt)), nt)
                    end)
               | _ => (let val nt = andrfty(#3 be,#3 ue)
                           val nbe = coerce(#2 be,#3 be,nt)
                           val nue = coerce(#2 ue,#3 ue,nt)
                           val z = mkLvar() and f = mkLvar() and y = mkLvar()
                           fun hdr le = LET(f,FN(y,LT.injINT,nbe),le)
                           val eb = APP(VAR f,INT 0)
                           val ee = switch_elist(dELIST(VAR z),nue,eb)
                        in (LET(z,ne,hdr(switch_ulist(VAR z,eb,ee))), nt)
                       end)
         end
     | SWITCH(e, rep, cl, opp) =>
         let val (ne,_) = m(n,e)
             fun k((c,x),(uu,ww)) = let val (nx,nt) = m(n,x)
                                   in ((c,nx,nt)::uu,nt::ww)
                                  end
             val (ncl,ntl) = foldr k ([],[]) cl
             val (nopp,mt) = case opp
               of NONE => (NONE,foldrfty ntl)
               | SOME z => (let val (nz,nzt) = m(n,z)
                                val nt = foldrfty(nzt::ntl)
                             in (SOME (coerce(nz,nzt,nt)), nt)
                            end)
             val ncl = map (fn (c,x,t) => (transdcon c,coerce(x,t,mt))) ncl
             val nrep = map transrep rep
          in (SWITCH(ne,nrep,ncl,nopp), mt)
         end
     | CON((_, LISTCONS, _, _), e) => consexp(m(n,e))
     | CON((_, LISTNIL, _, _), e) => 
         let val (ne,_) = m(n,e)
          in (TNIL(ne),ELISTty)
         end
     | DECON((_, LISTCONS, _, _),e) => dcnsexp(m(n,e))
     | DECON((_, LISTNIL, _, _),e) => 
         let val (ne,_) = m(n,e)
          in (dTNIL(ne),LT.injINT)
         end
     | CON(c as (s,rep,t_op,t),e) => 
         let val (ne,nt) = m(n,e)
             val nc = (s, transrep rep, t_op, t)  (* ZHONG? *)
          in (CON(nc,ne),apprfty(t,nt))
         end
     | DECON(c as (s,rep,t_op,t),e) => 
         let val (ne,nt) = m(n,e)
             val nc = (s, transrep rep, t_op, t)  (* ZHONG? *)
          in (DECON(nc,ne),apprfty(revrfty t,nt))
         end
     | RECORD el => 
         let fun p(e,(el,tl)) = let val (ne,nt) = m(n,e) in (ne::el,nt::tl) end
             val (nel,ntl) = foldr p ([],[]) el
          in (RECORD nel,LT.injRECORD ntl)
         end
     | SRECORD el => 
         let fun p(e,(el,tl)) = let val (ne,nt) = m(n,e) in (ne::el,nt::tl) end
             val (nel,ntl) = foldr p ([],[]) el
          in (SRECORD nel,LT.inj(LT.SRECORDty ntl))
         end
     | VECTOR el => 
         let fun p(e,(el,tl)) = let val (ne,nt) = m(n,e) in (ne::el,nt::tl) end
             val (nel,_) = foldr p ([],[]) el
          in (VECTOR nel,LT.injBOXED)
         end
     | SELECT(i,e) => 
         let val (ne,nt) = m(n,e)
             val ct = selrfty(i,nt)
          in (SELECT(i,ne),ct)
         end
     | RAISE(e,t) => 
         let val (ne,_) = m(n,e)
          in (RAISE(ne,t),t)
         end
     | HANDLE(e1,e2) => 
         let val (ne1,nt1) = m(n,e1)
             val (ne2,nt2) = m(n,e2)
          in (case LT.out nt2 
               of LT.ARROWty(a,b) => 
                  (let val nt = andrfty(nt1,b)
                       val ne1 = coerce(ne1,nt1,nt)
                       val ne2 = coerce(ne2,nt2,LT.injARROW(a,nt))
                    in (HANDLE(ne1,ne2),nt)
                   end)
                | _ => error "non-arrow ty in HANDLE in translist")
         end
     | WRAP(t,e) => 
         let val (ne,nt) = m(n,e)
             val ne = coerce(ne,nt,t)
          in (WRAP(t,ne),LT.injBOXED)
         end
     | UNWRAP(t,e) => 
         let val (ne,nt) = m(n,e)
          in (UNWRAP(t,ne),t)
         end
     | EXNF(e,t) => 
         let val (ne,nt) = m(n,e)
             val ne = coerce(ne,nt,t)
          in (EXNF(ne,t),LT.injBOXED)
         end
     | EXNC e => 
         let val (ne,nt) = m(n,e)
          in (EXNC ne,LT.injBOXED)
         end
     | PRIM(_,t) => (le,t)
     | INT _ => (le,LT.injINT)
     | WORD _ => (le,LT.injINT)
     | INT32 _ => (le,(LT.inj LT.INT32ty))
     | WORD32 _ => (le,(LT.inj LT.INT32ty))
     | REAL _ => (le,LT.injREAL)
     | STRING _ => (le,LT.injBOXED)
     | _ => error "unexpected lambda expressions in the function m")

 in #1 (m (unroll_level,lexp)) (* h lexp  *)
end


end (* local *)
end (* structure TransList *)

(*
 * $Log: translist.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:47  george
 *   Version 109.24
 *
 *)
