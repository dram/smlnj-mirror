(* Copyright 1996 by Bell Laboratories *)
(* modinline.sml *)

(* Cross-module inline expansion *)

signature MODINLINE =
sig
    val export : CPS.function * LambdaType.lty list -> CPS.function
    val import : {body: CPS.function, imports : CPS.function list} =>
	           CPS.function list
end

structure ModInline : MODINLINE =
struct

   open CPS
   structure LT = LambdaType

   val mkLvar = LambdaVar.mkLvar()

   fun import {body=(bk,bf,[bc,ba],bt,be),imports} =
     let val ba' = mkLvar()
         fun rebind(n,vars,nil) = 
	         RECORD(RK_VECTOR,
			map (fn x => (x,OFFp 0)) (rev vars),
			ba, be)
           | rebind(i,vars,imp as (_,f,_,_,_)::rest) =
	     let val v = mkLvar() and k = mkLvar() and x = mkLvar()
		 val exp = rebind(i+1,VAR x :: vars,rest)
	      in FIX([imp,
		       (CONT,k,[x],[PTRt], exp)],
		      PURE(P.subscriptv,[VAR ba', INT i],v,PTRt,
			   APP(VAR f, [VAR k, VAR v])))
             end
       in (bk,bf,[bc,ba'],bt, rebind(0,nil,imports)
      end


   fun export1(body as (bk,bf,[bc,ba],bt,be), index, LT.INTty) =


      let 
	  val function = 
	     (bk,bf',[bc',ba'],bt,
	      FIX([(CONT,k,[x],[PTRt?],
		    PURE(P.subscriptv,[VAR x, INT index],v,PTRt,
			 APP(VAR bc', [VAR v]))),
		   body],
		  APP(bf,[VAR k, VAR ba'])))

          val function' = contract function

       in if isgood function' then          

   fun export (body as (bk,bf,[bc,ba],bt,be), ltys) =
     let val ba' = mkLvar()
	 fun expo(i, vars, nil) =
	         RECORD(RK_VECTOR,
			map (fn x => (x,OFFp 0)) (rev vars),
			ba, be)
	   | expo(i, vars, t::rest) =
	     let val v = mkLvar()
		 val exp = expo(i+1,v::vars,rest)
 	      in PURE(P.subscriptv,[VAR ba', INT i],v,PTRt,exp)
             end
      in expo(0,nil,ltys)
     end
end

(*
 * $Log: modinline.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:32  george
 *   Version 109.24
 *
 *)
