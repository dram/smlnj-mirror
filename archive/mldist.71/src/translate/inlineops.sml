structure InLineOps : INLINE_OPS =
   struct
      structure Lambda = Lambda
      open Access Absyn Lambda Basics BasicTypes

      fun transDcon(DATACON{name,rep,...}) = (name,rep)
      val trueDcon' = transDcon trueDcon
      val falseDcon' = transDcon falseDcon

      val unitLexp = RECORD[]
      val conToLexp =
         fn (d as DATACON{const=true,...}) => CON'(transDcon d,unitLexp)
          | (d as DATACON{const=false,...}) => 
            let val v = mkLvar () in FN(v,CON'(transDcon d,VAR v)) end

      val COND = fn (a : lexp,b : lexp, c : lexp) => 
              SWITCH(a,boolsign,
                    [(DATAcon(trueDcon'),b),
                     (DATAcon(falseDcon'),c)],NONE)
      val LET = fn (v : lvar, e : lexp, b : lexp) => APP(FN(v,b),e)

(* Functions to generate lambda language expressions for the following
   ML functions *)

(*  val op sub : 'a array * int -> 'a =
	  fn (a,i) =>
	     if lessu(i, alength a) then subscript(a, i) else raise Subscript
*)
  
      val inlsubscript = fn () =>
        let val p = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val va = VAR a
            val vi = VAR i
        in FN(p,
            LET(a,SELECT(0,vp),
             LET(i,SELECT(1,vp),
                COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.slength,va)]),
                     APP(PRIM P.subscript,RECORD[va,vi]),
                     RAISE(conToLexp (!CoreInfo.exnSubscript))))))
        end

(*  val update : 'a array * int * 'a -> unit =
	  fn (a,i,v) => 
	     if lessu(i, alength a) then update(a, i, v) else raise Subscript
*)

      val inlupdate = fn ty =>
        let val t = mkLvar()
            val a = mkLvar()
            val i = mkLvar()
            val v = mkLvar()
            val vt = VAR t
            val va = VAR a
            val vi = VAR i
            val vv = VAR v
            val oper = case ty
                       of NONE => P.update
                        | SOME t => Unboxed.unboxedUpdate t 
        in FN(t,
              LET(a,SELECT(0,vt),
               LET(i,SELECT(1,vt),
                LET(v,SELECT(2,vt),
                 COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.alength,va)]),
                    APP(PRIM oper,RECORD[va,vi,vv]),
                    RAISE(conToLexp (!CoreInfo.exnSubscript)))))))
        end

(* Bytearray subscripting:

    val op sub = fn (s, i) =>
          if lessu(i, blength s) then byteof(s, i) else raise Subscript
*)

    val inlbyteof = fn () =>
        let val p = mkLvar()
            val s = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val vs = VAR s
            val vi = VAR i
        in FN(p,
            LET(s,SELECT(0,vp),
             LET(i,SELECT(1,vp),
                COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.slength,vs)]),
                     APP(PRIM P.ordof,RECORD[vs,vi]),
                     RAISE(conToLexp (!CoreInfo.exnOrd))))))
        end
 
(* Bytearray store:

   fun update(arg as (s,i,c)) =
	if i<0 orelse i >= blength s then raise Subscript
	else if c<0 orelse c>255 then raise Range
	else InLine.store arg
*)

      val inlstore = fn () =>
        let val t = mkLvar()
            val s = mkLvar()
            val i = mkLvar()
            val c = mkLvar()
            val vt = VAR t
            val vs = VAR s
            val vi = VAR i
            val vc = VAR c
        in FN(t,
              LET(s,SELECT(0,vt),
               LET(i,SELECT(1,vt),
                LET(c,SELECT(2,vt),
                 COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.slength,vs)]),
                   COND(APP(PRIM P.lessu,RECORD[vc,INT 256]),
                        APP(PRIM P.store,RECORD[vs,vi,vc]),
                        RAISE(conToLexp (!CoreInfo.exnRange))),
                   RAISE(conToLexp (!CoreInfo.exnOrd)))))))
        end

(*  String ordof:

    val ordof = fn (s,i) =>
	  if boxed s
            then if lessu(i, slength s) then ordof(s, i) else raise Ord
	    else if ieql(i,0) then cast s else raise Ord
*)

      val inlordof = fn () =>
        let val p = mkLvar()
            val s = mkLvar()
            val i = mkLvar()
            val vp = VAR p
            val vs = VAR s
            val vi = VAR i
        in FN(p,
            LET(s,SELECT(0,vp),
             LET(i,SELECT(1,vp),
               COND(APP(PRIM P.boxed,vs),
                 COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.slength,vs)]),
                     APP(PRIM P.ordof,RECORD[vs,vi]),
                     RAISE(conToLexp (!CoreInfo.exnOrd))),
                 COND(APP(PRIM P.ieql,RECORD[vi,INT 0]),
                      vs,
                      RAISE(conToLexp (!CoreInfo.exnOrd)))))))
         end


(* 
 * inlsubscriptf = fn(a,i)=> if lessu(i,slength a div 8) then subscriptf(a,i)
 *			     else raise Vector.Subscript
 *)
      val inlsubscriptf = fn () => 
	  let val p = mkLvar()
	      val a = mkLvar()
	      val i = mkLvar()
	      val vp = VAR p
	      val va = VAR a
	      val vi = VAR i
	  in FN(p,
  	      LET(a,SELECT(0,vp),
               LET(i,SELECT(1,vp),
	        COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.rshift,
						    RECORD[APP(PRIM P.slength,va),
							   INT 3])]),
		     APP(PRIM P.subscriptf,RECORD[va,vi]),
		     RAISE(conToLexp(!CoreInfo.exnFpSubscript))))))
	  end
	      
(*
 * val inlupdatef = fn(a,i,v)=> if lessu(i,slength a div 8) then updatef(a,i,v) 
 *				else raise FpSubscript
 *)
      val inlupdatef = fn () =>
	  let val t = mkLvar()
	      val a = mkLvar()
	      val i = mkLvar()
	      val v = mkLvar()
	      val vt = VAR t
	      val va = VAR a
	      val vi = VAR i
	      val vv = VAR v
	  in FN(t,
	      LET(a,SELECT(0,vt),
               LET(i,SELECT(1,vt),
                LET(v,SELECT(2,vt),
	          COND(APP(PRIM P.lessu,RECORD[vi,APP(PRIM P.rshift,
						      RECORD[APP(PRIM P.slength,va),
							     INT 3])]),
		       APP(PRIM P.updatef,RECORD[va,vi,vv]),
		       RAISE(conToLexp(!CoreInfo.exnFpSubscript)))))))
	  end
   end

