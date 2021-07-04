(* recover.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* recover the type information of a closed FLINT program *)
signature RECOVER =
sig
  val recover : (FLINT.prog * bool) ->
                  {getLty: FLINT.value -> FLINT.lty,
                   cleanUp: unit -> unit,
		   addLty: (FLINT.lvar * FLINT.lty) -> unit}
end (* signature RECOVER *)

structure Recover : RECOVER =
struct

local
  structure LT = LtyExtern
  structure DI = DebIndex
  structure LV = LambdaVar
  structure F = FLINT
  structure PP = PrettyPrint

  open FLINT

val debugging = FLINT_Control.rcdebugging
fun bug s = ErrorMsg.impossible ("Recover: " ^ s)
fun say msg = (Control.Print.say msg; Control.Print.flush ())
fun says msgs = say (concat msgs)
fun saynl msg = (say (msg ^ "\n"))
fun dbsay msg = if !debugging then say msg else ()
fun dbsays msgs = if !debugging then says msgs else ()
fun dbsaynl msg = if !debugging then saynl msg else ()

fun ppLty (lty : lty) =
    PP.with_default_pp
	(fn ppstrm => PPLty.ppLty 1000 ppstrm lty)

in (* local *)

fun ltInst (lt, ts) =
  (case LT.lt_inst(lt, ts)
    of [x] => x
     | _ => bug "unexpected case in ltInst")

(** these two functions are applicable to the types of primops and data
    constructors only (ZHONG) *)
fun arglty (lt, ts) =
  let val (_, atys, _) = LT.ltd_arrow(ltInst(lt, ts))
   in case atys of [x] => x
                 | _ => bug "unexpected case in arglty"
  end
fun reslty (lt, ts) =
  let val (_, _, rtys) = LT.ltd_arrow(ltInst(lt, ts))
   in case rtys of [x] => x
                 | _ => bug "unexpected case in reslty"
  end

fun check (test, msg) =
    if !debugging andalso not test
    then says ["!!! ", msg, "\n"]
    else ()

exception RecoverLty
fun recover (fdec: F.prog, postRep: bool) =
  let val ltyTable : lty LV.Tbl.hash_table = LV.Tbl.mkTable(32, RecoverLty)

      (* getLvar : LV.lvar -> lty *)
      fun getLvar (lv: LV.lvar): lty =
	   let val _ = dbsays [">>> : ", LV.toString lv, " --> "]
	       val lty = LV.Tbl.lookup ltyTable lv
			 handle RecoverLty =>
		           bug ("recover..getLvar: unbound lvar: " ^ LV.prLvar lv)
	    in if !debugging then ppLty lty else ();
	       lty
	   end

      (* addLvar : LV.lvar * lty -> unit *)
      fun addLvar (arg as (lvar: LV.lvar, lty: lty)) : unit =
	  (if !debugging
	   then (says ["+++ ", LV.toString lvar, " : "];
		 ppLty lty)
	   else ();
	   LV.Tbl.insert ltyTable arg)

      (* addLvars: (LV.lvar * lty) list -> unit *)
      fun addLvars vts = app addLvar vts

      (* getlty : FLINT.value -> lty *)
      fun getlty ((VAR lvar): F.value): lty = getLvar lvar
        | getlty (INT{ty, ...}) = LT.ltc_num ty
        | getlty (WORD{ty, ...}) = LT.ltc_num ty
        | getlty (REAL _) = LT.ltc_real
        | getlty (STRING _) = LT.ltc_string

      val lt_nvar_cvt = LT.lt_nvar_cvt_gen()

      (* lexpToLtys : lexp -> lty list *)
      fun lexpToLtys e =
        let (* lpv : F.value -> lty *)
	    fun lpv (u: F.value) = getlty u
					  
            (* lpvs : F.value list -> lty list *)
            fun lpvs vs = map lpv vs

            (* lpd : fundec/prog -> unit *)
            fun lpd (fkind, flvar, vts, e): unit =
              (addLvars vts; addLvar (flvar, LT.ltc_fkfun(fkind, map #2 vts, lpe e)))

            (* lpds : fundec list -> unit *)
            and lpds (fds as ((fk as {isrec=SOME _, ...},_,_,_)::_)) =
                  let fun h ((fk as {isrec=SOME (rts,_), ...},
                             f, vts, _) : fundec) =
                            addLvar(f, LT.ltc_fkfun(fk, map #2 vts, rts))
                        | h _ = bug "unexpected case in lpds"
                      val _ = app h fds
                   in app lpd fds
                  end
              | lpds [fd] = lpd fd
              | lpds _ = bug "unexpected case 2 in lpds"

            (* lpc : con * lexp -> ??? *)
            and lpc (DATAcon((_,_,lt), ts, v), e) =
                  (addLvar (v, arglty(lt, ts)); lpe e)
              | lpc (_, e) = lpe e

	    (* lpe : lexp -> lty list *)
            and lpe (RET vs : F.lexp) : lty list = lpvs vs
              | lpe (LET(vs, e1, e2)) =
		  let val e1Ltys = lpe e1
		  in check (length vs = length e1Ltys, "lpe[LET]");
                     addLvars (ListPair.zipEq (vs, e1Ltys));
		     lpe e2
		  end
              | lpe (FIX(fdecs, e)) = (lpds fdecs; lpe e)
              | lpe (APP(u, vs)) =
		  let val u' = lpv u
		  in (#2(LT.ltd_fkfun u')
		      handle LT.DeconExn =>
		       (print "\nError Application:\n";
			PPFlint.printLexp (APP(u, vs));
			raise LT.DeconExn))
		  end
              | lpe (TFN((tfk, v, tvks, e1), e2)) =
                  (addLvar(v, LT.lt_nvpoly(tvks, lexpToLtys e1));
                   lpe e2)
              | lpe (TAPP(v, ts)) = LT.lt_inst (lpv v, ts)
              | lpe (RECORD(rk,vs,v,e)) =
                  (addLvar (v, LT.ltc_rkind(rk, lpvs vs)); lpe e)
              | lpe (SELECT(u,i,v,e)) =
                  (addLvar (v, LT.ltd_rkind(lpv u, i)); lpe e)
              | lpe (CON((_,_,lt),ts,_,v,e)) =
                  (addLvar (v, reslty(lt, ts)); lpe e)
              | lpe (SWITCH(_, _, ces, e)) =
                  let val lts = map lpc ces
                   in case e of NONE => hd lts
                              | SOME e => lpe e
                  end
              | lpe (RAISE (_, lts)) = lts
              | lpe (HANDLE(e, _)) = lpe e
              | lpe (BRANCH(p, _, e1, e2)) =
                  let val _ = lpe e1
                   in lpe e2
                  end
              | lpe (PRIMOP((_,Primop.WCAST, lt, []), _, v, e)) =
                  if postRep then
                     (case LT.ltd_fct lt
                       of ([_],[r]) => (addLvar(v, r); lpe e)
                        | _ => bug "unexpected case for WCAST")
                  else bug "unexpected primop WCAST in recover"
              | lpe (PRIMOP((_,_,lt,ts), _, v, e)) =
                  (addLvar (v, reslty (lt, ts)); lpe e)

         in lpe e handle LT.DeconExn => (print "\nWhole Expr:\n";
					 PPFlint.printLexp e; bug "ltd decon")
        end (* function lexpToLtys *)

      val (fkind, f, vts, e) = fdec
      val _ = addLvars vts
      val atys = map #2 vts    (* argument ltys *)
      (* val _ = PPFlint.printLexp e *)
      val rtys = lexpToLtys e  (* result ltys *)
      val _ = addLvar (f, LT.ltc_fkfun(fkind, atys, rtys))
  in {getLty=getlty, cleanUp=fn () => LV.Tbl.clear ltyTable, addLty=addLvar}
 end (* function recover *)

end (* local *)
end (* structure Recover *)
