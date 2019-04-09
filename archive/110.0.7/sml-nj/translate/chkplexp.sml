(* Copyright 1996 by AT&T Bell Laboratories *)
(* chkplexp.sml *)

signature CHKPLEXP = 
sig 

val checkLty : PLambda.lexp * int -> bool
val newlam_ref : PLambda.lexp ref
val fname_ref : string ref

end (* signature CHKPLEXP *)

structure ChkPlexp : CHKPLEXP = 
struct

local structure LK = LtyKernel 
      structure LT = LambdaType
      structure LE = LtyEnv
      structure LV = LambdaVar
      structure DA = Access 
      structure DI = DebIndex
      open PLambda 
in

(*** a hack of printing diagnostic output into a separate file ***) 
val newlam_ref : PLambda.lexp ref = ref (RECORD[])
val fname_ref : string ref = ref "yyy"

fun bug s = ErrorMsg.impossible ("CheckLty: "^s)
val say = Control.Print.say

val anyerror = ref false
val clickerror = fn () => (anyerror := true)

(****************************************************************************
 *                         BASIC UTILITY FUNCTIONS                          *
 ****************************************************************************)
fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"

fun simplify(le,0) = STRING "<dummy>"
  | simplify(le,n) = 
      let fun h le = simplify(le, n-1)
       in case le 
           of FN(v, t, e) => FN(v, t, h e)
            | APP(e1, e2) => APP(h e1, h e2)
            | LET(v, e1, e2) => LET(v, h e1, h e2)
            | TFN(ks, e) => TFN(ks, h e)
            | TAPP(e, ts) => TAPP(h e, ts)
            | PACK(lt, ts, nts, e) => PACK(lt, ts, nts, h e)
            | CON(l, x, e) => CON(l, x, h e)
            | DECON(l, x, e) => DECON(l, x, h e)
            | FIX(lv, lt, le, b) => FIX(lv, lt, map h le, h b)
            | SWITCH(e, l, dc, opp) => 
               (let fun g(c, x) = (c, h x)
                    fun f x = case x of SOME y => SOME(h y) | NONE => NONE
                 in SWITCH(h e, l, map g dc, f opp)
                end)
            | RECORD e => RECORD (map h e)
            | SRECORD e => SRECORD (map h e)
            | VECTOR(e, x) => VECTOR (map h e, x)
            | SELECT(i, e) => SELECT(i, h e)
            | HANDLE(e1, e2) => HANDLE(h e1, h e2)
            | WRAP(t, b, e) => WRAP(t, b, h e)
            | UNWRAP(t, b, e) => UNWRAP(t, b, h e)
            | _ => le
      end (* end of simplify *)

(** utility functions for printing *)
val tkPrint = say o LT.tk_print
val tcPrint = say o LE.tcPrint
val ltPrint = say o LE.ltPrint
fun lePrint le = PPLexp.printLexp (simplify(le, 3))

(*** a hack for type checking ***)
fun laterPhase i = (i > 20)

(****************************************************************************
 *           MAIN FUNCTION --- val checkLty : PLambda.lexp -> bool          *
 ****************************************************************************)
fun checkLty (lexp, phase) = 
let 

val ltEquiv = LE.ltEqvGen phase
val ltString = if laterPhase(phase) then LT.ltc_void else LT.ltc_string
val ltExn = if laterPhase(phase) then LT.ltc_void else LT.ltc_exn
val ltIexn = if laterPhase(phase) then LT.ltc_void else LT.ltc_iexn
fun ltVector t = if laterPhase(phase) then LT.ltc_void
                 else LT.ltc_tyc(LT.tcc_app(LT.tcc_vector,[t]))

(** utility functions for type checking *)
fun ltTyApp le s (lt, ts, kenv) = 
      ((LE.ltAppChk(lt, ts, kenv))
       handle zz => 
       (clickerror ();
        say (s ^ "  **** Kind conflicting in lexp =====> \n    ");
        case zz of LE.LtyAppChk => say "      exception LtyAppChk raised! \n"
                 | LE.TkTycChk =>  say "      exception TkTycChk raised! \n"
                 | _ => say "   other weird exception raised! \n";
        say "\n \n"; lePrint le; say "\n For Types: \n";  
        ltPrint lt; say "\n and   \n    "; 
        app (fn x => (tcPrint x; say "\n")) ts;   say "\n \n";  
        say "***************************************************** \n"; 
        bug "fatal typing error in ltTyApp"))

fun ltMatch le s (t1, t2) = 
  (if ltEquiv(t1,t2) then ()
   else (clickerror();
         say (s ^ "  **** Lty conflicting in lexp =====> \n    ");
         ltPrint t1; say "\n and   \n    "; ltPrint t2;
         say "\n \n";  PPLexp.printLexp le;
         say "***************************************************** \n"))
  handle zz => 
  (clickerror();
   say (s ^ "  **** Lty conflicting in lexp =====> \n    ");
   say "uncaught exception found ";
   say "\n \n";  PPLexp.printLexp le; say "\n";  
   ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n";  
   say "***************************************************** \n")

fun ltFnApp le s (t1, t2) = 
  let val (a1, b1) = 
        ((LE.ltArrow t1) handle zz =>
            (clickerror ();
             say (s ^ "  **** Applying Non-Arrow Type in lexp =====> \n    ");
             case zz of LE.LtyArrow => say "exception LtyArrow raised. \n"
                      | LK.tcUnbound => say "exception tcUnbound raised. \n"
                      | _ => say "other weird exceptions raised\n";
             say "\n \n";  lePrint le; say "\n For Types \n";
             ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n \n";  
             say "***************************************************** \n"; 
             bug "fatal typing error in ltFnApp"))

   in ltMatch le s (a1, t2); b1
  end

fun ltFnAppR le s (t1, t2) =  (*** used for DECON lexps ***)
  let val (a1, b1) = 
        ((LE.ltArrow t1) handle zz => 
            (clickerror ();
             say (s ^ "  **** Rev-Apply Non-Arrow Type in lexp =====> \n    ");
             case zz of LE.LtyArrow => say "exception LtyArrow raised. \n"
                      | LK.tcUnbound => say "exception tcUnbound raised. \n"
                      | _ => say "other weird exceptions raised\n";
             say "\n \n";  lePrint le; say "\n For Types \n";
             ltPrint t1; say "\n and   \n    "; ltPrint t2; say "\n \n"; 
             say "***************************************************** \n"; 
             bug "fatal typing error in ltFnApp"))

   in ltMatch le s (b1, t2); a1
  end

fun ltSelect le s (lt, i) = 
  ((LE.ltSelect(lt, i))
     handle zz => 
       (clickerror ();
        say (s ^ "  **** Select from a wrong-type lexp  =====> \n    ");
        case zz of LE.LtySelect => say "exception LtyArrow raised. \n"
                 | LK.tcUnbound => say "exception tcUnbound raised. \n"
                 | _ => say "other weird exceptions raised\n";
        say "\n \n";  lePrint le; say "\n \n";
        say "Selecting "; say (Int.toString i); 
        say "-th component from the type: \n     "; ltPrint lt; say "\n \n "; 
        say "***************************************************** \n"; 
        bug "fatal typing error in ltSelect"))

(** ltConChk currently does not check the case for DATAcon *)
(** Of course, we could easily check for monomorphic DATAcons *)
fun ltConChk le s (DATAcon _, lt) = ()
  | ltConChk le s (c, lt) = 
      let val nt = (case c of INT32con _ => LT.ltc_int32
                            | WORD32con _ => LT.ltc_int32
                            | REALcon _ => LT.ltc_real
                            | STRINGcon _ => ltString
                            |  _ => LT.ltc_int)
       in ltMatch le s (nt, lt)
      end


(** check : tkindEnv * ltyEnv * DI.depth -> lexp -> lty *)
fun check (kenv, venv, d) = 
  let fun loop le =
       (case le
         of VAR v => 
              (LE.ltLookup(venv, v, d) 
               handle LE.ltUnbound => 
                (say ("** Lvar ** " ^ (LV.lvarName(v)) ^ " is unbound *** \n");
                 bug "unexpected lambda code in checkLty"))
          | (INT _ | WORD _) => LT.ltc_int
          | (INT32 _ | WORD32 _) => LT.ltc_int32
          | REAL _ => LT.ltc_real
          | STRING _ => ltString
          | PRIM(p, t, ts) => ltTyApp le "PRIM" (t, ts, kenv) 

          | FN(v, t, e1) => 
              let val venv' = LE.ltInsert(venv, v, t, d)
                  val res = check (kenv, venv', d) e1
               in LE.ltFun(t, res) (* handle both functions and functors *)
              end

          | FIX(vs, ts, es, eb) => 
              let fun h (env, v::r, x::z) = h(LE.ltInsert(env, v, x, d), r, z)
                    | h (env, [], []) = env
                    | h _ = bug "unexpected FIX bindings in checkLty."
                  val venv' = h(venv, vs, ts)

                  val nts = map (check (kenv, venv', d)) es
                  val _ = app2(ltMatch le "FIX1", ts, nts)

               in check (kenv, venv', d) eb
              end

          | APP(e1, e2) => ltFnApp le "APP" (loop e1, loop e2)

          | LET(v, e1, e2) => 
              let val venv' = LE.ltInsert(venv, v, loop e1, d)
               in check (kenv, venv', d) e2
              end

          | TFN(ks, e) => 
              let val kenv' = LK.tkInsert(kenv, ks)
                  val lt = check (kenv', venv, DI.next d) e
               in LT.ltc_poly(ks, lt)
              end

          | TAPP(e, ts) => ltTyApp le "TAPP" (loop e, ts, kenv)
          | GENOP(dict, p, t, ts) => 
              ((* should type check dict also *)
               ltTyApp le "GENOP" (t, ts, kenv))

          | PACK(lt, ts, nts, e) => 
              let val argTy = ltTyApp le "PACK-A" (lt, ts, kenv)
               in ltMatch le "PACK-M" (argTy, loop e);
                  ltTyApp le "PACK-R" (lt, nts, kenv)
              end

          | CON((_, rep, lt), ts, e) =>   
              let val t1 = ltTyApp le "CON" (lt, ts, kenv)
                  val t2 = loop e
               in case rep
                   of (DA.CONSTANT _ | DA.EXNCONST _) => 
                        (ltMatch le "CON-M" (t2, LT.ltc_unit); t1)
                    | _ => ltFnApp le "CON-A" (t1, t2)
              end

          | DECON((_, rep, lt), ts, e) =>   
              let val t1 = ltTyApp le "DECON" (lt, ts, kenv)
                  val t2 = loop e
               in case rep
                   of DA.CONSTANT _ => bug "DECON a constant in checkLty"
                    | _ => ltFnAppR le "DECON" (t1, t2)
              end

          | RECORD [] => LT.ltc_unit
          | RECORD el => LE.ltTup (map loop el)
          | SRECORD [] => LT.ltc_unit
          | SRECORD el => LT.ltc_str (map loop el)

          | VECTOR (el, t)  => 
              let val ts = map loop el
               in app (fn x => ltMatch le "VECTOR" (x, LT.ltc_tyc t)) ts; 
                  ltVector t
              end

          | SELECT(i,e) => ltSelect le "SEL" (loop e, i)

          | SWITCH(e, _, cl, opp) => 
              let val root = loop e
                  fun h (c, x) = (ltConChk le "SWT1" (c, root); loop x)
                  val ts = map h cl

               in (case ts
                    of [] => bug "empty switch in checkLty"
                     | a::r => 
                        (app (fn x => ltMatch le "SWT2" (x, a)) r;
                         case opp
                          of NONE => a
                           | SOME be => (ltMatch le "SWT3" (loop be, a); a)))
              end

          | EXNC e => 
              let val z = loop e   (* what do we check on e ? *)
               in ltExn
              end

          | EXNF(e, t) => 
              let val z = loop e   (* what do we check on e ? *)
                  val argt = ltFnAppR le "EXNF1" (t, ltExn)
               in ltIexn
              end

          | RAISE(e,t) => 
              (ltMatch le "RAISE" (loop e, ltExn); t)

          | HANDLE(e1,e2) => 
             let val t1 = loop e1
                 val arg = ltFnAppR le "HANDLE" (loop e2, t1)
              in t1
             end

          (** these two cases should never happen before wrapping *)
          | WRAP(t, b, e) => 
              (ltMatch le "WRAP" (loop e, LT.ltc_tyc t); 
               if laterPhase(phase) then LT.ltc_void
               else LT.ltc_tyc(if b then LT.tcc_box t else LT.tcc_abs t))

          | UNWRAP(t, b, e) => 
              let val ntc = if laterPhase(phase) then LT.tcc_void
                            else (if b then LT.tcc_box t else LT.tcc_abs t)
                  val nt = LT.ltc_tyc ntc
               in (ltMatch le "UNWRAP" (loop e, nt); LT.ltc_tyc t)
              end)
                            

  in loop 
 end (* end-of-fn-check *)

in 
anyerror := false;
check (LK.initTkEnv, LE.initLtyEnv, DI.top) lexp; !anyerror
end (* end of function checkLty *)

end (* toplevel local *)
end (* structure CheckLty *)

(*
 * $Log: chkplexp.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/05/06 01:15:34  george
 *   Version 109.27+
 *
 * Revision 1.2  1997/04/18  15:49:02  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:46  george
 *   Version 109.24
 *
 *)
