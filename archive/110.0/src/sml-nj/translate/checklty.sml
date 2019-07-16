(* Copyright 1996 by AT&T Bell Laboratories *)
(* checklty.sml *)

signature CHECKLTY = 
sig 

val checkLty : Lambda.lexp * int -> bool
val newlam_ref : Lambda.lexp ref
val fname_ref : string ref

end (* signature CHECKLTY *)

structure CheckLty : CHECKLTY = 
struct

local structure LK = LtyKernel 
      structure LT = LambdaType
      structure LE = LtyEnv
      structure LV = LambdaVar
      structure DA = Access 
      structure DI = DebIndex
      open Lambda 
in

(*** a hack of printing diagnostic output into a separate file ***) 
val newlam_ref : Lambda.lexp ref = ref (RECORD[])
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

fun simplify(le,0) = SVAL(STRING "<dummy>")
  | simplify(le,n) = 
      let fun h le = simplify(le, n-1)
       in case le 
           of FN(v, t, e) => FN(v, t, h e)
            | FIX(lv, lt, le, b) => FIX(lv, lt, map h le, h b)
            | LET(v, e1, e2) => LET(v, h e1, h e2)
            | TFN(ks, e) => TFN(ks, h e)
            | SWITCH(v, l, dc, opp) => 
               (let fun g(c, x) = (c, h x)
                    fun f x = case x of SOME y => SOME(h y) | NONE => NONE
                 in SWITCH(v, l, map g dc, f opp)
                end)
            | HANDLE(e, v) => HANDLE(h e, v)
            | _ => le
      end (* end of simplify *)

(** utility functions for printing *)
val tkPrint = say o LT.tk_print
val tcPrint = say o LE.tcPrint
val ltPrint = say o LE.ltPrint
fun lePrint le = MCprint.printLexp (simplify(le, 3))
fun svPrint sv = MCprint.printSval (sv)

(*** a hack for type checking ***)
fun laterPhase i = (i > 20)

(****************************************************************************
 *           MAIN FUNCTION --- val checkLty : Lambda.lexp -> bool           *
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
         say "\n \n";  MCprint.printLexp le;
         say "***************************************************** \n"))
  handle zz => 
  (clickerror();
   say (s ^ "  **** Lty conflicting in lexp =====> \n    ");
   say "uncaught exception found ";
   say "\n \n";  MCprint.printLexp le; say "\n";  
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
  let fun lpsv sv = 
       (case sv
         of VAR v => 
              (LE.ltLookup(venv, v, d) 
               handle LE.ltUnbound => 
                (say ("** Lvar ** " ^ (LV.lvarName(v)) ^ " is unbound *** \n");
                 bug "unexpected lambda code in checkLty"))
          | (INT _ | WORD _) => LT.ltc_int
          | (INT32 _ | WORD32 _) => LT.ltc_int32
          | REAL _ => LT.ltc_real
          | STRING _ => ltString
          | PRIM(p, t, ts) => ltTyApp (SVAL sv) "PRIM" (t, ts, kenv) 
          | GENOP(dict, p, t, ts) => 
              ((* should really do more here, check the consistency of dict *)
               ltTyApp (SVAL sv) "GENOP" (t, ts, kenv)))

      fun loop le =
       (case le
         of SVAL sv => lpsv sv
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

          | APP(v1, v2) => ltFnApp le "APP" (lpsv v1, lpsv v2)

          | LET(v, e1, e2) => 
              let val venv' = LE.ltInsert(venv, v, loop e1, d)
               in check (kenv, venv', d) e2
              end

          | TFN(ks, e) => 
              let val kenv' = LK.tkInsert(kenv, ks)
                  val lt = check (kenv', venv, DI.next d) e
               in LT.ltc_poly(ks, lt)
              end

          | TAPP(sv, ts) => ltTyApp le "TAPP" (lpsv sv, ts, kenv)

          | PACK(lt, ts, nts, sv) => 
              let val argTy = ltTyApp le "PACK-A" (lt, ts, kenv)
               in ltMatch le "PACK-M" (argTy, lpsv sv);
                  ltTyApp le "PACK-R" (lt, nts, kenv)
              end

          | CON((_, rep, lt), ts, sv) =>   
              let val t1 = ltTyApp le "CON" (lt, ts, kenv)
                  val t2 = lpsv sv
               in case rep
                   of (DA.CONSTANT _ | DA.EXNCONST _) => 
                        (ltMatch le "CON-M" (t2, LT.ltc_unit); t1)
                    | _ => ltFnApp le "CON-A" (t1, t2)
              end

          | DECON((_, rep, lt), ts, sv) =>   
              let val t1 = ltTyApp le "DECON" (lt, ts, kenv)
                  val t2 = lpsv sv
               in case rep
                   of DA.CONSTANT _ => bug "DECON a constant in checkLty"
                    | _ => ltFnAppR le "DECON" (t1, t2)
              end

          | RECORD [] => LT.ltc_unit
          | RECORD vl => LE.ltTup (map lpsv vl)
          | SRECORD [] => LT.ltc_unit
          | SRECORD vl => LT.ltc_str (map lpsv vl)

          | VECTOR (vl, t)  => 
              let val ts = map lpsv vl
               in app (fn x => ltMatch le "VECTOR" (x, LT.ltc_tyc t)) ts; 
                  ltVector t
              end

          | SELECT(i, sv) => ltSelect le "SEL" (lpsv sv, i)

          | SWITCH(v, _, cl, opp) => 
              let val root = lpsv v
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

          | EXNC v => 
              let val z = lpsv v   (* what do we check on e ? *)
               in ltExn
              end

          | EXNF(v, t) => 
              let val z = lpsv v   (* what do we check on e ? *)
                  val argt = ltFnAppR le "EXNF1" (t, ltExn)
               in ltIexn
              end

          | RAISE(v,t) => 
              (ltMatch le "RAISE" (lpsv v, ltExn); t)

          | HANDLE(e,sv) => 
             let val t1 = loop e
                 val arg = ltFnAppR le "HANDLE" (lpsv sv, t1)
              in t1
             end

          (** these two cases should never happen before wrapping *)
          | WRAP(t, b, sv) => 
              (ltMatch le "WRAP" (lpsv sv, LT.ltc_tyc t); 
               if laterPhase(phase) then LT.ltc_void
               else LT.ltc_tyc(if b then LT.tcc_box t else LT.tcc_abs t))

          | UNWRAP(t, b, sv) => 
              let val ntc = if laterPhase(phase) then LT.tcc_void
                            else (if b then LT.tcc_box t else LT.tcc_abs t)
                  val nt = LT.ltc_tyc ntc
               in (ltMatch le "UNWRAP" (lpsv sv, nt); LT.ltc_tyc t)
              end)
                            
  in loop 
 end (* end-of-fn-check *)

in 
anyerror := false;
check (LK.initTkEnv, LE.initLtyEnv, DI.top) lexp; !anyerror
end (* end of function checkLty *)

end (* toplevel local *)
end (* structure CheckLty *)

