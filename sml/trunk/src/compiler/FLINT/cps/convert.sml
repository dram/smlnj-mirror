(* COPYRIGHT 1998 BY YALE FLINT PROJECT *)
(* convert.sml *)

(***************************************************************************
 *                         IMPORTANT NOTES                                 *
 *                                                                         *
 *          The CPS code generated by this phase should not                *
 *                use OFFSET and RECORD accesspath SELp.                   *
 *                  generated by this module.                              *
 ***************************************************************************)
signature CONVERT = sig 
  val convert : FLINT.prog -> CPS.function 
end (* signature CONVERT *)

functor Convert(MachSpec : MACH_SPEC) : CONVERT = struct

local structure DA = Access
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure AP = PrimOp
      structure DI = DebIndex
      structure F  = FLINT
      structure FU = FlintUtil

      open CPS 
in

fun bug s = ErrorMsg.impossible ("ConvertN: " ^ s)
val say = Control.Print.say
val mkv = fn _ => LV.mkLvar()
fun mkfn f = let val v = mkv() in f v end
val ident = fn le => le
val OFFp0 = OFFp 0

(* testing if two values are equivalent lvar values *)
fun veq (VAR x, VAR y) = (x = y)
  | veq _ = false

(***************************************************************************
 *              CONSTANTS AND UTILITY FUNCTIONS                            *
 ***************************************************************************)

fun unwrapf64(u,x,ce) = PURE(P.funwrap,[u],x,FLTt,ce)
fun unwrapi32(u,x,ce) = PURE(P.i32unwrap,[u],x,INT32t,ce)
fun wrapf64(u,x,ce)   = PURE(P.fwrap,[u],x,BOGt,ce)
fun wrapi32(u,x,ce)   = PURE(P.i32wrap,[u],x,BOGt,ce)

fun all_float (FLTt::r) = all_float r
  | all_float (_::r) = false
  | all_float [] = true

fun selectFL(i,u,x,ct,ce) = SELECT(i,u,x,ct,ce)

fun selectNM(i,u,x,ct,ce) =
  (case ct
    of FLTt => mkfn(fn v => SELECT(i,u,v,BOGt,unwrapf64(VAR v,x,ce)))
     | INT32t => mkfn(fn v => SELECT(i,u,v,BOGt,unwrapi32(VAR v,x,ce)))
     | _ => SELECT(i,u,x,ct,ce))

fun recordFL(ul,_,w,ce) = 
  RECORD(RK_FBLOCK, map (fn u => (u,OFFp 0)) ul, w, ce)

fun recordNM(ul,ts,w,ce) =
  let fun g(FLTt::r,u::z,l,h) = 
            mkfn(fn v => g(r, z, (VAR v,OFFp 0)::l, 
                          fn ce => h(wrapf64(u,v,ce))))
        | g(INT32t::r,u::z,l,h) = 
            mkfn(fn v => g(r, z, (VAR v,OFFp 0)::l, 
                              fn ce => h(wrapi32(u,v,ce))))
        | g(_::r,u::z,l,h) = g(r, z, (u,OFFp0)::l, h)
        | g([],[],l,h) = (rev l, h)
        | g _ = bug "unexpected in recordNM in convert"

      val (nul,header) = g(ts,ul,[],fn x => x)
   in header(RECORD(RK_RECORD,nul,w,ce))
  end

(***************************************************************************
 *              UTILITY FUNCTIONS FOR PROCESSING THE PRIMOPS               *
 ***************************************************************************)

(* numkind: AP.numkind -> P.numkind *)
fun numkind (AP.INT bits) = P.INT bits
  | numkind (AP.UINT bits) = P.UINT bits
  | numkind (AP.FLOAT bits) = P.FLOAT bits

(* cmpop: AP.stuff -> P.branch *)
fun cmpop stuff = 
  (case stuff 
    of {oper=AP.EQL,kind=AP.INT 31} => P.ieql
     | {oper=AP.NEQ,kind=AP.INT 31} => P.ineq
     | {oper,kind=AP.FLOAT size} => 
         let fun c AP.>    = P.fGT
	       | c AP.>=   = P.fGE
	       | c AP.<    = P.fLT
	       | c AP.<=   = P.fLE
 	       | c AP.EQL  = P.fEQ
 	       | c AP.NEQ  = P.fLG
 	       | c _ = bug "cmpop:kind=AP.FLOAT"
          in P.fcmp{oper= c oper, size=size}
         end
     | {oper, kind} => 
         let fun check (_, AP.UINT _) = ()
 	       | check (oper, _) = bug ("check" ^ oper)
 	     fun c AP.>   = P.>  
 	       | c AP.>=  = P.>= 
 	       | c AP.<   = P.< 
 	       | c AP.<=  = P.<=
 	       | c AP.LEU = (check ("leu", kind); P.<= )
 	       | c AP.LTU = (check ("ltu", kind); P.< )
 	       | c AP.GEU = (check ("geu", kind); P.>= )
 	       | c AP.GTU = (check ("gtu", kind); P.> )
 	       | c AP.EQL = P.eql
 	       | c AP.NEQ = P.neq
  	  in P.cmp{oper=c oper, kind=numkind kind} 
         end)

(* map_branch:  AP.primop -> P.branch *)
fun map_branch p = 
  (case p 
    of AP.BOXED => P.boxed
     | AP.UNBOXED => P.unboxed
     | AP.CMP stuff => cmpop stuff
     | AP.PTREQL => P.peql
     | AP.PTRNEQ => P.pneq
     | _ => bug "unexpected primops in map_branch")

(* primwrap: cty -> P.pure *)
fun primwrap(INTt) = P.iwrap
  | primwrap(INT32t) = P.i32wrap
  | primwrap(FLTt) = P.fwrap
  | primwrap _ = P.wrap

(* primunwrap: cty -> P.pure *)
fun primunwrap(INTt) = P.iunwrap
  | primunwrap(INT32t) = P.i32unwrap
  | primunwrap(FLTt) = P.funwrap
  | primunwrap _ = P.unwrap

(* arithop: AP.arithop -> P.arithop *)
fun arithop AP.~ = P.~
  | arithop AP.ABS = P.abs
  | arithop AP.NOTB = P.notb
  | arithop AP.+ = P.+
  | arithop AP.- = P.-
  | arithop AP.* = P.*
  | arithop AP./ = P./
  | arithop AP.LSHIFT = P.lshift
  | arithop AP.RSHIFT = P.rshift
  | arithop AP.RSHIFTL = P.rshiftl
  | arithop AP.ANDB = P.andb
  | arithop AP.ORB = P.orb
  | arithop AP.XORB = P.xorb

(* a temporary classifier of various kinds of CPS primops *)
datatype pkind 
  = PKS of P.setter  
  | PKP of P.pure
  | PKL of P.looker
  | PKA of P.arith

(* map_primop: AP.primop -> pkind *)
fun map_primop p = 
  (case p 
    of AP.TEST(from,to) =>   PKA (P.test(from, to))
     | AP.TESTU(from,to) =>  PKA (P.testu(from, to))
     | AP.COPY(from,to) =>   PKP (P.copy(from,to))
     | AP.EXTEND(from,to) => PKP (P.extend(from, to))
     | AP.TRUNC(from,to) =>  PKP (P.trunc(from, to))

     | AP.ARITH{oper,kind,overflow=true} =>
         PKA(P.arith{oper=arithop oper,kind=numkind kind})
     | AP.ARITH{oper,kind,overflow=false} =>
         PKP(P.pure_arith{oper=arithop oper,kind=numkind kind})
     | AP.ROUND{floor,fromkind,tokind} =>
         PKA(P.round{floor=floor, fromkind=numkind fromkind,
                     tokind=numkind tokind})
     | AP.REAL{fromkind,tokind} =>
         PKP(P.real{tokind=numkind tokind, fromkind=numkind fromkind})

     | AP.SUBSCRIPTV => PKP (P.subscriptv)
     | AP.MAKEREF =>    PKP (P.makeref)
     | AP.LENGTH =>     PKP (P.length)
     | AP.OBJLENGTH =>  PKP (P.objlength)
     | AP.GETTAG =>     PKP (P.gettag)
     | AP.MKSPECIAL =>  PKP (P.mkspecial)
     | AP.THROW =>      PKP (P.cast)
     | AP.CAST =>       PKP (P.cast)
     | AP.MKETAG =>     PKP (P.makeref)
        
     | AP.SUBSCRIPT => PKL (P.subscript)
     | AP.NUMSUBSCRIPT{kind,immutable=false,checked=false} => 
           PKL(P.numsubscript{kind=numkind kind})
     | AP.NUMSUBSCRIPT{kind,immutable=true,checked=false} => 
           PKP(P.pure_numsubscript{kind=numkind kind})
     | AP.DEREF =>     PKL(P.!)
     | AP.GETRUNVEC => PKL(P.getrunvec)
     | AP.GETHDLR =>   PKL(P.gethdlr)
     | AP.GETVAR  =>   PKL(P.getvar)
     | AP.GETPSEUDO => PKL(P.getpseudo)
     | AP.GETSPECIAL =>PKL(P.getspecial)
     | AP.DEFLVAR  =>  PKL(P.deflvar)
        
     | AP.SETHDLR => PKS(P.sethdlr)
     | AP.NUMUPDATE{kind,checked=false} =>
           PKS(P.numupdate{kind=numkind kind})
     | AP.UNBOXEDUPDATE => PKS(P.unboxedupdate)
     | AP.BOXEDUPDATE => PKS(P.boxedupdate)
     | AP.UPDATE => PKS(P.update)
     | AP.SETVAR => PKS(P.setvar)
     | AP.SETPSEUDO => PKS(P.setpseudo)
     | AP.SETMARK => PKS(P.setmark)
     | AP.DISPOSE => PKS(P.free)
     | AP.SETSPECIAL => PKS(P.setspecial)
     | AP.USELVAR => PKS(P.uselvar)
     
     | _ => bug ("bad primop in map_primop: " ^ (AP.prPrimop p) ^ "\n"))

(***************************************************************************
 *                  SWITCH OPTIMIZATIONS AND COMPILATIONS                  *
 ***************************************************************************)
(* 
 * BUG: The defintion of E_word is clearly incorrect since it can raise
 *        an overflow at code generation time. A clean solution would be 
 *        to add a WORD constructor into the CPS language -- daunting! The
 *        revolting hack solution would be to put the right int constant 
 *        that gets converted to the right set of bits for the word constant.
 *)
fun do_switch_gen ren = Switch.switch {
   E_int    = fn i => if i < ~0x20000000 orelse i >= 0x20000000
                      then raise Switch.TooBig else INT i, 
   E_word   = fn w => (* if w >= 0wx20000000 
                      then raise Switch.TooBig else *) INT (Word.toIntX w),
   E_real   = (fn s => REAL s),
   E_switchlimit = 4,
   E_neq    = P.ineq,
   E_w32neq = P.cmp{oper=P.neq,kind=P.UINT 32},
   E_i32neq = P.cmp{oper=P.neq,kind=P.INT 32},
   E_word32 = INT32,
   E_int32  = INT32, 
   E_wneq   = P.cmp{oper=P.neq, kind=P.UINT 31},
   E_pneq   = P.pneq,
   E_fneq   = P.fneq,
   E_less   = P.ilt,
   E_branch = (fn (cmp,x,y,a,b) => BRANCH(cmp,[x,y],mkv(),a,b)),
   E_strneq = (fn (w,str,a,b) => BRANCH(P.strneq, [INT(size str), w, 
                                                   STRING str], mkv(), a, b)),
   E_switch = (fn (v,l) => SWITCH(v, mkv(), l)),
   E_add    = (fn (x,y,c) => 
                    mkfn(fn v => ARITH(P.iadd,[x,y],v,INTt,c(VAR v)))),
   E_gettag = (fn (x,c) => mkfn(fn v => PURE(P.getcon,[x],v,INTt,c(VAR v)))), 
   E_unwrap = (fn (x,c) => mkfn(fn v => PURE(P.unwrap,[x],v,INTt,c(VAR v)))),
   E_getexn = (fn (x,c) => mkfn(fn v => PURE(P.getexn,[x],v,BOGt,c(VAR v)))), 
   E_length = (fn (x,c) => mkfn(fn v => PURE(P.length,[x],v,INTt,c(VAR v)))), 
   E_boxed  = (fn (x,a,b) => BRANCH(P.boxed,[x],mkv(),a,b)),
   E_path   = (fn (DA.LVAR v, k) => k(ren v)
                | _ =>  bug "unexpected path in convpath")}

(***************************************************************************
 *       UTILITY FUNCTIONS FOR DEALING WITH META-LEVEL CONTINUATIONS       *
 ***************************************************************************)
(* an abstract representation of the meta-level continuation *)
datatype mcont = MCONT of {cnt: value list -> cexp, ts: cty list}

(* appmc : mcont * value list -> cexp *)
fun appmc (MCONT{cnt, ...}, vs) = cnt(vs)

(* makmc : (value list -> cexp) * cty list -> cexp *)
fun makmc (cnt, ts) = MCONT{cnt=cnt, ts=ts}

(* rttys : mcont -> cty list *)
fun rttys (MCONT{ts, ...}) = ts

(* isEta : cexp * value list -> value option *)
fun isEta (APP(w, vl), ul) = 
      let fun h (x::xs, y::ys) = 
                  if (veq(x, y)) andalso (not (veq(w, y)))
                  then h(xs, ys) else NONE
            | h ([], []) = SOME w
            | h _ = NONE
       in h(ul, vl)
      end
  | isEta _ = NONE

(* preventEta : mcont -> (cexp -> cexp) * value *)
fun preventEta (MCONT{cnt=c, ts=ts}) = 
  let val vl = map mkv ts
      val ul = map VAR vl
      val b = c ul
   in case isEta(b, ul) 
       of SOME w => (ident, w)
        | NONE => let val f = mkv()
                   in (fn x => FIX([(CONT,f,vl,ts,b)],x), VAR f)
                  end
  end (* function preventEta *)

(***************************************************************************
 *                        THE MAIN FUNCTION                                *
 *                   convert : F.prog -> CPS.function                      *   
 ***************************************************************************)
fun convert fdec = 
 let val {getLty=getLtyGen, cleanUp} = Recover.recover (fdec, true)
     val getlty = getLtyGen DI.top
     val ctypes = map ctype
     fun res_ctys f = 
       let val lt = getlty (F.VAR f)
        in if LT.ltp_fct lt then ctypes (#2(LT.ltd_fct lt))
           else if LT.ltp_arrow lt then ctypes (#3(LT.ltd_arrow lt))
                else [BOGt]
       end
     fun get_cty v = ctype (getlty v)
     fun is_float_record u = 
       LT.ltw_tyc (getlty u, 
                   fn tc => LT.tcw_tuple (tc, fn l => all_float (map ctyc l),
                                          fn _ => false),
                   fn _ => false)

     val bogus_cont = mkv() 
     fun bogus_header ce = 
       let val bogus_knownf = mkv()
        in FIX([(KNOWN, bogus_knownf, [mkv()], [BOGt],
               APP(VAR bogus_knownf, [STRING "bogus"]))], 
               FIX([(CONT, bogus_cont, [mkv()], [BOGt],
                    APP(VAR bogus_knownf, [STRING "bogus"]))], ce))
       end 

     local exception Rename
           val m : value Intmap.intmap = Intmap.new(32, Rename)
     in 
     (* F.lvar -> CPS.value *)
     fun rename v = Intmap.map m v handle Rename => VAR v

     (* F.lvar * CPS.value -> unit *) 
     fun newname (v, w) = 
       (case w of VAR w' => LV.sameName (v, w') | _ => ();
        Intmap.add m (v, w))

     (* F.lvar list * CPS.value list -> unit *) 
     fun newnames (v::vs, w::ws) = (newname(v,w); newnames(vs, ws))
       | newnames ([], []) = ()
       | newnames _ = bug "unexpected case in newnames"
     end (* local of Rename *)

     (* switch optimization *)
     val do_switch = do_switch_gen rename

     (* lpvar : F.value -> value *)
     fun lpvar (F.VAR v) = rename v
       | lpvar (F.INT32 i) = 
           let val int32ToWord32 = Word32.fromLargeInt o Int32.toLarge
            in INT32 (int32ToWord32 i)
           end
       | lpvar (F.WORD32 w) = INT32 w
       | lpvar (F.INT i) = INT i
       | lpvar (F.WORD w) = INT(Word.toIntX w)
       | lpvar (F.REAL r) = REAL r
       | lpvar (F.STRING s) = STRING s
          
     
     (* lpvars : F.value list -> value list *)
     fun lpvars vl = 
       let fun h([], z) = rev z
             | h(a::r, z) = h(r, (lpvar a)::z)
        in h(vl, [])
       end
     
     (* lpfd : F.fundec -> function *)
     fun lpfd ((fk, f, vts, e) : F.fundec) = 
       let val k = mkv()
           val vl = k::(map #1 vts)
           val cl = CNTt::(map (ctype o #2) vts)
           val kont = makmc (fn vs => APP(VAR k, vs), res_ctys f)
        in (ESCAPE, f, vl, cl, loop(e, kont))
       end
     
     (* loop : F.lexp * (value list -> cexp) -> cexp *)
     and loop (le, c) = 
       (case le 
         of F.RET vs => appmc(c, lpvars vs)
          | F.LET(vs, e1, e2) =>
              let val kont = 
                    makmc (fn ws => (newnames(vs, ws); loop(e2, c)),
                           map (get_cty o F.VAR) vs)
               in loop(e1, kont)
              end
     	  
          | F.FIX(fds, e) => FIX(map lpfd fds, loop(e, c))   
          | F.APP(f, vs) => 
              let val (hdr, F) = preventEta c
                  val vf = lpvar f
                  val ul = lpvars vs
               in hdr(APP(vf, F::ul))
              end
     	  
          | (F.TFN _ | F.TAPP _) => 
              bug "unexpected TFN and TAPP in convert"
     	  
          | F.RECORD(F.RK_VECTOR _, [], v, e) => 
              bug "zero length vectors in convert"
          | F.RECORD(rk, [], v, e) => 
              let val _ = newname(v, INT 0)
               in loop(e, c)
              end
          | F.RECORD(rk, vl, v, e) => 
              let val ts = map get_cty vl
                  val nvl = lpvars vl
                  val ce = loop(e, c)
               in case rk 
                   of F.RK_TUPLE _ => 
                       if (all_float ts) then recordFL(nvl, ts, v, ce)
                       else recordNM(nvl, ts, v, ce)
                    | F.RK_VECTOR _ => 
                       RECORD(RK_VECTOR, map (fn x => (x, OFFp0)) nvl, v, ce)
                    | _ => recordNM(nvl, ts, v, ce)
              end
          | F.SELECT(u, i, v, e) => 
              let val ct = get_cty (F.VAR v)
                  val nu = lpvar u
                  val ce = loop(e, c)
               in if is_float_record u then selectFL(i, nu, v, ct, ce)
                  else selectNM(i, nu, v, ct, ce)
              end
     	  
          | F.SWITCH(e,l,[a as (F.DATAcon((_,DA.CONSTANT 0,_),_,_),_),
                          b as (F.DATAcon((_,DA.CONSTANT 1,_),_,_),_)], 
                     NONE) =>
              loop(F.SWITCH(e,l,[b,a],NONE),c)
          | F.SWITCH (u, sign, l, d) => 
              let val (header,F) = preventEta c
                  val kont = makmc(fn vl => APP(F, vl), rttys c)
                  val body = 
                    let val df = mkv()
                        fun proc (cn as (F.DATAcon(dc, _, v)), e) = 
                              (cn, loop (F.LET([v], F.RET [u], e), kont))
                          | proc (cn, e) = (cn, loop(e, kont))
                        val b = do_switch{sign=sign, exp=lpvar u, 
                                          cases=map proc l,
                                          default=APP(VAR df, [INT 0])}
                     in case d 
                         of NONE => b
                          | SOME de => FIX([(CONT, df, [mkv()], [INTt], 
                                           loop(de, kont))], b)
                    end
               in header(body)
              end 
          | F.CON(dc, ts, u, v, e) => 
              bug "unexpected case CON in cps convert" 
     	  
          | F.RAISE(u, lts) =>
              let (* execute the continuation for side effects *)
                  val _ = appmc(c, (map (fn _ => VAR(mkv())) lts))
                  val h = mkv()
               in LOOKER(P.gethdlr, [], h, FUNt,
                         APP(VAR h,[VAR bogus_cont,lpvar u]))
              end
          | F.HANDLE(e,u) => (* recover type from u *)
              let val (hdr, F) = preventEta c
                  val h = mkv()
                  val kont = 
                    makmc (fn vl => 
                             SETTER(P.sethdlr, [VAR h], APP(F, vl)),
                           rttys c)
                  val body =                         
                    let val k = mkv() and v = mkv()
                     in FIX([(ESCAPE, k, [mkv(), v], [CNTt, BOGt],
                              SETTER(P.sethdlr, [VAR h], 
                                     APP(lpvar u, [F, VAR v])))],
                            SETTER(P.sethdlr, [VAR k], loop(e, kont)))
                    end
               in LOOKER(P.gethdlr, [], h, FUNt, hdr(body))
              end
     	  
          | F.PRIMOP((_,p as (AP.CALLCC | AP.CAPTURE),_,_), [f], v, e) =>
              let val (kont_decs, F) = 
                    let val k = mkv()
                        val ct = get_cty f
                     in ([(CONT, k, [v], [ct], loop(e, c))], VAR k)
                    end
     	  
                  val (hdr1,hdr2) = 
                    (case p 
                      of AP.CALLCC =>
                          mkfn(fn h => 
                           (fn e => SETTER(P.sethdlr, [VAR h], e),
                            fn e => LOOKER(P.gethdlr, [], h, BOGt, e)))
                       | _ => (ident, ident))
     	  
                  val (ccont_decs, ccont_var) = 
                    let val k = mkv() (* captured continuation *)
                        val x = mkv() 
                     in ([(ESCAPE, k, [mkv(), x], [CNTt, BOGt], 
                           hdr1(APP(F, [VAR x])))], k)
                    end
               in FIX(kont_decs, 
                    hdr2(FIX(ccont_decs, 
                             APP(lpvar f, [F, VAR ccont_var]))))
              end
     	  
          | F.PRIMOP((_,AP.ISOLATE,lt,ts), [f], v, e) => 
              let val (exndecs, exnvar) = 
                    let val h = mkv() and z = mkv() and x = mkv()
                     in ([(ESCAPE, h, [z, x], [CNTt, BOGt],
                         APP(VAR bogus_cont, [VAR x]))], h)
                    end
                  val newfdecs = 
                    let val nf = v and z = mkv() and x = mkv()
                     in [(ESCAPE, v, [z, x], [CNTt, BOGt],
                           SETTER(P.sethdlr, [VAR exnvar],
                             APP(lpvar f, [VAR bogus_cont, VAR x])))] 
                    end
               in FIX(exndecs, FIX(newfdecs, loop(e, c)))
              end

          | F.PRIMOP(po as (_,AP.WCAST,_,_), [u], v, e) =>
              (newname(v, lpvar u); loop(e, c))
     	  
          | F.PRIMOP(po as (_,AP.WRAP,_,_), [u], v, e) => 
              let val ct = ctyc(FU.getWrapTyc po)
               in PURE(primwrap ct, [lpvar u], v, BOGt, loop(e, c))
              end
          | F.PRIMOP(po as (_,AP.UNWRAP,_,_), [u], v, e) =>
              let val ct = ctyc(FU.getUnWrapTyc po)
               in PURE(primunwrap ct, [lpvar u], v, ct, loop(e, c))
              end
     	  
          | F.PRIMOP(po as (_,AP.MARKEXN,_,_), [x,m], v, e) =>
              let val bty = LT.ltc_void
                  val ety = LT.ltc_tuple[bty,bty,bty]
                  val (xx,x0,x1,x2) = (mkv(),mkv(),mkv(),mkv())
                  val (y,z,z') = (mkv(),mkv(),mkv())
               in PURE(P.unwrap,[lpvar x],xx,ctype(ety),
                    SELECT(0,VAR xx,x0,BOGt,
                      SELECT(1,VAR xx,x1,BOGt,
                        SELECT(2,VAR xx,x2,BOGt,
                          RECORD(RK_RECORD,[(lpvar m, OFFp0),
                                            (VAR x2, OFFp0)], z,
                                 PURE(P.wrap,[VAR z],z',BOGt,
                                   RECORD(RK_RECORD,[(VAR x0,OFFp0),
                                                     (VAR x1,OFFp0),
                                                     (VAR z', OFFp0)],
                                          y,
                                      PURE(P.wrap,[VAR y],v,BOGt,
                                           loop(e,c)))))))))
              end
     	  
          | F.PRIMOP(po as (_,p,lt,ts), ul, v, e) => 
              let val ct = 
                    case (#3(LT.ltd_arrow(LT.lt_pinst (lt, ts))))
                     of [x] => ctype x
                      | _ => bug "unexpected case in F.PRIMOP"
                  val vl = lpvars ul
               in case map_primop p
                   of PKS i => let val _ = newname(v, INT 0)
                                in SETTER(i, vl, loop(e,c))
                               end
                    | PKA i => ARITH(i, vl, v, ct, loop(e,c))
                    | PKL i => LOOKER(i, vl, v, ct, loop(e,c))
                    | PKP i => PURE(i, vl, v, ct, loop(e,c))
              end
     	  
          | F.BRANCH(po as (_,p,_,_), ul, e1, e2) => 
              let val (hdr, F) = preventEta c
                  val kont = makmc(fn vl => APP(F, vl), rttys c)
               in hdr(BRANCH(map_branch p, lpvars ul, mkv(),
                             loop(e1, kont), loop(e2, kont)))
              end)
 
    (* processing the top-level fundec *)
    val (fk, f, vts, be) = fdec
    val k = mkv()    (* top-level return continuation *)
    val kont = makmc (fn vs => APP(VAR k, vs), res_ctys f)
    val body = loop(be, kont)

    val vl = k::(map #1 vts)
    val cl = CNTt::(map (ctype o #2) vts)
 in (ESCAPE, f, vl, cl, bogus_header body) before cleanUp()
end (* function convert *)

end (* toplevel local *)
end (* functor Convert *)

