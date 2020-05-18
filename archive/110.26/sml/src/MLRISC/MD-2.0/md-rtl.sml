(*
 * Process rtl descriptions
 *)
functor MDRTL(structure Typing : MD_TYPING) : MD_RTL =
struct
   structure Comp  = Typing.Comp
   structure Ast   = Comp.Ast
   structure AstPP = Comp.AstPP
   structure Env   = Comp.Env
   structure H     = HashTable
   structure L     = LambdaRTL
   structure TypeUtil = Typing.TypeUtil
   structure T     = Comp.Trans

   open Ast Comp.Util MDError

   datatype rtlmd = RTLMD of 
     { md        : Comp.md,
       env       : Env.env,
       userDecls : Ast.decl, 
       rtls      : L.rtl list 
     }

   val rtls = ref [] : L.rtl list ref

   exception NotFound
   exception UseDefault

   fun md(RTLMD{md, ...}) = md
   fun getRTLs(RTLMD{rtls,...}) = rtls
   fun getUserDecls(RTLMD{userDecls,...}) = userDecls
   fun lookupRTL(RTLMD{rtls,...}) name =
   let fun f [] = raise NotFound
         | f((x as (n,_,_))::rtls) = if n = name then x else f rtls
   in  f rtls
   end

   fun t2s t = PP.text(AstPP.ty t)
   fun e2s e = PP.text(AstPP.exp e)
   fun p2s p = PP.text(AstPP.pat p)
   fun d2s d = PP.text(AstPP.decl d)

   val rw  = T.rewrite 
   val NIL = T.noRewrite

   (*
    * Generate a report from the set of compiled RTLs
    *)
   fun report(RTLMD{md, rtls, env, ...}) = 
   let fun pr(rtl as (f,_,body)) = 
             f^" "^L.defUseToString(L.defUse rtl)^"\n"^
             "\t"^ L.RTL.rtlToString body^"\n"
       val typs = Env.foldVal (fn (id,e,t,l) => (id,t2s t^"\n")::l) [] env
       val typs = String.concat
                     (map (fn (x,t) => x^" : "^t)
                       (ListMergeSort.sort (fn ((a,_),(b,_)) => String.>(a,b)) 
                          typs))
       val text = Int.toString(length rtls)^" rtls\n"^
                  typs^
                  String.concat (map pr rtls)^"\n"
   in  Comp.outfile md "" ".log" text
   end

   (*
    * Rewrite the program to fill in all syntactic shorthands
    *)
   fun expandSyntacticSugar(md, rtlDecls) =
   let (* Function to define a new operator *)
       fun newRtlOp argTy f = 
           let fun newVars(i,n) = 
               if i < n then ("x"^Int.toString i)::newVars(i+1,n)
               else []
               fun arity(TUPLEty x) = length x
                 | arity _ = 1
               val names = newVars(0,arity argTy)
               val formals = TUPLEpat(map IDpat names)
               val actuals = LISTexp(map ID names,NONE)
           in  LOCALdecl([VAL("newOper",APP("newOp",STRINGexp f))],
                         [FUN(f,formals,APP("newOper",actuals))])
           end
 
       (* Rewrite the program first to fill in all syntactic shorthands *)
       fun exp ==> (e as INTexp _)  = APP("intConst", e)
         | exp ==> (e as WORDexp _) = APP("wordConst",e)
         | exp ==> (APPexp(IDexp(IDENT([],":=")), 
                           TUPLEexp[LOCexp(m,x,NONE),y])) =
             APP(":=",TUPLEexp[APP("*lhs*",APPexp(STRINGexp m,x)),y])
         | exp ==> (APPexp(IDexp(IDENT([],":=")), 
                           TUPLEexp[LOCexp(m,x,SOME r),y])) =
             APP(":=",TUPLEexp[APP("*memlhs*",APPexp(STRINGexp m,
                           TUPLEexp[x,STRINGexp r])),y])
         | exp ==> e = e 
       fun decl ==> (RTLSIGdecl(fs,FUNty(argTy,_))) = 
             SEQdecl(map (newRtlOp argTy) fs) 
         | decl ==> (d as RTLSIGdecl(fs,ty)) = (error("bad type in "^d2s d); d)
         | decl ==> d = d

       val rtlDecls = #decl (rw{exp=exp,pat=NIL,decl=decl,ty=NIL}) rtlDecls

       (* The aggregation operator is dependent on the endianess *)
       val aggr = case Comp.endianess md of LITTLE => "aggl" | BIG => "aggb"
       val idaggr = "idaggr" (* The identity aggregation *)

       fun aggrOf "cellset" = idaggr
         | aggrOf m = 
           let val CELLdecl{id,...} = Comp.lookupCellKind md m
           in  if id = "MEM" then aggr else idaggr end

       fun exp ==> (e as LOCexp(m,_,_)) = 
                  APP("fetch",APP(aggrOf m,e))
         | exp ==> (APPexp(IDexp(IDENT([],":=")),
                          TUPLEexp[APPexp(IDexp(IDENT([],"*lhs*")),
                                      APPexp(STRINGexp m,x)),y])) =
                APP(":=",TUPLEexp[APP(aggrOf m,LOCexp(m,x,NONE)),y]) 
         | exp ==> (APPexp(IDexp(IDENT([],":=")),
                          TUPLEexp[APPexp(IDexp(IDENT([],"*memlhs*")),
                            APPexp(STRINGexp m,TUPLEexp[x,STRINGexp r])),y])) =
                APP(":=",TUPLEexp[APP(aggrOf m,LOCexp(m,x,SOME r)),y]) 
         | exp ==> e = e
   in  #decl (rw{exp=exp,pat=NIL,decl=NIL,ty=NIL}) rtlDecls end


   (*
    * Perform type interference and arity raising
    *)
   fun typeInference(md, rtlDecls) = 
   let (* Perform typechecking *)
       val (semantics, env) = 
           (print "Typechecking...";
            Typing.typeCheck md rtlDecls before print "done\n")

       (* Make sure that there are no unresolved type applications after
        * arity raising.
        *)
       fun checkUnresolvedTypeApplications(d,loc) = 
           let val poly = ref false
               fun exp ==> (e as TYPEexp t) =
                   (if Typing.isPolymorphic t then poly := true else (); e)
                 | exp ==> e = e
           in  #decl (rw{exp=exp,ty=NIL,decl=NIL,pat=NIL}) d;
               if !poly then
                 errorPos(loc,"unresolved type applications in:\n"^d2s d)
               else ()
           end

       fun decl ==> d =
            (case d of 
              MARKdecl(l,d as VALdecl _) => 
                 checkUnresolvedTypeApplications(d, l)
            | RTLdecl(_,_,loc) => checkUnresolvedTypeApplications(d, loc)
            | _ => ();
            d 
            )
       val _ = #decl (rw{exp=NIL,ty=NIL,decl=decl,pat=NIL}) semantics;
   in  (semantics, env) end

   (*
    * Translate the rtl declarations into an executable form.
    *)
   fun codeGen(md, env, rtlDecls) = 
   let fun cellOf "cellset" = TUPLEexp[STRINGexp "cellset",INTexp 0]
         | cellOf k = 
           let val CELLdecl{id, bits, ...} = Comp.lookupCellKind md k
           in  TUPLEexp[STRINGexp id,INTexp bits] end

       fun exp ==> (LOCexp(m,e,NONE)) = APPexp(APP("$",cellOf m),e)
         | exp ==> (LOCexp(m,e,SOME r)) = 
              APPexp(APP("$$",cellOf m),TUPLEexp[e,ID r])
         | exp ==> (IFexp(a,b,c))   = APP("If",TUPLEexp[a,b,c])
         | exp ==> (TUPLEexp [])    = ID "Nop"
         | exp ==> (IDexp(IDENT([],"="))) = ID "=="
         | exp ==> (TYPEDexp(e,t)) = e
         | exp ==> (APPexp(BITSLICEexp(e,r),t)) = 
              APPexp(APPexp(APP("bitslice",t),
                 LISTexp(map (fn (a,b) => TUPLEexp[INTexp a,INTexp b]) r,
                         NONE)),e)
         | exp ==> (BOOLexp false) = ID "False"
         | exp ==> (BOOLexp true) = ID "True"
         | exp ==> e = e

       val allRtls = ref []

       fun addRtls(p, loc) = 
       let fun processBinding x =
           let val (_,t) = Env.lookupVal env (IDENT([],x))
               val t = #ty (rw{exp=NIL,pat=NIL,decl=NIL,ty=NIL}) t
           in  if Typing.isPolymorphic t then
                    errorPos(loc, "rtl "^x^" has polymorphic type "^
                             t2s t) 
               else 
               case t of 
                  FUNty(RECORDty lts,_) => (allRtls := (x,lts,loc) :: !allRtls)
                | t => errorPos(loc, "rtl "^x^" has bad type "^t2s t) 
           end
           fun pat ==> (p as IDpat x) = (processBinding x; p)
             | pat ==> p = p
       in #pat (rw{exp=NIL,ty=NIL,decl=NIL,pat=pat}) p end

       fun decl ==> (DATATYPEdecl _) = SEQdecl[]
         | decl ==> (TYPESIGdecl _) = SEQdecl[]
         | decl ==> (VALSIGdecl _) = SEQdecl[]
         | decl ==> (VALdecl[VALbind(LISTpat(pats,NONE),
                       APPexp(
                          APPexp(APPexp(IDexp(IDENT([],"map")),_),f),
                              LISTexp(es,NONE)))]) =
              VALdecl(ListPair.map (fn (p,e) => VALbind(p,APPexp(f,e)))
                         (pats,es))
         | decl ==> (VALdecl[VALbind(LISTpat(pats,NONE),LISTexp(es,NONE))]) =
              VALdecl(ListPair.map VALbind (pats,es))
         | decl ==> (RTLdecl(pat,exp,loc)) = 
              (addRtls(pat,loc); ==>(VALdecl[VALbind(pat,exp)]))
         | decl ==> (MARKdecl(_,SEQdecl [])) = SEQdecl[]
         | decl ==> d = d

   in (#decl (rw{exp=exp,pat=NIL,decl=decl,ty=NIL}) rtlDecls, rev(!allRtls)) end

   (*
    * Process the rtl description 
    *)
   fun compile md =
   let (* The semantics environment *)
       val semantics = Comp.declOf md "RTL"

       (* Expand Syntactic sugar *)
       val semantics = expandSyntacticSugar(md, semantics)

       (* Perform typechecking *)
       val (semantics, env) = typeInference(md, semantics)

       (* Generate the rtl functions defined by the user *)
       val (userRtlDecls, allRtls) = codeGen(md, env, semantics)

       (* Generate the rtl table *)
       val rtlTable = 
       let fun rtlEntry const (name,args,loc) =
           let fun kind(x,APPty(IDENT([],"bits"),[INTVARty i])) = (i,"reg")
                 | kind(x,APPty(IDENT([],"operand"),[INTVARty i])) = (i,"opn")
                 | kind(x,APPty(IDENT([],"immed"),[INTVARty i])) = (i,"imm")
                 | kind(x,APPty(IDENT([],"loclist"),[INTVARty i])) = 
                     (i,"loclist")
                 | kind(x,IDty(IDENT([],"label"))) = (0,"lab")
                 | kind(x,IDty(IDENT([],"region"))) = (0,"region")
                 | kind(x,t) = fail("argument "^x^" of rtl "^name^
                                     " has unknown type "^t2s t)
               fun mkArg(x,t) = 
               let val (ty,kind) = kind(x,t)
               in  (x,const(APP("!",
                       TUPLEexp[INTexp ty,STRINGexp kind,STRINGexp x])))
               end
           in  TUPLEexp
                  [STRINGexp name,
                   const(LISTexp(map (fn (x,_) => STRINGexp x) args,NONE)),
                   APP(name, RECORDexp(map mkArg args))
                  ]
           end
           fun body const = LISTexp(map (rtlEntry const) allRtls,NONE)
       in  Comp.withConsts(fn cst => VALdecl[VALbind(IDpat "rtls", body cst)])
       end
       

       (* The ArchRTL functor *)
       val strname = Comp.strname md "RTL"
       val archRTL = 
             STRUCTUREdecl(strname,[$["BuildRTL : BUILD_RTL"]],
               DECLsexp
                [LOCALdecl([OPENdecl [IDENT([],"BuildRTL")]],[userRtlDecls])])

       (* Write the functor to a file *)
       val _ = Comp.codegen md "mltree/RTL" [AstPP.decl archRTL]

       val code =
         LOCALdecl(
             [archRTL,
              STRUCTUREdecl(strname,[],APPsexp(IDENT([],strname),
                                        IDsexp(IDENT([],"BuildRTL")))),
              LOCALdecl([OPENdecl [IDENT([],"BuildRTL"),IDENT([],strname)]],
                        [rtlTable])
             ],
             [ 
               $["val _ = MDRTL.rtls := rtls"]
             ]
         )

       (* Compile RTL into internal form *)
       fun elaborateRTL(code) = 
       let val _  = rtls := []
           val name = "CompileRTL"
           val _ = Comp.codegen md name [AstPP.decl code]
           val filename = Comp.pathName md name ".sml"
       in  if !errorCount = 0 then Compiler.Interact.useFile filename else ()
       end
   in  elaborateRTL(code); 
       RTLMD{md        = md,
             env       = env,
             userDecls = userRtlDecls,
             rtls      = !rtls
            }
   end

   (*
    * Generate code for RTL
    *)
   fun gen(rtlmd as RTLMD{rtls,...}) = 
   let val _ = report rtlmd
   in  ()
   end

   (*
    * Code generation
    *)

   datatype rtlpat = LITrtl of string
                   | TYrtl  of string * datatypebind

   exception Undefined

   fun tuplepat [p] = p
     | tuplepat ps  = TUPLEpat ps
   fun tupleexp [e] = e
     | tupleexp es  = TUPLEexp es

   (*
    *  Process all datatypes that have RTLs associated with them.
    *)
   fun queryFun rtlmd =
   let (* The machine description *)
       val md = md rtlmd

       (* The instructions *)
       val instructions = Comp.instructions md

       (* The main environment *)
       val env = Comp.env md

       (* Make a record pattern *) 
       fun mkRecordPat xs = RECORDpat(map (fn x => (x,IDpat x)) xs,false)

       (* Generate a call to the bug function *)
       fun bug funName = APP("bug",TUPLEexp[STRINGexp funName,ID "instr"])

       (* The semantics environment *)
       val semEnv = Env.lookupStr env (IDENT([],"RTL"))

       (* The instruction environment *)
       val instrEnv = Env.lookupStr env (IDENT([],"Instruction"))

       (* All datatype definitions in the instruction environment *)
       val datatypeDefinitions = Env.datatypeDefinitions instrEnv

       fun mkClause(cb,exp) = Env.consToClause {prefix="I",pat=fn p=>p,
                                                exp=exp, cons=cb}

       val callUndefined = APP("undefined",TUPLEexp [])
       val undefinedClause = [CLAUSE([WILDpat],callUndefined)]

       (* Make a function *)
       fun mkClauses(funName,cbs,body) =
       let fun processClauses([], false) = []
             | processClauses([], true) = undefinedClause
             | processClauses(cb::cbs, error) =
                mkClause(cb,body cb)::processClauses(cbs,error)
                handle _ => processClauses(cbs, true)
       in  processClauses(cbs, false) end

       fun mkFunction(funName, errorName, cons, body) =
           FUNdecl[FUNbind(funName, mkClauses(errorName, cons, body))] 

       (* Make a query function *)
       fun queryFun const 
           {name,extraArgs,args,extraExps,localDecls,body,composite} =
       let (* Set up the parameters *)
           val args = case args of
                        [x] => IDpat x
                      | _   => mkRecordPat args

           val args = case extraArgs of
                        [] => [args]
                      | _  => [mkRecordPat extraArgs,args]

           val wilds = map (fn _ => WILDpat) extraExps

           (* Generate code for each instruction *)
           fun foreachInstr(CONSbind{rtl=NONE, ...}) = raise Undefined
             | foreachInstr(instr as CONSbind{rtl=SOME rtlExp, id, loc, ...}) = 
               let (* parameters of instr *)
                   val _      = setLoc loc
                   val params = Env.consBindings instr 
                   fun err x = fail("parameter "^x^
                                    " is undefined in instruction "^id)
                   fun transRTL(Ast.LITrtl s) = LITrtl s
                     | transRTL(Ast.IDrtl x) = 
                       let val (_,ty) = Env.lookupVal' err params (IDENT([],x))
                           val db =
                             case ty of
                               IDty(IDENT([],t)) => Comp.lookupDatatype md t
                             | _ => fail("illegal type "^PP.text(AstPP.ty ty))
                       in  TYrtl(x,db) end
                   fun exp ==> (RTLexp [COMPOSITErtl x]) =
                       let val (_,ty) = Env.lookupVal' err params (IDENT([],x))
                       in  composite{instr=instr,id=x,ty=ty} end
                     | exp ==> (RTLexp rtl) =
                          foreachRtlPat (genCode(instr,params)) 
                             (map transRTL rtl) 
                     | exp ==> e = e
                   val rw = rw{exp=exp,decl=NIL,pat=NIL,ty=NIL}
               in  #exp rw rtlExp end

           (* Enumerate all rtl patterns *)
           and foreachRtlPat gen rtlpats =
               let fun enum([],pats,name) = [(tuplepat pats,name)]
                     | enum(LITrtl s::rest,pats,name) = enum(rest,pats,s^name)
                     | enum(TYrtl(_,DATATYPEbind{cbs,...})::rest,pats,name) =
                       let val names = 
                           map (fn cb as CONSbind{id,...} => 
                                let val p = Env.consToPat{cons=cb,prefix="I"}
                                in  enum(rest,p::pats,id^name)
                                end) cbs 
                       in  List.concat names end
                   fun caseExps [] = []
                     | caseExps(LITrtl _::rest) = caseExps rest
                     | caseExps(TYrtl(x,_)::rest) = ID x::caseExps rest
                   val exps  = caseExps rtlpats
                   val cases = enum(rev rtlpats,[],"") (* all possible cases *)
                   val clauses = map gen cases
                   val extraClause = (*case extraExps of
                                       [] => []
                                     | _  => [CLAUSE([WILDpat],callUndefined)]*)
                                     []
               in  CASEexp(TUPLEexp(tupleexp exps :: map ID extraExps),
                           clauses@extraClause) 
               end

           (* Generate code for one rtl *)
           and genCode (instr,params) (pats,nameOfRtl) =
               let val rtl as (_,args,_) = lookupRTL rtlmd nameOfRtl 
                   fun err x = 
                       error("can't found parameter "^x^" in rtl "^nameOfRtl) 
                   val _ = app (fn x => Env.lookupVal' err params (IDENT([],x)))
                               args
                   val {pat=pats',exp=exp} = 
                       body{const=const,rtl=rtl,instr=instr}
               in  CLAUSE([tuplepat(pats :: pats')],exp)
               end handle NotFound => 
                   (warning("rtl "^nameOfRtl^" is undefined"); 
                    CLAUSE([tuplepat(pats :: wilds)],callUndefined))

           val query = mkFunction("query", name, instructions, foreachInstr)

           (* error handler function *)
           val undefined = 
                 FUNdecl[FUNbind("undefined",[CLAUSE([TUPLEpat []],bug name)])]
           val localDecls = undefined :: localDecls @ [query]
 
           (* main query function *)
        in FUNdecl[FUNbind(name,[CLAUSE(args,
                   LETexp(localDecls,[APP("query",ID "instr")]))])] 
        end
   in  fn x => Comp.withConsts (fn cnst => queryFun cnst x)
   end (* queryFun *)

   (*
    *  Generate a query function according to the def/use info
    *)
   fun queryExp rtlmd {name, reg, fixreg, regs, cellset, opnd, lab, imm, region}
                      (items, dflt) =
   let (* The machine description *)
       val md = md rtlmd

       fun gen(L.OBJ(L.PARAM r,_,L.REG k),e) = reg(ID r,k,e)
         | gen(L.OBJ(L.CONST i,_,L.REG k),e) =
           let val CELLdecl{from, ...} = Comp.lookupCellKind md k
               val r = INTexp(!from + i)
           in  fixreg(r,k,e) end
         | gen(L.OBJ(L.PARAM r,_,L.REGS k),e) = regs(ID r,k,e)
         | gen(L.OBJ(L.PARAM r,_,L.OPND),e) = opnd(ID r,e)
         | gen(L.OBJ(L.PARAM r,_,L.LAB),e)  = lab(ID r,e)
         | gen(L.OBJ(L.PARAM r,_,L.IMM),e)  = imm(ID r,e)
         | gen(L.OBJ(L.PARAM r,_,L.CELLSET),e) = cellset(ID r,e)
         | gen(L.OBJ(L.PARAM r,_,L.REGION),e) = region(ID r,e)
         | gen _ = fail name

   in  foldr gen dflt items
   end

   (*
    *  Generate a query function according to the def/use info
    *)
   fun queryPatExp rtlmd 
          {name, defUse, reg, fixreg, regs, cellset, opnd, lab, imm, region} 
              (items, dflt) =
   let (* The machine description *)
       val md = md rtlmd

       fun newItem(name,i,LISTpat(ps,NONE),e,f) = 
           let val name = name^defUse
           in  (i+1,LISTpat(ps @ [IDpat name], NONE), f(fn () => ID name,e)) 
               handle UseDefault => 
               (i+1,LISTpat(ps @ [WILDpat], NONE), e)
           end
         | newItem(name,~1,pat as LISTpat(ps,p'),e,f) = 
            ((~1,pat,f(fn () => fail "newItem",e))
             handle UseDefault => (~1, pat, e)
            )
         | newItem(name,i,pat as LISTpat(ps,p'),e,f) = 
           ((i+1,pat,f(fn () => APP("List.nth",TUPLEexp[ID defUse,INTexp i]),e))
            handle UseDefault => (i+1, pat, e)
           )
         | newItem _ = fail "newItem: bad list"

       fun rmvWilds [] = []
         | rmvWilds (WILDpat::ps) = 
            (case rmvWilds ps of
               [] => []
             | ps => WILDpat::ps
            )
         | rmvWilds (p::ps) = p::rmvWilds ps

       fun newItems(name,i,LISTpat(ps,NONE),e,f) = 
           let val name = name^defUse   
           in  (~1, LISTpat(ps,SOME(IDpat name)), f(fn () => ID name,e)) 
               handle UseDefault => 
                (~1, LISTpat(rmvWilds ps, SOME WILDpat), e)
           end 
         | newItems(name,~1,pat as LISTpat(ps,p'),e,f) = 
             ((~1,pat,f(fn () => fail "newItems",e))
              handle UseDefault => (~1,pat,e)
             )
         | newItems(name,i,pat as LISTpat(ps,p'),e,f) = 
           ((~1,pat,f(fn () => APP("List.drop",TUPLEexp[ID defUse,INTexp i]),e))
            handle UseDefault => (~1,pat,e)
           )
         | newItems _ = fail "newItem: bad list"

       fun gen(L.OBJ(L.PARAM r,_,L.REG k),(i,p,e)) = 
              newItem(r,i,p,e,fn (x,e) => reg(x,k,e))
         | gen(L.OBJ(L.CONST n,_,L.REG k),(i,p,e)) =
           let val CELLdecl{from, ...} = Comp.lookupCellKind md k
               val r = !from + n
           in  newItem("r"^Int.toString r,i,p,e, 
                       fn (x,e) => fixreg(x,INTexp r,k,e)) 
           end
         | gen(L.OBJ(L.PARAM r,_,L.REGS k),(i,p,e))  = 
              newItems(r,i,p,e,fn (x,e) => regs(x,k,e))
         | gen(L.OBJ(L.PARAM r,_,L.OPND),(i,p,e)) = 
              newItem(r,i,p,e,fn (x,e) => opnd(x,e))
         | gen(L.OBJ(L.PARAM r,_,L.LAB),(i,p,e)) = 
              newItem(r,i,p,e,fn (x,e) => lab(x,e))
         | gen(L.OBJ(L.PARAM r,_,L.IMM),(i,p,e)) = 
              newItem(r,i,p,e,fn (x,e) => imm(x,e))
         | gen(L.OBJ(L.PARAM r,_,L.CELLSET),(i,p,e)) = 
              newItems(r,i,p,e,fn (x,e) => cellset(x,ID r,e))
         | gen(L.OBJ(L.PARAM r,_,L.REGION),(i,p,e))  = 
              newItems(r,i,p,e,fn (x,e) => region(x,e))
         | gen _ = fail name
       val (_,p,e) = foldl gen (0,LISTpat([],NONE),dflt) items
       val p =
        case p of
          LISTpat([],SOME WILDpat) => WILDpat
        | LISTpat(ps,NONE) =>
            if List.all (fn WILDpat => true | _ => false) ps then WILDpat
            else p
        | p => p
   in  (p,e)
   end

   (* 
    * Generate code for operand
    *)
   fun queryOpnd rtlmd {name, extraArgs, reg, opnd, imm, default} =
   let val md = md rtlmd
       val DATATYPEbind{cbs, ...} = Comp.lookupDatatype md "operand"
       val arg = ID "opnd" 
       fun genClause(cb as CONSbind{rtl, ...}) = 
       let val pats = [Env.consToPat{cons=cb,prefix="I"}]
           val (pats,exp)  = 
            (case rtl of
              SOME(LOCexp(cell,e,NONE)) => (pats, reg e)
            | SOME(APPexp(IDexp(IDENT([],"immed")),e)) => (pats, imm e) 
            | SOME _ => ([WILDpat],opnd arg) (* XXX *)
            | _      => ([WILDpat],opnd arg)
            ) handle _ => ([WILDpat],default)
       in  CLAUSE(pats,exp) end
       val args = IDpat "opnd"
       val args = case extraArgs of 
                    [] => [args]
                  | _  => [TUPLEpat(args::map IDpat extraArgs)]
   in  FUNdecl[FUNbind(name,
               [CLAUSE(args,CASEexp(arg, map genClause cbs))])
             ]
   end

  (* Generate function for generating operands *)
   fun queryGenOpnd rtlmd {name} =
   let val md = md rtlmd
       val DATATYPEbind{cbs, ...} = Comp.lookupDatatype md "operand"

       fun reg ((cb as CONSbind{id, rtl, ...})::cbs) = 
           (case rtl of
              SOME(LOCexp(cell,e,NONE)) => "I."^id
           | _ => reg cbs 
           )
         | reg [] = "error \""^name^"\""

       fun immed((cb as CONSbind{id, rtl, ...})::cbs) = 
           (case rtl of
              SOME(APPexp(IDexp(IDENT([],"immed")),e)) => "I."^id
            | _ => immed cbs
           )
         | immed [] = "error \""^name^"\""
   in {reg=reg cbs, immed=immed cbs}
   end              

end
