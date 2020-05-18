(*
 * Generate the <arch>RTLProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for SSA optimizations.
 *)

functor MDRTLProps(RTL : MD_RTL) : MD_GEN2_MODULE =
struct

   structure Comp = RTL.Comp
   structure Ast  = Comp.Ast
   structure Env  = Comp.Env
   structure L    = LambdaRTL
   structure RTL  = RTL

   open Ast Comp.Util MDError

   exception Undefined
   exception NotFound

   (*
    * Generate a table of compiled RTLs for SSA use
    *)
   fun genRTLTable rtlmd =
   let val rtls = RTL.getRTLs rtlmd
       val md   = RTL.md rtlmd
       val builderName = Comp.strname md "RTL" 
       val tbl = Comp.newConstTable()
       fun mkRTL(rtl as (name, _, _)) =  
       let fun mkArg(L.IN_OUT{obj=L.OBJ(_,ty,_),input, output, name, ...}) = 
           let val e = 
                 case (output, input) of
                   (SOME x, SOME y) => 
                      APP("T.REXT",TUPLEexp[INTexp ty,
                        APP("RTL.PARAM", TUPLEexp[INTexp x,INTexp y])])
                 | (SOME x, NONE) => APP("T.REG",TUPLEexp[INTexp ty,INTexp x]) 
                 | (NONE, SOME x) => APP("T.REG",TUPLEexp[INTexp ty,INTexp x]) 
                 | (NONE, NONE)   => APP("T.REG",TUPLEexp[INTexp ty,INTexp 0]) 
           in  (name, Comp.const tbl e) end
           val arguments = map mkArg (L.inOut rtl)
           val new = IDexp(IDENT(["RTL"],"new"))
       in  VALdecl[VALbind(IDpat name,
                    APPexp(new,APPexp(IDexp(IDENT([builderName],name)),
                           RECORDexp arguments)))]
       end
       val body = map mkRTL rtls  
   in  STRUCTUREdecl("Arch",[],DECLsexp[LOCALdecl(Comp.genConsts tbl,body)])
   end

   (* Generate a call to the bug function *)
   fun bug funName = APP("bug",TUPLEexp[STRINGexp funName,ID "instr"])

   fun gen rtlmd =
   if !errorCount = 0 then 
   let (* The machine description *)
       val md = RTL.md rtlmd

       (* All cellkinds that are defined *)
       val cellKinds = Comp.cells md

       (* name of the structure/signature *)
       val strName = Comp.strname md "RTLProps"  
       val sigName = "RTL_PROPERTIES"
 
       (* query function *)
       val queryFun = RTL.queryFun rtlmd

       (* default way of handling composite instructions *)
       fun composite{instr,id,ty} = APP("query",ID id)

       (* Function for translating instructions into RTL *)
       val rtl = 
           let fun queryRTL{instr,rtl=(name,_,_),const} = 
                   {exp=IDexp(IDENT(["Arch"],name)),pat=[]}
           in  queryFun{name="rtl",extraArgs=[],args=["instr"],extraExps=[],
                        localDecls=[],body=queryRTL,
                        composite=composite}
           end

       (* 
        * Function for translating instructions into def/use 
        *)
       fun mkDefUse{name,predefined,item} = 
       let fun kindOf k = IDexp(IDENT(["C"],k))
           fun withKind k e = APP("withKind",TUPLEexp[kindOf k,e])
           fun queryDefUse{instr,rtl,const} =
           let fun get(f,r,e) = cons(APP(f,r),e)

               (* Translate each def/use into the appropriate function *)
               fun items defUse objs =
                    RTL.queryExp rtlmd
                    {name    = name,
                     reg     = fn (r,k,l) => cons(item(r,kindOf k),l),
                     fixreg  = fn (r,k,l) => cons(item(r,kindOf k),l),
                     regs    = fn (rs,k,l) => append(withKind k rs,l),
                     opnd    = fn (x,l) => get("getOpnd",x,l),
                     lab     = fn (x,l) => l,
                     imm     = fn (x,l) => get("getImm",x,l),
                     cellset = fn (c,l) => append(APP("getCellSet",c),l),
                     region  = fn (r,l) => append(APP("getRegion"^defUse,r),l)
                    } (objs, LISTexp([],NONE))

               val (defs,uses) = L.defUse rtl
           in  {exp=TUPLEexp[items "Def" defs, items "Use" uses], pat=[]} 
           end

           val getOpnd =
                RTL.queryOpnd rtlmd
                   {name      = "getOpnd'",
                    extraArgs = [],
                    reg       = fn r => r,
                    imm       = fn i => APP("immed",i),
                    opnd      = fn i => APP("operand",i),
                    default   = TUPLEexp []
                   }
           val cellSets = Comp.cellSets md
           val cellSetNames = map (fn CELLdecl{id,...} => id) cellSets
           val getCellSet =
               FUN("getCellSet",TUPLEpat(map IDpat cellSetNames),
                   foldr (fn (x,LISTexp ([],NONE)) => withKind x (ID x)
                           | (x,e) => APP("@",TUPLEexp[withKind x (ID x),e])) 
                         (LISTexp([],NONE)) cellSetNames)
       in  queryFun{name=name,
                    extraArgs=["immed","operand"],
                    args=["instr"],
                    extraExps=[],localDecls=[getOpnd,predefined,getCellSet], 
                    body=queryDefUse, composite=composite}
       end

       (* Function for translating instructions into def/use *)
       val defUse = 
           mkDefUse{name= "defUse",
                    item= fn(e,k) => e,
                    predefined =
                     $["fun getOpnd x = getOpnd' x",
                       "fun getImm i  = immed i", 
                       "fun getRegionUse r = RegionProps.readFrom r",
                       "fun getRegionDef r =",
                       "let val (d,u) = RegionProps.writeTo r",
                       "in  d end",
                       "fun withKind(k,l) = l"
                      ]
                   }
       (* Function for translating instructions into def/use with 
        * cellkind information 
        *)
       val defUseWithCellKind = 
           mkDefUse{name= "defUseWithCellKind",
                    item= fn(e,k) => TUPLEexp[e,k],
                    predefined =
                     $["fun getOpnd x = (getOpnd' x,C.GP)",
                       "fun getImm x  = (immed x,C.GP)",
                       "fun getRegionUse r = ",
                       "  map (fn r => (r,C.MEM)) (RegionProps.readFrom r)",
                       "fun getRegionDef r = ",
                       "  let val (d,u) = RegionProps.writeTo r",
                       "  in  map (fn r => (r,C.MEM)) d end",
                       "fun withKind(k,l) = map (fn x => (x,k)) l"
                      ]
                   }
 
       (* Function for translating instructions into the operand kinds *)
       val opnKind = 
       let fun queryOpnKind{instr,rtl,const} =
           let val REG  = ID "REG"
               val FIX  = ID "FIX"
               val IMM  = ID "IMM"
               val CTRL = ID "CTRL"
               val inOut = L.inOut rtl
               fun isInOut (IDexp(IDENT([],r))) =
                  List.exists 
                  (fn L.IN_OUT{name,input=SOME _,output=SOME _, ...} => name = r
                    | _ => false) inOut
                 | isInOut _ = false
               fun nonRenameable "GP" = false
                 | nonRenameable "FP" = false
                 | nonRenameable _ = true
               fun items defUse objs =
               let val (e,isConst) = 
                    RTL.queryExp rtlmd
                    {name    = "opnKind",
                     reg     = fn (r,"CTRL",(l,c)) => (cons(CTRL,l),c)
                                | (r,k,(l,c)) => 
                                     (cons(if isInOut r orelse nonRenameable k 
                                           then FIX else REG,l),c),
                     fixreg  = fn (r,"CTRL",(l,c)) => (cons(CTRL,l),c)
                                | (r,k,(l,c))      => (cons(FIX,l),c),
                     regs    = fn (rs,k,(l,c)) => 
                             (append(APP("REGs",rs),l),false),
                     opnd    = fn (x,(l,c)) => (cons(IMM,l),c), (* XXX *)
                     lab     = fn (x,(l,c)) => (l,c),
                     imm     = fn (x,(l,c)) => (cons(IMM,l),c),
                     cellset = fn (x,(l,c)) => 
                             (append(APP("getCellSet",x),l),false),
                     region  = fn (x,(l,c)) => 
                             (append(APP("getRegion"^defUse,x),l),false)
                    } (objs, (LISTexp([],NONE),true))
               in  if isConst then const e else e end
               val (defs,uses) = L.defUse rtl
           in  {exp=TUPLEexp[items "Def" defs, items "Use" uses], pat=[]}
           end
           val REGs = $["fun REGs rs = map (fn _ => REG) rs"]
           val FIXs = $["fun FIXs rs = map (fn _ => FIX) rs"]
           val MEMs = $["fun MEMs rs = map (fn _ => MEM) rs"]
           val cellSets = Comp.cellSets md
           val cellSetNames = map (fn CELLdecl{id,...} => id) cellSets
           val getCellSet = 
                FUN("getCellSet",TUPLEpat(map IDpat cellSetNames),
                    foldr (fn (x,l) => append(APP("FIXs",ID x),l)) 
                          (LISTexp([],NONE)) cellSetNames)
   
           val getRegion = 
               $["fun getRegionUse r = MEMs (RegionProps.readFrom r)",
                 "fun getRegionDef r = ",
                 "let val (d,u) = RegionProps.writeTo r",
                 "in  MEMs d end"
               ]
   
       in  queryFun{name="opnKind",extraArgs=[],args=["instr"],
                    extraExps=[],
                    localDecls=[REGs,FIXs,MEMs,getCellSet,getRegion],
                    body=queryOpnKind,
                    composite=composite
                   }
       end

                    
       (* Function for updating the cellkinds of instructions *)
       val updateCellKind = 
       let fun queryCellKind{instr,rtl,const} = 
           let fun enter(r,k,l) = APP("enter"^k,r)::l
               fun appenter(r,k,l) = APPexp(APP("app",ID("enter"^k)),r)::l
               val (defs,uses)       = L.defUse rtl
               fun items defUse objs =
                    RTL.queryExp rtlmd
                    {name    = "updateCellKind",
                     reg     = fn (r,"GP",l) => l
                                | (r, k, l)  => enter(r,k,l),
                     fixreg  = fn (r,"GP",l) => l
                                | (r, k, l)  => enter(r,k,l),
                     regs    = fn (rs,"GP",l) => l
                                | (rs,k,l) => appenter(rs,k,l),
                     opnd    = fn (x,l) => l,
                     lab     = fn (x,l) => l,
                     imm     = fn (x,l) => l,
                     cellset = fn (x,l) => APP("enterCellSet",x)::l,
                     region  = fn (x,l) => APP("enterRegion"^defUse,x)::l
                    } (objs, [])
           in  {exp=SEQexp(items "Def" defs @ items "Use" uses),pat=[]} end

           (* Generate an enter function for each cellkind *)
           val cellKinds = Comp.cells md
           val enterFuns = 
               FUNdecl(map (fn CELLdecl{id, ...} => 
                  FUNbind("enter"^id,
                     [CLAUSE([IDpat "r"],
                       APP("update",TUPLEexp[ID "r",IDexp(IDENT(["C"],id))]))]))
                      cellKinds)
           val cellSets = Comp.cellSets md
           val enterCellSet =
               FUN("enterCellSet",
                  TUPLEpat(map (fn CELLdecl{id,...} => IDpat id) cellSets),
                  SEQexp(foldr (fn (CELLdecl{id="GP", ...},e) => e
                                 | (CELLdecl{id,...},e) =>
                             APPexp(APP("app",ID("enter"^id)),ID id)::e) [] 
                         cellSets))
           val enterRegion =
               $["fun enterRegionUse r = app enterMEM (RegionProps.readFrom r)",
                 "fun enterRegionDef r = ",
                 "let val (d,u) = RegionProps.writeTo r",
                 "in  app enterMEM d; app enterMEM u end"
                ]
       in  queryFun{name="updateCellKind",extraArgs=["update"],args=["instr"],
                    extraExps=[],
                    localDecls=[enterFuns,enterCellSet,enterRegion],
                    body=queryCellKind, composite=composite}
       end

       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES",
            "structure Asm : INSTRUCTION_EMITTER where I = Instr",
            "  sharing Instr.Region = RegionProps.Region"
           ]

       (* The functor *)
       val strBody = 
           [$ ["structure I   = Instr",
               "structure C   = I.C",
               "structure RTL = MLTreeRTL",
               "structure T   = RTL.T",
               "",
               "datatype opnkind =",
               "  IMM     (* a constant operand *)",
               "| REG     (* can be renamed *)",
               "| FIX     (* cannot be renamed *)",
               "| MEM     (* memory *)",
               "| CTRL    (* control dependence *)",
               ""
              ],
            ERRORfun strName,
            $ ["",
               "fun bug(msg,instr) =",
               "let val Asm.S.STREAM{emit, ...} = Asm.makeStream []",
               "in  emit (fn r => r) instr; error msg end",
               "",
               "structure "^Comp.strname md "RTL"^" = "^
                            Comp.strname md "RTL"^"(BuildRTL)"
              ],
            genRTLTable rtlmd,
            rtl,
            defUse,
            defUseWithCellKind,
            opnKind,
            updateCellKind
           ]

   in  Comp.codegen md "mltree/RTLProps"
         [Comp.mkFct md "RTLProps" args sigName (map Comp.simpDecl strBody)
         ]
   end
   else ()
end
