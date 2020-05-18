(*
 * Generate the <arch>SSAProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for SSA optimizations.
 *)

functor MDSSAProps(RTL : MD_RTL) : MD_GEN2_MODULE =
struct

   structure Comp = RTL.Comp
   structure Ast  = Comp.Ast
   structure Env  = Comp.Env
   structure L    = LambdaRTL
   structure RTL  = RTL

   open Ast Comp.Util MDError

   exception Undefined

     (* Insert copies *)

   fun copyFuns hasImpl = 
   let val (implInit,implPat,implCopy) = 
              if hasImpl then
                 ("impl=ref NONE,","impl,", "impl=impl,")
              else 
                 ("", "", "")
   in
    $["fun copies cps =",
      "let fun f([],id,is,fd,fs) = (id,is,fd,fs)",
      "      | f({kind,dst,src}::cps,id,is,fd,fs) =",
      "        if dst=src then f(cps,id,is,fd,fs)",
      "        else case kind of",
      "             C.GP   => f(cps,dst::id,src::is,fd,fs)",
      "          |  C.FP   => f(cps,id,is,dst::fd,src::fs)",
      "          |  C.MEM  => f(cps,id,is,fd,fs)",
      "          |  C.CTRL => f(cps,id,is,fd,fs)",
      "          |  _      => error(\"copies: \"^C.cellkindToString kind^",
      "                             \" dst=\"^C.toString kind dst^",
      "                             \" src=\"^C.toString kind src)",
      " val (id,is,fd,fs) = f(cps,[],[],[],[])",
      " val icopy = case id of",
      "               []  => []",
      "             | [_] => [I.COPY{src=is,dst=id,"^implInit^"tmp=NONE}]",
      "             | _   => [I.COPY{src=is,dst=id,"^implInit,
      "                              tmp=SOME(I.Direct(C.newReg()))}]",
      " val fcopy = case fd of",
      "               []  => []",
      "             | [_] => [I.FCOPY{src=fs,dst=fd,"^implInit^"tmp=NONE}]",
      "             | _   => [I.FCOPY{src=fs,dst=fd,"^implInit,
      "                               tmp=SOME(I.FDirect(C.newFreg()))}]",
      "in icopy @ fcopy end",
      "",
      "fun copy{instr=I.COPY{"^implPat^"tmp,...},dst=dst as [_],src} =",
      "        I.COPY{"^implCopy^"tmp=NONE,dst=dst,src=src}",
      "  | copy{instr=I.COPY{"^implPat^"tmp,...},dst,src} =",
      "        I.COPY{"^implCopy^"tmp=tmp,dst=dst,src=src}",
      "  | copy{instr=I.FCOPY{"^implPat^"tmp,...},dst=dst as [_],src} =",
      "        I.FCOPY{"^implCopy^"tmp=NONE,dst=dst,src=src}",
      "  | copy{instr=I.FCOPY{"^implPat^"tmp,...},dst,src} =",
      "        I.FCOPY{"^implCopy^"tmp=tmp,dst=dst,src=src}",
      "  | copy{instr=I.ANNOTATION{i,a},dst,src} =",
      "        I.ANNOTATION{i=copy{instr=i,dst=dst,src=src},a=a}",
      "  | copy{instr,...} = bug(\"copy\",instr)"
   ]
   end


   exception NotFound

   (* Generate a call to the bug function *)
   fun bug funName = APP("bug",TUPLEexp[STRINGexp funName,ID "instr"])

   (* Expressions building utilities *)
   fun cons(x,LISTexp(a,b)) = LISTexp(x::a,b)
     | cons(x,y) = LISTexp([x],SOME y)
   fun append(x,LISTexp([],NONE)) = x 
     | append(x,y) = APP("@",TUPLEexp[x,y])
   val NIL = LISTexp([],NONE)

   fun gen rtlmd =
   if !errorCount = 0 then 
   let (* The machine description *)
       val md = RTL.md rtlmd

       (* All cellkinds that are defined *)
       val cellKinds = Comp.cells md

       (* name of the structure/signature *)
       val strName = Comp.strname md "SSAProps"  
       val sigName = "SSA_PROPERTIES"
 
       (* query function *)
       val queryFun = RTL.queryFun rtlmd

       (* default way of handling composite instructions *)
       fun composite{instr,id,ty} = APP("query",ID id)

       (* Create a list pattern that pattern matches on the ssa input/output
        * arguments.
        *)
       fun rmvWild [] = []
         | rmvWild(WILDpat::pats) = rmvWild pats
         | rmvWild pats = pats
       fun mkPat(suffix, objs) = 
       let fun scan(i, L.OBJ(L.PARAM r,_,L.REG k)::objs, pats, NONE) = 
                  scan(i+1, objs, IDpat(r^suffix)::pats, NONE)
             | scan(i, L.OBJ(L.PARAM r,_,L.OPND)::objs, pats, NONE) = 
                  scan(i+1, objs, IDpat(r^suffix)::pats, NONE)
             | scan(i, L.OBJ(L.PARAM r,_,L.REG k)::objs, pats, SOME _) = 
                  fail "mkpat"
             | scan(i, 
                    L.OBJ(L.PARAM _,_, (L.REGS _ | L.REGION | L.CELLSET))::objs,
                    pats, opt)= 
                 scan(i+1, objs, rmvWild pats, SOME WILDpat)
             | scan(i, L.OBJ(L.PARAM r,_,L.LAB)::objs, pats, opt) = 
                 scan(i+1, objs, pats, opt)
             | scan(i, L.OBJ(L.CONST _,_,L.REG k)::objs, pats, opt) = 
                 scan(i+1, objs, WILDpat::pats, opt)
             | scan(i, _::objs, pats, NONE) = 
                 scan(i+1, objs, WILDpat::pats, NONE)
             | scan(i, _::objs, pats, opt) = 
                 scan(i+1, objs, pats, opt)
             | scan(i, [], pats, opt) = LISTpat(rev pats, opt)
       in  scan(0, objs, [], NONE) end

       (* Function for extracting naming constraints from an RTL *)
       val namingConstraints =
       let fun queryNamingConstraints{instr as CONSbind{id,...},rtl,const} = 
           let fun mark(x,y) = TUPLEexp[x,y]
               fun queryConstraints defUse objs C =
                  RTL.queryPatExp rtlmd 
                     {name   ="namingConstraints",
                      defUse = defUse,
                      reg    = fn (r,"CTRL",C) => raise RTL.UseDefault
                                | (r,"MEM",C) => raise RTL.UseDefault
                                | (r,k,C) => raise RTL.UseDefault,
                      fixreg = fn (r,i,"CTRL",C) => raise RTL.UseDefault
                                | (r,i,"MEM",C) => raise RTL.UseDefault
                                | (r,i,k,C) => cons(mark(r(),i),C),
                      regs   = fn (rs,k,C) => raise RTL.UseDefault,
                      opnd   = fn (_,C) => raise RTL.UseDefault,
                      lab    = fn (_,C) => raise RTL.UseDefault,
                      imm    = fn (_,C) => raise RTL.UseDefault,
                      cellset= fn (r,s,C) => 
                         APP("cellset",TUPLEexp[r(),s,C]),
                      region = fn (_,C) => raise RTL.UseDefault
                     } (objs, C)

               val (def,use) = L.defUse rtl
               val (dstPat,dstC) = queryConstraints "dst" def NIL
               val (srcPat,srcC) = queryConstraints "src" use NIL
               val dstsrcC = NIL
               val dstPat =
                   case dstC of
                     LISTexp([],NONE) => WILDpat
                   | _ => dstPat
               val srcPat =
                   case srcC of
                     LISTexp([],NONE) => WILDpat
                   | _ => srcPat

           in  {exp=RECORDexp([("dst",dstC),("src",srcC),("dstsrc",dstsrcC)]), 
                pat=[dstPat,srcPat]
               }
           end

           (* Handle composite instructions *)
           fun compositeNamingConstraints{instr,id,ty} = APP("query",ID id)

           val localDecls =
                $["fun cellset(dstsrc,S,C) =",
                  "let val S = C.cellsetToCells S",
                  "in  List.revAppend(ListPair.zip(dstsrc,S),C) end"
                 ]

       in  queryFun{name="namingConstraints",extraArgs=[],
                    extraExps=["dst","src"],
                    args=["instr","dst","src"],localDecls=[localDecls],
                    body=queryNamingConstraints, 
                    composite=compositeNamingConstraints}
       end

       (* Function for rewriting the operands of an RTL *)
       val rewriteOperands =
       let fun queryRewrite{instr,rtl,const} = 
           let val (def,use) = L.defUse rtl

               val defPat = mkPat("'", def)
               val usePat = mkPat("''", use)
               val inOut = L.inOut rtl 

               fun inOutOf x' = 
               let fun f((x as L.IN_OUT{name,...})::xs) = 
                        if name=x' then x else f xs
                     | f _ = raise NotFound
               in  f inOut end

               fun rewriteArg(arg,ty) = 
               let val L.IN_OUT{obj, input, output, ...} = inOutOf arg
               in  case (obj, input, output) of
                    (L.OBJ(L.PARAM r,_,L.REG _),SOME _,NONE) => ID(arg^"''")
                   |(L.OBJ(L.PARAM r,_,L.REG _),NONE,SOME _) => ID(arg^"'")
                   |(L.OBJ(L.PARAM r,_,L.REG _),SOME _,SOME _) => ID(arg^"'")
                   |(L.OBJ(L.PARAM r,_,L.OPND),SOME _,NONE) => 
                      APP("rwOpnd",ID(arg^"''"))
                      (* renaming constraints!!! XXX *)
                   | _ => raise NotFound
               end handle NotFound => ID arg

          in  {exp=Env.consToExp {cons=instr, prefix="I", exp=rewriteArg},
                pat=[defPat,usePat]
               }
          end

          (* Handle composite instructions *)
          fun compositeRewrite{instr,id,ty} =
          let fun rewriteArg(x,_) =
                   if x = id then APP("query",ID x) else ID x
          in  Env.consToExp{cons=instr, prefix="I",exp=rewriteArg} end

          (* Handle operands *)
          val {reg, immed} = RTL.queryGenOpnd rtlmd {name = "rwOpnd"}
          val rwOpnd = 
               $["fun rwOpnd v =",
                 "if v >= 0 then "^reg^"(v)",
                 "else (case const v of",
                 "       OT.OPERAND opnd => opnd",
                 "     | OT.IMMED i => "^immed^"(i)",
                 "     )"
                ]
 
                  

 
       in  queryFun{name="rewriteOperands",extraArgs=["const"],
                    extraExps=["dst","src"],
                    args=["instr","dst","src"],localDecls=[rwOpnd],
                    body=queryRewrite, composite=compositeRewrite}
       end

       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES ",
            "structure RTLProps : RTL_PROPERTIES where I = Instr",
            "structure Asm : INSTRUCTION_EMITTER where I = Instr", 
            "structure OperandTable : OPERAND_TABLE where I = Instr",
            "  sharing RegionProps.Region = Instr.Region",
            "val volatile : Instr.C.cell list",
            "val pinnedDef  : Instr.C.cell list",
            "val pinnedUse  : Instr.C.cell list",
            "val fixedDef   : Instr.C.cell list",
            "val fixedUse   : Instr.C.cell list"
           ]

       (* The functor *)
       val strBody = 
           [$ ["structure I        = Instr",
               "structure C        = I.C",
               "structure RTLProps = RTLProps",
               "structure RTL      = RTLProps.RTL",
               "structure T        = RTL.T",
               "structure OT       = OperandTable",
               "structure RP       = RegionProps",
               "",
               "datatype const = datatype OT.const",
               ""
              ],
            ERRORfun strName,
            $ ["",
               "fun bug(msg,instr) =",
               "let val Asm.S.STREAM{emit, ...} = Asm.makeStream []",
               "in  emit (fn r => r) instr; error msg end",
               "",
               "val volatile = volatile",
               "val pinnedDef = pinnedDef",
               "val pinnedUse = pinnedUse",
               "val fixedDef  = fixedDef",
               "val fixedUse  = fixedUse",
               "val source = I.SOURCE{}",
               "val sink   = I.SINK{}",
               "val phi    = I.PHI{}",
               ""
              ],
            namingConstraints,
            rewriteOperands,
            copyFuns (Comp.hasCopyImpl md),
            Comp.declOf md "SSA"
           ]

   in  Comp.codegen md "SSA/SSAProps"
         [Comp.mkFct md "SSAProps" args sigName (map Comp.simpDecl strBody)
         ]
   end
   else ()
end
