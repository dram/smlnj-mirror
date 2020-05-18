(*
 * Generate the <arch>Rewrite functor.
 * This module performs register renaming.
 *)

functor MDRewrite(Comp : MD_COMPILE) : MD_GEN_MODULE =
struct

   structure Comp = Comp
   structure Ast  = Comp.Ast
   structure Env  = Comp.Env

   open Ast Comp.Util 

   fun gen md =
   let (* name of the structure/signature *)
       val strName = Comp.strname md "Rewrite"  
       val sigName = "REWRITE_INSTRUCTIONS"

       (* The instructions *)
       val instructions = Comp.instructions md

       (* The Instruction environment *)
       val env = Env.lookupStr (Comp.env md) (IDENT([],"Instruction"))

       (* All cellkinds inside cellsets *)
       val cellSets = Comp.cellSets md
       val cellSetPat = 
            TUPLEpat(map (fn CELLdecl{id, ...} => IDpat("set"^id)) cellSets)

       (* Arguments to the instruction functor *)
       val args =
           ["Instr : "^Comp.signame md "INSTR"
           ]

       datatype defUse = DEF | USE

       (*
        * Make a rewrite function of type:
        *   regmap * instruction * fromReg * toReg -> instruction
        *)
       fun mkRewrite(f, cellkind, defUse) =
       let val args = TUPLEpat[IDpat "regmap", IDpat "instr", 
                               IDpat "rs", IDpat "rt"]
           fun rewrite(cons as CONSbind{id, ...}) = 
               Env.consToClause {cons=cons, pat=fn p=>p, 
                                 exp=APP("error",STRINGexp id),
                                 prefix="I"
                                }
           val rewriteFun = FUNdecl[FUNbind("rewrite",
                             [CLAUSE([IDpat "instr"],
                                CASEexp(ID "instr",
                                   map rewrite instructions))])]
           val renameCellSet =
                FUN("renameCellSet",cellSetPat,
                    TUPLEexp(
                      map (fn CELLdecl{id, ...} => 
                            if id = cellkind 
                            then APP("renameSet", ID("set"^id))
                            else ID("set"^id)) cellSets))
           val localDecls = 
               [$["fun rename r = if regmap r = rs then rt else r",
                  "fun renameSet regs = SL.uniq(map rename regs)"
                 ],
                renameCellSet,
                rewriteFun
               ]
       in  FUNdecl[FUNbind(f,[CLAUSE([args], 
                LETexp(localDecls, [APP("rewrite",ID "instr")]))])]
       end

       val rewriteDef  = mkRewrite("rewriteDef", "GP", DEF)
       val rewriteUse  = mkRewrite("rewriteDef", "GP", USE)
       val frewriteDef = mkRewrite("frewriteDef", "FP", DEF)
       val frewriteUse = mkRewrite("frewriteUse", "FP", USE)

       (* The functor *)
       val strBody = 
           [$ ["structure I = Instr",
               "structure C = I.C",
               "structure SL = SortedList",
               ""
              ],
            rewriteDef,
            rewriteUse,
            frewriteDef,
            frewriteUse
           ]

   in  Comp.codegen md "ra/Rewrite2"
         [Comp.mkFct md "Rewrite" args sigName strBody
         ]
   end
end
