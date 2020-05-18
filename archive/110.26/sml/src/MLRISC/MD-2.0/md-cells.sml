(*
 * Generate the <arch>Cells structure.
 * This structure contains various information about the register
 * properties of the architecture.
 *)

functor MDCells(Comp : MD_COMPILE) : MD_GEN_MODULE =
struct

   structure Ast  = Comp.Ast
   structure Comp = Comp

   open Ast Comp.Util

   fun gen md =
   let (* name of the structure *)
       val strName = Comp.strname md "Cells"  
       val sigName = Comp.signame md "CELLS"

       (* all cell kinds *)
       val cellKinds = Comp.cells md

       (* Process *) 
       fun process([], r) = r
         | process(CELLdecl{id, from, to, count, ...}::ds, r) =
           let val count = case count of NONE => 0 | SOME c => c
           in  from := r;
               to   := r + count - 1;
               process(ds, r+count)
           end
  
       val firstPseudo = process(cellKinds, 0)
            

       (* all cell kind names *)
       val cellKindNames = map (fn CELLdecl{id, ...} => id) cellKinds

       val allCellKindNames = cellKindNames @ [(* "MEM", "CTRL", *) "UNKNOWN"]

       (* cellkinds that has to be put into the cellset *)
       val cellSets = Comp.cellSets md
       val cellSets' = Comp.cellSetsAliases md

       val cellSetNames = map (fn CELLdecl{id, ...} => id) cellSets

       (* locations *)
       val locations = Comp.locations md
   
       (* Datatype representing the cellkind and cellset 
        * The type MEM and CTRL are always added 
        *)
       val cellDatatype = 
         DATATYPEdecl
            ([DATATYPE("mycellkind", [],
                (map (fn id => CONS(id, NONE)) allCellKindNames ))
             ],
             [TYPEbind("cellset", [],
                TUPLEty(map (fn _ => INTLISTty) cellSets))
             ]
            )

       (* Functions showXXX *)
       val TYty       = IDty(IDENT([],"ty"))
       val showFunTy  = FUNty(REGISTERty, STRINGty)
       val showWithTypeFunTy = FUNty(TUPLEty[REGISTERty,TYty], STRINGty)
       val showFunSig = 
            VALSIGdecl(map (fn k => "show"^k) cellKindNames,showFunTy)
       val showWithTypeFunSig = 
            VALSIGdecl(map (fn k => "show"^k^"WithType") 
                       cellKindNames,showWithTypeFunTy)
       val showWithTypeFuns =
           let fun shift(from, to) e = 
                   if !from = 0 then e
                   else LET([VAL("r",
                           IFexp(APP("<=",TUPLEexp[ID "r",INTexp(!to)]),
                                 APP("-",TUPLEexp[ID "r",INTexp(!from)]),
                                 ID "r"))],e)
           in  FUNdecl(
                map (fn CELLdecl{id, from, to, print, ...} =>
                 FUNbind("show"^id^"WithType",
                  [CLAUSE([TUPLEpat[IDpat "r",IDpat "ty"]],
                          shift (from, to) (APPexp(print,
                              TUPLEexp[ID "r",ID "ty"])))]))
                    cellKinds)
           end

       val showFuns = 
           SEQdecl(map (fn CELLdecl{id, from, to, print, bits, ...} =>
               FUN("show"^id,IDpat "r",
                       APP("show"^id^"WithType",TUPLEexp[ID "r",INTexp bits])))
                          cellKinds)
   
       (* Function toStringWithType *)
       val toStringWithTypeFun =
           let fun show id prefix =
                 CLAUSE([IDpat id],
                   LAMBDAexp[CLAUSE([TUPLEpat[IDpat "r",IDpat "ty"]],
                     APP("^",TUPLEexp[STRINGexp prefix,
                       APP("Int.toString",ID "r")]))])
           in  FUNdecl[
                 FUNbind("toStringWithType",
                     map (fn k => CLAUSE([IDpat k],ID("show"^k^"WithType"))) 
                           cellKindNames @ [show "UNKNOWN" "unknown"]
                        )]
           end
       (* Function toString *)
       val toStringFun =
           let fun show id prefix =
                 CLAUSE([IDpat id],
                   LAMBDAexp[CLAUSE([IDpat "r"],
                     APP("^",TUPLEexp[STRINGexp prefix,
                       APP("Int.toString",ID "r")]))])
           in  FUNdecl[
                 FUNbind("toString",
                     map (fn k => CLAUSE([IDpat k],ID("show"^k))) 
                           cellKindNames @ [show "UNKNOWN" "unknown"]
                        )]
           end

       (* Functions addXXX *)
       val addFunTy = FUNty(TUPLEty[REGISTERty,CELLSETty],CELLSETty)
       val addFunSig = VALSIGdecl(map (fn s => "add"^s) cellSetNames, addFunTy)

       (* Function cellKindToString *)
       val cellKindToStringFun =
           VAL("cellkindToString",LAMBDAexp( 
               map (fn k => CLAUSE([IDpat k],STRINGexp k)) 
                 allCellKindNames))

       (* create CellsBasis *)
       val cellsBasis =
           STRUCTUREdecl("MyCellsBasis",[],
             APPsexp(IDENT([],"CellsBasis"),
               DECLsexp
               [$["type cellkind = mycellkind",
                  "exception Cells = "^strName,
                  "val unknown = UNKNOWN",
                  "val cellkindToString = cellkindToString",
                  "val INT = GP",
                  "val FLOAT = FP",
                  "val firstPseudo = 256"
                 ],
                VAL("kinds", LISTexp(map ID allCellKindNames,NONE)),
                VAL("physical",
                    LISTexp(map (fn CELLdecl{from,to,id,...} =>
                       RECORDexp[("from",INTexp(!from)),
                                 ("to",INTexp(!to)),
                                 ("kind",ID id)]) cellKinds,NONE))
               ]))

       (* Offsets of cellkind encoding *)
       val offsets =
            VALdecl(map (fn CELLdecl{from, id, ...} =>
                         VALbind(IDpat("offset"^id), INTexp(!from))) cellKinds) 

       (* User defined locations *)
       val locationsSig = 
            map (fn LOCbind(id,NONE,_) => VALSIGdecl([id],REGISTERty)
                  | LOCbind(id,SOME _,_) =>
                     VALSIGdecl([id],FUNty(INTty,REGISTERty)))
                locations
       val locationsFun =
            map (fn LOCbind(id,NONE,e) => VAL(id,e)
                  | LOCbind(id,SOME p,e) => VAL(id,LAMBDAexp[CLAUSE([p],e)]))
                locations

       (* Empty cellset *)
       val emptyCellSet = 
           VAL("empty",TUPLEexp(map (fn _ => LISTexp([],NONE)) cellSets))

       (* Names of cellset *)
       val namesOfCellSets =
           VAL("cellsetnames",LISTexp(map STRINGexp cellSetNames,NONE))

       fun set k = ID("set"^k)

       (* Functions zeroReg *)
       val zeroReg =
       let fun mkZeroReg(CELLdecl{id, zero=SOME r, ...}::cs) = 
                  CLAUSE([IDpat id],APP("SOME",LOCexp(id,INTexp r,NONE)))::
                  mkZeroReg cs
             | mkZeroReg(_::cs) = mkZeroReg cs
             | mkZeroReg [] = [CLAUSE([WILDpat],ID "NONE")]
       in  FUNdecl[FUNbind("zeroReg",mkZeroReg cellKinds)]
       end

       (* Functions {add/rmv/get/update}Cells ... *)
       val cellFuns =
       let fun cellFun prefix (CELLdecl{id,alias=NONE, ...}) = 
                 CLAUSE([IDpat id], ID(prefix^id))
             | cellFun prefix (CELLdecl{id,alias=SOME id', ...}) = 
                 CLAUSE([IDpat id], ID(prefix^id'))
           fun mkCellFun prefix cellsets =
                 FUNbind (prefix^"Cell",
                       map (cellFun prefix) cellsets @ [ERROR(prefix^"Cell")])

           val pat1 = TUPLEpat(map (fn k => IDpat("set"^k)) cellSetNames) 
           val pat2 = TUPLEpat[IDpat "r",pat1] 
           val pat3 = TUPLEpat[pat1,IDpat "r"] 
           fun mkAddFun k =
               FUNbind("add"^k, 
                [CLAUSE([pat2], 
                   TUPLEexp(map (fn k' => if k=k' then 
                              APP("SL.enter",TUPLEexp[ID "r",set k']) else
                              set k') cellSetNames))])
           fun mkRmvFun k =
               FUNbind("rmv"^k, 
                [CLAUSE([pat2], 
                   TUPLEexp(map (fn k' => if k=k' then 
                              APP("SL.rmv",TUPLEexp[ID "r",set k']) else
                              set k') cellSetNames))])
           fun mkGetFun k =
               FUNbind("get"^k, [CLAUSE([pat1],set k)])
           fun mkUpdateFun k =
               FUNbind("update"^k, 
                 [CLAUSE([pat3], 
                   TUPLEexp(map (fn k' => 
                      if k=k' then ID "r" else set k') cellSetNames))])
           fun mkCellSetToString cellSetNames =
               FUNbind("cellsetToString",
                 [CLAUSE([pat1],
                    APP("printTuple", 
                      TUPLEexp[ID "cellsetnames", LISTexp(
                       map (fn k => 
                          APPexp(APP("printSet", ID("show"^k)), 
                              set k)) cellSetNames, NONE)]
                       ))])
           fun mkCellSetToString' cellSetNames = 
                FUNbind("cellsetToString'",
                    [CLAUSE([IDpat"regmap"],
                       LAMBDAexp[CLAUSE([pat1], 
                         APP("cellsetToString",
                           TUPLEexp(map (fn k => 
                             APPexp(APP("map", ID "regmap"), set k)) 
                                cellSetNames)))])])
                         
           fun mkCellSetToCells cellSetNames =
               FUNbind("cellsetToCells",
                 [CLAUSE([pat1], 
                     foldr (fn (k, LISTexp([],NONE)) => set k
                             | (k, e) => APP("@",TUPLEexp[set k,e]))
                           (LISTexp([],NONE)) cellSetNames)])
       in 
           FUNdecl(
             [mkCellFun "add" cellSets',
              mkCellFun "rmv" cellSets',
              mkCellFun "get" cellSets',
              mkCellFun "update" cellSets'
             ] @ 
               map mkAddFun cellSetNames @
               map mkRmvFun cellSetNames @
               map mkGetFun cellSetNames @
               map mkUpdateFun cellSetNames @
             [ mkCellSetToString cellSetNames, 
               mkCellSetToString' cellSetNames,
               mkCellSetToCells cellSetNames 
             ]
           )
       end

       (* body of signature *) 
       val sigBody = 
          [cellDatatype, 
           $["include CELLS_BASIS where type cellkind = mycellkind"],
           showFunSig,
           showWithTypeFunSig,
           SEQdecl locationsSig,
           addFunSig,
           $["val zeroReg : cellkind -> cell option",
             "val toString : cellkind -> cell -> string",
             "val toStringWithType : cellkind -> cell * ty -> string",
             "val addCell : cellkind -> cell * cellset -> cellset",
             "val rmvCell : cellkind -> cell * cellset -> cellset",
             "val addReg : cell * cellset -> cellset",
             "val rmvReg : cell * cellset -> cellset",
             "val addFreg : cell * cellset -> cellset",
             "val rmvFreg : cell * cellset -> cellset",
             "val getCell : cellkind -> cellset -> cell list",
             "val updateCell : cellkind -> cellset * cell list -> cellset",
             "val empty : cellset",
             "val cellsetToString : cellset -> string",
             "val cellsetToString' : (cell -> cell) -> cellset -> string",
             "val cellsetToCells : cellset -> cell list"
            ]
          ]
                        
       (* body of structure *) 
       val strBody = 
           [cellDatatype,
            $["exception "^strName,
              "structure SL = SortedList",
              "fun error msg = MLRiscErrorMsg.error(\""^strName^"\",msg)"
             ],
            cellKindToStringFun,
            cellsBasis,
            $["open MyCellsBasis"],
            offsets,
            SEQdecl locationsFun,
            zeroReg,
            showWithTypeFuns,
            showFuns,
            toStringWithTypeFun,
            toStringFun,
            namesOfCellSets,
            emptyCellSet,
            cellFuns,
            $["val addReg = addGP",
              "val addFreg = addFP",
              "val rmvReg = rmvGP",
              "val rmvFreg = rmvFP"
             ],
            Comp.declOf md "Cells"
           ] 
  
   in  
       Comp.codegen md "instructions/Cells" 
        [Comp.mkSig md "CELLS" sigBody,
         Comp.mkStr md "Cells" sigName strBody]

   end

end
