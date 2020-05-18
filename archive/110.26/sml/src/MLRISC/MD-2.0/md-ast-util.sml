functor MDAstUtil(Ast : MD_AST) : MD_AST_UTIL =
struct

   structure Ast = Ast
   open Ast

   fun ID id = IDexp(IDENT([],id))
   fun APP(f,e) = APPexp(ID f,e)
   fun BINOPexp(f,x,y) = APP(f,TUPLEexp[x,y])
   fun PLUS(a,INTexp 0) = a
     | PLUS(a,WORDexp 0w0) = a
     | PLUS(INTexp 0,a) = a
     | PLUS(WORDexp 0w0,a) = a
     | PLUS(a,b) = BINOPexp("+",a,b)
   fun MINUS(a,INTexp 0) = a
     | MINUS(a,WORDexp 0w0) = a
     | MINUS(a,b) = BINOPexp("-",a,b)
   fun ANDB(a,b) = BINOPexp("&&",a,b)
   fun ORB(a,b) = BINOPexp("||",a,b)
   fun SLL(a,WORDexp 0w0) = a
     | SLL(a,b) = BINOPexp("<<",a,b)
   fun SLR(a,WORDexp 0w0) = a
     | SLR(a,b) = BINOPexp(">>",a,b)
   fun SAR(a,WORDexp 0w0) = a
     | SAR(a,b) = BINOPexp("~>>",a,b)
   val UNIT = TUPLEexp []
   val TRUE = BOOLexp true
   val FALSE = BOOLexp false
   fun ANDALSO(BOOLexp true,x) = x
     | ANDALSO(BOOLexp false,x) = FALSE
     | ANDALSO(x,BOOLexp true) = x
     | ANDALSO(x,BOOLexp false) = FALSE
     | ANDALSO(x,y) = BINOPexp("andalso",x,y)
   fun ORELSE(BOOLexp true,x) = TRUE
     | ORELSE(BOOLexp false,x) = x
     | ORELSE(x,BOOLexp true) = TRUE
     | ORELSE(x,BOOLexp false) = x
     | ORELSE(x,y) = BINOPexp("orelse",x,y)

   val UNITty = IDty(IDENT([],"unit"))
   val BOOLty = IDty(IDENT([],"bool"))
   val INTty = IDty(IDENT([],"int"))
   val REGISTERty = IDty(IDENT([],"cell"))
   val REGISTERLISTty = APPty(IDENT([],"list"),[REGISTERty])
   val INTLISTty = APPty(IDENT([],"list"),[INTty])
   val STRINGty = IDty(IDENT([],"string"))
   val WORD32ty = IDty(IDENT(["Word32"],"word"))
   val WORDty = IDty(IDENT(["Word"],"word"))
   val LABELty = IDty(IDENT(["Label"],"label"))
   val LABEXPty = IDty(IDENT(["LabelExp"],"labexp"))
   val CONSTty = IDty(IDENT(["Constant"],"const"))
   val CELLKINDty = IDty(IDENT([],"cellkind"))
   val CELLSETty = IDty(IDENT([],"cellset"))

   fun DATATYPE(id,args,cbs) = 
        DATATYPEbind{id=id,tyvars=args,mc=NONE,asm=false,field=NONE,cbs=cbs}
   fun CONS(id,arg) = CONSbind{id=id,ty=arg,mc=NONE,asm=NONE,rtl=NONE,
                               nop=FLAGoff,nullified=FLAGoff,
                               delayslot=(DELAY_NONE,DELAY_NONE),
                               delaycand=NONE,sdi=NONE,latency=NONE,
                               pipeline=NONE, loc=SourceMap.dummyLoc}
   fun VAL(id,e) = VALdecl [VALbind(IDpat id,e)]
   fun FUN'(id,p,e) = FUNbind(id,[CLAUSE([p],e)])
   fun FUN(id,p,e) = FUNdecl [FUN'(id,p,e)]
   fun LET([],e) = e 
     | LET(d,e) = LETexp(d,[e])

   fun ERROR text = CLAUSE([WILDpat],APP("error",STRINGexp text))
   fun ERRORfun name = 
       $["fun error msg = MLRiscErrorMsg.error(\""^name^"\",msg)"]
   fun DUMMYfun name = 
       $["fun "^name^" _ = error \""^name^"\""]

   fun BITSLICE(e,ranges) =
   let val temp = ID "temp"
       fun gen(tmp, [], pos, e) = e
         | gen(tmp, (a,b)::slices, pos, e) =
           let val width = b - a + 1
               val mask  = Word32.<<(0w1, Word.fromInt width) - 0w1
               val field = SLL(tmp, WORDexp(Word32.fromInt a))
               val field = ANDB(field, WORDexp mask)
           in  gen(tmp, slices, pos+width,
                   PLUS(SLL(field, WORDexp(Word32.fromInt pos)),e))
           end
       fun emit(tmp) = gen(tmp, rev ranges, 0, WORDexp 0w0)
   in  case ranges of
         [_] => emit e
       | _   => LETexp([VALdecl[VALbind(IDpat "temp",e)]], [emit(ID "temp")])
   end

      (* Add an entry *)
   fun cons(x,LISTexp(a,b)) = LISTexp(x::a,b)
     | cons(x,y)            = LISTexp([x],SOME y)

   (* Append an entry *)
   fun append(x,LISTexp([],NONE)) = x
     | append(x,y) = APP("@",TUPLEexp[x,y])

end
