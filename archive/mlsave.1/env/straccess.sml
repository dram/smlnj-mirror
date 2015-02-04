(* straccess.sml *)

structure StrAccess = struct

local open ErrorMsg Access Basics in

val checkSTR = fn STRbind s => s | _ => raisex Table.next;
val lookSTRinTable = Table.look checkSTR;

fun contextOf(DIRECT{context,...}) = context
  | contextOf(FCTAPP{context,...}) = context
  | contextOf(PARAM _) = FCTctx  (* ??? *)
  | contextOf(SPEC _) = FCTctx   (* ??? irrelevant ??? *)

fun getSign(sign, [], ap) = (sign, ap)
  | getSign(SIG{env,...}, id::rest, ap) =
      let val STRvar{binding=SPEC(sign),access=SLOT(n),...} = 
	        lookSTRinTable(env,id)
       in getSign(sign,rest,n::ap)
      end

exceptionx pnodeSon: unit

fun pnodeSon(STRpnode{sons,...},id) : int * pnode =
    let fun loop ((id',son)::rest) =
	      if Symbol.Eq(id,id') then son else loop rest
	  | loop [] = raisex pnodeSon
     in loop (!sons)
    end
  | pnodeSon _ = Impossible "straccess.83"

fun extendParam(pnode,oldpath,sign,newpath,ap) : Structure * Access.path =
    let fun loop(pnode as STRpnode{sign,...}, p as id::rest, ap) =
	      let val (slot,pnode') = pnodeSon(pnode,id)
	       in loop(pnode',rest,slot::ap)
	      end
	      handlex pnodeSon => 
	        let val (sign',ap') = getSign(sign,p,ap)
		 in (PARAM{pnode=pnode,spath=p,sign=sign'},ap')
		end
	  | loop(pnode as STRpnode{sign,...}, [],ap) =
	      (PARAM{pnode=pnode,spath=[],sign=sign},ap)
	  | loop _ = Impossible "straccess.127"
     in case oldpath
	  of [] => loop(pnode,newpath,ap)
	   | _ =>
	       let val (sign',ap') = getSign(sign,newpath,ap)
	        in (PARAM{pnode=pnode,spath=oldpath@newpath,sign=sign'},ap')
	       end
    end

exceptionx lookPnode

fun lookPnode(pnode,[]) = raisex lookPnode
  | lookPnode(pnode,(pnode',str)::rest) =
      if eqPnode(pnode,pnode')
        then str
	else lookPnode(pnode,rest)

(*  -- not used
fun isFctRelative(TOPctx: context) : bool = false
  | isFctRelative(FCTctx) = true
  | isFctRelative(RELctx(s,c)) = isFctRelative(c)
*)

exceptionx appendContext: unit

fun appendCtxMaybe(FCTctx,ctx as RELctx _) = ctx
  | appendCtxMaybe(RELctx(str,ctx'),ctx) = RELctx(str,appendCtxMaybe(ctx',ctx))
  | appendCtxMaybe(_) = raisex appendContext

fun appendCtx(ctx,TOPctx) = ctx
  | appendCtx(ctx,FCTctx) = ctx
  | appendCtx(FCTctx,ctx as RELctx _) = ctx
  | appendCtx(FCTctx,ctx) = (PrintBasics.printContext ctx;
			     Impossible "appendCtxMaybe: inner FCTctx")
  | appendCtx(RELctx(str,ctx'),ctx) = RELctx(str,appendCtx(ctx',ctx))
  | appendCtx(ctx,_) = ctx  (* more checking? PAR and SIG should not be involved *)

fun getStr(str:Structure, []:spath, ctx:context, ap:Access.path)
    : Structure*Access.path =
      (case str
	of DIRECT{stamp,context,table} =>
	     ((DIRECT{stamp=stamp,context=appendCtxMaybe(context,ctx),
		      table=table}
	       handlex appendContext => str),ap)
	 | FCTAPP{stamp,context,body,env} =>
	     ((FCTAPP{stamp=stamp,context=appendCtxMaybe(context,ctx),
		      body=body,env=env}
	       handlex appendContext => str),ap)
	 | PARAM{pnode,spath,sign} =>
	     (case ctx
	        of RELctx(FCTAPP{env,...},ctx') =>
		     getStr(lookPnode(pnode,env),spath,ctx',ap)
		 | FCTctx => 
		     extendParam(pnode,spath,sign,[],ap)
		 | _ => Impossible "straccess.getstr")
         | _ => Impossible "getstr -- bad Structure arg" )
  | getStr(str, sp as id::rest, ctx, ap) =
      case str
        of DIRECT{table,context,...} =>
	     let val STRvar{binding,access=SLOT(n),...} =
		       lookSTRinTable(table,id)
	      in getStr(binding,rest,appendCtx(context,ctx),n::ap)
	     end
	 | FCTAPP{stamp,context,body,env} =>
	     getStr(body,sp,RELctx(str,appendCtx(context,ctx)),ap)
         | PARAM{pnode,spath,sign} =>
	     (case ctx
	       of RELctx(FCTAPP{env,...},ctx') =>
		    getStr(lookPnode(pnode,env),rest@spath,ctx',ap)
		| FCTctx =>
		    extendParam(pnode,spath,sign,sp,ap)
		 | _ => Impossible "straccess.getstr.2")
         | _ => Impossible "getStr -- bad Structure arg" 
	   (* shouldn't be applied directly to SPEC *)

fun reduceStr(str: Structure, ctx: context): Structure =
    case str
      of DIRECT{stamp,context,table} =>
	   DIRECT{stamp=stamp,context=appendCtx(context,ctx),table=table}
       | FCTAPP{body,env,context,...} =>
	   reduceStr(body,RELctx(str,appendCtx(context,ctx)))
       | PARAM{pnode,spath,...} =>
	   (case ctx
	      of RELctx(FCTAPP{env,...},ctx') =>
		   (case getStr(lookPnode(pnode,env),spath,ctx',[])
		      of (str' as FCTAPP{body,env,context,...},ap') =>
			   reduceStr(body,RELctx(str',context))
		       | (str,_) => str)
               | FCTctx => str  (* context when accessing param inside functor *)
	       | _ => Impossible "reduceStr: PARAM in invalid context")
       | SPEC _ => str  (* is this really needed here? *)

end (* local open ... *)

end (* structure StrAccess *)
