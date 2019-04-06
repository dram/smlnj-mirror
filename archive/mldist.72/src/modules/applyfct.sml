signature APPLYFUNCTOR =
    sig
       val applyFunctor : Modules.Functor * Modules.Structure *
	                  Stamps.scope * Modules.spath *
			  ErrorMsg.complainer * Modules.env ->
			        Modules.Structure * Modules.thinning
    end
	                 
structure ApplyFunctor : APPLYFUNCTOR =
struct
  open Modules Types Stamps ModuleUtil ErrorMsg
  fun applyFunctor
     (FCT{paramName: Symbol.symbol,
	  argument: Signature,
	  body={tyseq,strseq,str}},
          arg: Structure,
          scope : scope,
          path: spath,
          err: complainer,
          parseEnv: env)
     : Structure * thinning =
    let	val makeStamp = Stamps.newStamp scope
	val (argstr,thin) = SigMatch.match {sign=argument,
					    str=arg,
					    spath=getStrPath arg,
					    scope=scope,
					    err=err,
					    printEnv=parseEnv,
					    abstract=false,
					    self=false}
        val tyinst = Array.arrayoflist tyseq
	fun insttyc (ABSFBtyc (SEQ j)) =
	     (Array.sub(tyinst,j) handle Array.Subscript =>
	                                        (print "offset=";
						 print j;
						 print "\n";
						 impossible "ApplyFct.insttyc"))
	  | insttyc (ABSFBtyc (PARAM pos)) = transPosTycon argstr pos
	  | insttyc tyc = tyc
	and insttype ty =
	    case ty
	      of CONty(tycon,args) => CONty(insttyc tycon, map insttype args)
	       | POLYty{sign,tyfun=TYFUN{arity,body},abs} =>
	           POLYty{sign=sign,abs=abs,
		  	 tyfun=TYFUN{arity=arity,body=insttype body}}
	       | _ => ty
        fun insttyc' (GENtyc{stamp,arity,eq,path=path',kind}) =
	                let val kind = case !kind
				       of d as DATAtyc _ =>ref d
					| _ => kind
			in GENtyc{stamp = makeStamp(),
				  eq = eq,
				  arity = arity,
				  path = path'@path,
				  kind = kind}
                        end
	  | insttyc' (DEFtyc{path=path',tyfun=TYFUN{arity,body}}) =
		        DEFtyc{path=path'@path,
			       tyfun=TYFUN{arity=arity,body=insttype body}}
	  | insttyc' _ = impossible "applyFunctor.insttyc"
	fun redefineCon (GENtyc{stamp,kind=r as ref(DATAtyc dcons),...}) =
 	       r := DATAtyc(map (fn DATACON{name,const,typ,rep,sign} =>
	  	 	            DATACON{name=name,const=const,
					    rep=rep,sign=sign,
				            typ=insttype typ}) dcons)
	  | redefineCon str = ()
        val _ = ArrayExt.remap(insttyc',tyinst)
        val _ = ArrayExt.app(redefineCon,tyinst)
	val strinst = Array.arrayoflist strseq
        val tyarrays : (tycon array * tycon array) list ref = ref nil
        val strarrays : (Structure array * Structure array) list ref = ref nil
        fun inststr (ABSFB_STR (SEQ pos)) = 
	     (Array.sub(strinst,pos)
	      handle Array.Subscript =>
		  impossible (implode ["ApplyFct.inststr, pos = ",
			               makestring pos]))
          | inststr (ABSFB_STR (PARAM pos)) = transPosStr argstr pos
	  | inststr str = str

        fun inststr' (INSTANCE{sign,types,subStrs,origin,path=p'}) =
	     let fun find (elem,a) =
		    let fun f ((key,data) :: t) =
			     if elem=key then data else f t
		          | f nil =
			     let val r=ArrayExt.copy elem
			     in a := (elem,r) :: (!a);
			        r
			     end
		    in f (!a)
		    end
                  val types' = find(types,tyarrays)
		  val subStrs' = find(subStrs,strarrays)
	          val origin' =
		      case origin
		      of SELF _ => SELF (makeStamp())
		       | _ => ModuleUtil.getOrigin(inststr origin)
             in INSTANCE{sign=sign,subStrs=subStrs',
			 types=types',origin=origin',path=p'@path}
             end
          | inststr' _ = impossible "ApplyFunctor.inststr'"

       val _ = ArrayExt.remap(inststr',strinst)
       val _ = app (fn (_,r) => ArrayExt.remap(insttyc,r)) (!tyarrays)
       val _ = app (fn (_,r) => ArrayExt.remap(inststr,r)) (!strarrays)
  in (inststr str,thin)
  end
 | applyFunctor _ = (ERROR_STR,NONE)

end (* structure ApplyFunctor *)
