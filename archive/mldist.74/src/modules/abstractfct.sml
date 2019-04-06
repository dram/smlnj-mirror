(* abstractfct.sml: abstraction of functor body.
   Replace all bound components with abstracted versions *)

(* Assumptions: bound stamps are unique *across* scopes. 
                That way we can use stamp maps which contain
                stamps from more than one scope.
*)

signature ABSTRACTFCT =
  sig
     val abstractBody : Modules.Structure * Modules.Structure * (Stamps.stamp -> bool)
	                * (Stamps.stamp -> bool)
		        -> {tyseq : Types.tycon list,
			    strseq : Modules.Structure list,
			    str : Modules.Structure}
  end


structure AbstractFct : ABSTRACTFCT =
struct

open ErrorMsg Types Modules ModuleUtil Stamps Variables

fun abstractBody (body,param,isBodyStamp : stamp -> bool,
		  isParamStamp : stamp -> bool) =
let 
   val _ = if !System.Control.internals then print "\nabstractBody:\n"
           else ()
   val next = fn i => (!i before inc i)

   exception Member
   val tycCount = ref 0
   val strCount = ref 0

   val tycSeries : tycon list ref = ref nil
   val strSeries : Structure list ref = ref nil

   fun addTyc tyc = (tycSeries := tyc :: !tycSeries;
		     ABSFBtyc(SEQ (next tycCount)))
   fun addStr str = (strSeries := str :: !strSeries;
		     ABSFB_STR(SEQ (next strCount)))

    (* initMap: create mappings from bound stamps to locations in the parameter
        structure *)

    fun initMap (isBound,strStampMap,tycStampMap) =
	let fun scanTypes(types,loc) =
		let fun scan i =
		        (case Array.sub(types,i)
			  of GENtyc{stamp,...} =>
			      if isBound stamp
				 then (Stamps.applyMap(tycStampMap,stamp); ())
				      handle Member =>
					  (Stamps.updateMap tycStampMap (stamp,ABSFBtyc (PARAM (rev (i::loc)))))
			         else ()
			   | _ => ();
			 scan (i+1))
		 in scan 0
		    handle Array.Subscript => ()
		end
 
	    fun scanStr (ERROR_STR,_) = ()
              | scanStr(s as INSTANCE{sign,subStrs,types,...},loc) =
		let val strStamp = getStrStamp s
		    val signStamp = getSignStamp sign
		in if isBound strStamp
		       then (Stamps.updateMap strStampMap	
			      (strStamp,(signStamp,
					 ABSFB_STR(PARAM(rev loc))) ::
					 (Stamps.applyMap(strStampMap,strStamp)
					  handle Member => nil));

(* Scan the instantiation arrays.
   Do not process the instantiation arrays for structures with EMBEDDED
   signatures.  This code will loop if you do, since structures matching
   EMBEDDED signatures are in their own instantiation array.  We're always
   guaranteed to process the structure with a TOP signature that contains
   those instantation arrays also.*)

			      case sign 
				  of SIG {kind=TOP _,...} =>
				     let fun scan i =
					(scanStr(Array.sub(subStrs,i),i::loc);
					scan (i+1))
 			             in scan 0 handle Array.Subscript => ();
					scanTypes(types,loc)
				     end
				   | _ => ())
		       else ()
		 end
	      | scanStr _ = impossible "abstractBody.initMap.scanSubs";
	 in fn s => scanStr(s,[])
	end

   val paramStrMap = Stamps.newMap Member : (stamp * Structure) list Stamps.stampMap
   val paramTycMap = Stamps.newMap Member : tycon Stamps.stampMap
   val _ = initMap(isParamStamp,paramStrMap,paramTycMap) param

   (* apply a function f to all DEFtycs and to all GENtycs with
      parameter or body stamps *)

   fun memoTyc (f : tycon * (tycon -> tycon) * (tycon -> unit) -> tycon) =
       let val tycMap = Stamps.newMap Member : tycon Stamps.stampMap
	   val defTycs : (tycon * tycon) list ref = ref nil
           fun findDefTyc deftyc =
	        let fun find ((tyc,elem) :: r) =
		          if tyc=deftyc then elem else find r
		      | find nil =
			  f(deftyc,rewriteTycon,
			    fn r => defTycs := (deftyc,r) :: !defTycs)
                in find(!defTycs)
                end
           and rewriteTycon tyc =
		  case tyc
		  of DEFtyc _ => findDefTyc tyc
		   | GENtyc{stamp,path=spath,...} =>
			if isParamStamp stamp orelse isBodyStamp stamp then
			  (Stamps.applyMap(tycMap,stamp) handle Member =>
			    f(tyc,rewriteTycon,
			      fn r =>Stamps.updateMap tycMap (stamp,r)))
			else tyc
		    | tyc => tyc
	in rewriteTycon
	end

   (* memoStr:
        If:
	  1. an INSTANCE structure has a parameter origin structure,
	     see if we can find its position (if any) in the
	     actual paramter structure using paramStrMap.  If we can't
	     then go to step 2.
	  2. an INSTANCE structure has:
	           1. an origin which is either a functor body or parameter
		      structure, then apply a function f to it.
		   2. a signature which is a functor body signature, then
		      apply a function f to it.
		   3. otherwise leave it unchanged
	   3. a SIMPLE structure was:
	           1. from the functor body, apply a function f to it
		   2. otherwise leave it unchanged.
       Memoize the application of f so that we apply it only once to
       any argument. *)
	      
   fun memoStr (f : Structure * (Structure -> Structure)
		    * (Structure -> unit) -> Structure) =
       let val strMap = Stamps.newMap Member : (stamp * Structure) list Stamps.stampMap
	   val simpleStrMap = Stamps.newMap Member : Structure Stamps.stampMap
          fun memo (str as (INSTANCE{sign,...})) =
              let val strStamp = getStrStamp str
	          val signStamp = getSignStamp sign
		  exception NotFound
                  fun find ((stamp,str)::r) =
		         if signStamp=stamp then str
		         else find r
		    | find nil = raise NotFound
		  fun getEntry () = 
		      Stamps.applyMap(strMap,strStamp)
		      handle Member => nil
		  fun add () =
		    find (getEntry())
		    handle NotFound =>
		       f(str,memo,fn result =>
			 Stamps.updateMap strMap
			            (strStamp,(signStamp,result)::getEntry()))
              in if isParamStamp strStamp then
		      find (Stamps.applyMap(paramStrMap,strStamp))
		      handle Member => impossible "AbstractFct.memo 2"
			   | NotFound => add()
		 else if isBodyStamp strStamp orelse isBodyStamp signStamp then
		       add()
		 else str (* structure doesn't change across functor apps *)
	      end
            | memo (str as (SIMPLE{stamp,...})) =
	         if isBodyStamp stamp then 
	            Stamps.applyMap(simpleStrMap,stamp)
		    handle Member =>
		        f(str,memo,fn result =>
		                 Stamps.updateMap simpleStrMap (stamp,result))
		 else str
	   | memo str = str
      in memo
      end

   fun rewriteType (ty,memo) =
       let fun f ty =
           case ty
	   of VARty(ref(INSTANTIATED ty')) => f ty'
	    | FLEXRECORDty(ref(CLOSED ty')) => f ty'
	    | POLYty{tyfun=TYFUN{body,arity},sign,abs} =>
	             POLYty{tyfun=TYFUN{body=f body,arity=arity},


			    sign=sign,abs=abs}
	    | VARty _ => ty
	    | CONty(tyc,args) => CONty(memo tyc,
				       map f args)
	    | ERRORty => ty
	    | _ => raise Cascade "abstractBody.abstractType 2"
       in f ty
       end

   fun tyconSeries1 (tyc,memo,add) =
     case tyc
     of DEFtyc {path,strict,tyfun as (TYFUN{arity,body})} =>
	let val r =
	    let val body' = rewriteType(body,memo)
            in if body<>body' then 
                  addTyc(DEFtyc{path=path,
				strict=strict,
				tyfun=TYFUN{arity=arity,body=body'}})
                else tyc
            end
        in add r;
	   r
	end
     | GENtyc{stamp,kind,...} =>
          if isParamStamp stamp then
	     Stamps.applyMap(paramTycMap,stamp)
	     handle Member =>
		 ErrorMsg.impossible "AbstractFct.tyconSeries 1: param tycon not found"
          else if isBodyStamp stamp then
	    let val r = addTyc tyc
	    in add r;
	       case kind
	       of ref (DATAtyc dcons) =>
	         kind := DATAtyc
		        (map (fn DATACON {name,const,typ,rep,sign} =>
		               DATACON{name=name,const=const,rep=rep,
				       sign=sign,typ=rewriteType(typ,memo)})
			 dcons)
                | _ => ();
               r
            end
          else tyc
      | tyc => tyc

    val tyconSeries = memoTyc tyconSeries1

   fun listofarray a =
      let fun f i = if i<Array.length a then Array.sub(a,i) :: f (i+1) else nil
      in f 0
      end

   val bogusTycSym = Symbol.tycSymbol "?.bogus"
   val bogusName = [bogusTycSym]

   fun relativizeSimpleEnv(env : Modules.env) =
     let val extratycs = ref (nil : tycon list)
         val extrastrs = ref (nil : Structure list)
	 val tyccount = ref 0
         val strcount = ref 0
         fun addtyc(tyc,memo,add) =
	      let val r = RELtyc {name=bogusName,pos=([],next tyccount)}
	      in extratycs := tyc :: !extratycs;
		 add r;
		 r
	      end
         fun addstr str =
	     (extrastrs := str :: !extrastrs;
	      FORMAL{pos=next strcount,spec=ERROR_SIG})
         val tyconPosition = memoTyc addtyc
         val relativeType = fn ty => rewriteType(ty,tyconPosition)
         fun abstractDcon(DATACON{name,const,typ,rep,sign}) =
	                DATACON{name=name,const=const,rep=rep,sign=sign,
		                typ=relativeType typ}
         fun relativizeBinding binding =
	     case binding
	     of VARbind(VALvar{name,access,typ}) =>
		    VARbind(VALvar{name=name,access=access,
				   typ=ref(relativeType (!typ))})
	      | CONbind dcon => CONbind(abstractDcon dcon)
	      | TYCbind tyc =>
	         (case tyconPosition tyc
		  of RELtyc {pos=([],count),...} =>
				  TYCbind (FORMtyc{pos=count,spec=ERRORtyc,
						   name=bogusTycSym})
                   | tyc' => TYCbind tyc')
	      | STRbind(STRvar{name,access,binding}) =>
                    STRbind(STRvar{name=name,access=access,
				   binding=addstr binding})
	      | _ => binding
         val env' = Env.map relativizeBinding env
         val newSubStrs = Array.arrayoflist (rev (!extrastrs))
	 val newTypes = Array.arrayoflist (rev (!extratycs))
     in (env',newSubStrs,newTypes)
     end

   fun relativizeSignEnv(env : Modules.env,types : tycon array) =
     let val extratycs = ref (nil : tycon list)
	 val tyccount = ref (Array.length types)
         fun addtyc(tyc,memo,add) =
	      let val r = RELtyc {name=bogusName,pos=([],next tyccount)}
	      in extratycs := tyc :: !extratycs;
		 add r;
		 r
	      end
         val tyconPosition = memoTyc addtyc
         val relativeType = fn ty => rewriteType(ty,tyconPosition)
         fun abstractDcon(DATACON{name,const,typ,rep,sign}) =
	                DATACON{name=name,const=const,rep=rep,sign=sign,
		                typ=relativeType typ}
         fun relativizeBinding binding =
	     case binding
	     of VARbind(VALvar{name,access,typ}) =>
		    VARbind(VALvar{name=name,access=access,
				   typ=ref(relativeType (!typ))})
	      | CONbind dcon => CONbind(abstractDcon dcon)
	      | _ => binding
         val env' = Env.map relativizeBinding env
	 val newTypes = 
	       case !extratycs
	       of nil => types
		| l => Array.arrayoflist(listofarray types @ rev l)
    in (env',newTypes)
     end

    fun relativizeSign (arg as (SIG{symbols,env,kind,stamp,...},types)) =
      if isBodyStamp stamp then
        let val (env',types') = relativizeSignEnv(env,types)
	in (SIG{symbols=symbols,kind=kind,env=env',stamp=Stamps.newFree(),
		path=NONE},  (* anonymous *)
	    types')
        end
      else arg
      | relativizeSign arg = arg

   fun abstractStr1(str as (INSTANCE{types,subStrs,origin,path,sign}),
		    memo,add) =

	  (* create an abstracted instance of the structure.
	     We can overwrite the instantiation arrays because they
	     were created during the parsing of the functor body *)

       let val origin' = case origin
		         of SELF s => SELF Stamps.null
			  | _ => memo origin
	   val (sign',types') =  relativizeSign(sign,types)
	   val result = addStr(INSTANCE{types=types',subStrs=subStrs,
			      path=path,origin=origin',sign=sign'})

       in add result; ArrayExt.remap(tyconSeries,types');
	  ArrayExt.remap(memo,subStrs); result
       end
      | abstractStr1 (str as (SIMPLE{stamp,env,path}),memo,add) =
           let val (env,subStrs,types) = relativizeSimpleEnv env
	       val symbols = map #1 (ModuleUtil.sortEnvBindings env)
	       val result =
		   addStr(INSTANCE{sign=SIG {env=env,symbols=symbols,
					     path=NONE, (* anonymous *)
				       stamp=Stamps.newGenStamp Stamps.freeScope (),
				       kind=TOP{strcount=Array.length subStrs,
					        typecount=Array.length types,
						slotcount=0,
						sConstraints=nil,
						tConstraints=nil}},
			      path=path,
			      subStrs=subStrs,
			      types=types,
			      origin=SELF Stamps.null})
           in add result; ArrayExt.remap(tyconSeries,types);
	       ArrayExt.remap(memo,subStrs); result
	   end
      | abstractStr1(str,_,_) = str

   val abstractStr =  memoStr abstractStr1
   val str = abstractStr body
in {tyseq=rev(!tycSeries),strseq=rev(!strSeries),str=str}
end
end
