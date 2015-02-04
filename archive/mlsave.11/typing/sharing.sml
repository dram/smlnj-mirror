(* sharing.sml *)

signature SHARING =
  sig
    structure Basics: BASICS
    val doSharing : Basics.symtable * Basics.strenv * Basics.sharespec -> Basics.sharespec
    val checkSharing : Basics.symtable * Basics.strenv * Basics.sharespec -> unit
  end

structure Sharing : SHARING =
struct

  structure Basics = Basics

  open ErrorMsg Basics EnvAccess

(*  val _ = internals := true   DEBUGGING *)

  (* a couple of useful iterators *)

  fun for a b = app b a

  fun upto n f = 
      let fun loop i = if i>=n then () else (f i; loop(i+1))
       in loop 0
      end


  (* functions for correctly comparing type components when checking type
     sharing.  These should move to typing/typesutil.sml *)

  exception ReduceType

  fun reduceType(CONty(ref(TYPEtyc{params,def,...}), args)) =
      (* replacement for expandTy *)
      let fun subst(v as VARty(tv)) =
		   let fun look ([],[]) = v
			 | look (tv'::params', arg::args') =
			    if eqTyvar(tv,tv') then arg else look(params',args')
			 | look _ = impossible "reduceType: arity mismatch"
		    in look (params,args)
		   end
	    | subst(CONty(reftycon, args')) =
		 CONty(reftycon, map subst args')
	    | subst _ = impossible "reduceType: illegal type"
       in subst def
      end
    | reduceType _ = raise ReduceType

  fun equalTycon(tycs) =
      (* needed to deal with abbreviations *)
      eqTycon(tycs) orelse
      case tycs
	of (TYPEtyc{params,def,...},tyc) =>
	     length params = tyconArity tyc
	     andalso equalType(def,CONty(ref tyc, map VARty params))
	 | (tyc,TYPEtyc{params,def,...}) =>
	     length params = tyconArity tyc
	     andalso equalType(def,CONty(ref tyc, map VARty params))
	 | _ => false

  and equalType(ty,ty') =
      let fun eq(VARty(tv),VARty(tv')) = eqTyvar(tv,tv')
	    | eq(ty as CONty(ref tycon, args), ty' as CONty(ref tycon', args')) =
		eqTycon(tycon, tycon') andalso List2.all2 equalType(args,args') orelse
		(equalType(reduceType ty, ty')
		 handle ReduceType =>
		   (equalType(ty,reduceType ty')
		    handle ReduceType => false))
	    | eq _ = false
       in eq(prune ty, prune ty')
      end


  fun getStr([],str) = str
    | getStr(id::rest,STRstr{table,env={s,...},...}) =
	let val STRvar{binding,...} = lookSTRinTable(table,id)
	 in getStr(rest,(case binding of INDstr i => s sub i | _ => binding))
	end

  fun findStr(id::rest,table,{s,t}) : Structure =
       (let val str = case lookSTRinTable(table,id)
			of STRvar{binding=INDstr i,...} => s sub i
			 | STRvar{binding,...} => binding
	 in getStr(rest,str)
	    handle Table.Notfound_Table =>
	      condemn "bad path is sharing specification"
	end
	handle Table.Notfound_Table =>  (* look for global structure *)
	  let val STRvar{binding,...} = lookSTR(id)
	   in getStr(rest,binding)
	  end
	  handle Table.Notfound_Table =>
	    condemn "bad path is sharing specification")
    | findStr([],_,_) = impossible "Sharing.findStr with empty path"

  fun findTycon(path,table,env as {s,t}) : tycon =
      let fun split ([],p) = impossible "path too short in Sharing.findTycon"
	    | split ([id],p) = (rev p,id)
	    | split (id::rest,p) = split(rest,id::p)
	  val (path,id) = split(path,[])
       in case path
	    of [] => (case !(lookTYCinTable(table,id))
			of INDtyc [i] => t sub i
			 | tyc => tyc)
	     | _ => !(lookTYCinStr(findStr(path,table,env),id))
      end

  fun doSharing(table,env as {s=senv,t=tenv},{s=strShare,t=typeShare}) =
      let val {assoc,getassoc,union,find} = Siblings.new(fixedStamp)
	  val {union=tunion,find=tfind} = Unionfind.new(fixedStamp)

	  exception DONE

	  fun strMerge(p' as STRstr{stamp=p,...}, q' as STRstr{stamp=q,...}) =
	      if (assoc(p,p'); find p) = (assoc(q,q'); find q)
	      then ()
	      else let val pclass = getassoc p
		       and qclass = getassoc q
		    in union(p,q);
		       for pclass (fn x =>
			 for qclass (fn y =>
			   sMerge(x,y)))
		   end

	  and sMerge(str1 as STRstr{stamp=s1,kind=k1,env={s=senv1,t=tenv1,...},...},
		     str2 as STRstr{stamp=s2,kind=k2,env={s=senv2,t=tenv2,...},
			     	    table,...}) =
	      case (k1,k2)
		of (STRkind,STRkind) => if s1 = s2 then () else condemn "sharing 1"
		 | (STRkind,SIGkind _) => sMerge(str2,str1)
		 | (SIGkind{bindings,...},  _ ) =>
		     for bindings 
		       (fn STRbind(STRvar{name,binding,...}) =>
			      let val STRvar{binding=target,...} =
				        lookSTRinTable(table,name)
			       in strMerge((case binding
					      of INDstr i => senv1 sub i 
					       | _ => binding),
					   (case target
					      of INDstr i => senv2 sub i
					       | _ => target))
			      end
			 | TYCbind(ref tycon) =>
			     let val tyc1 = case tycon
					      of INDtyc [i] => tenv1 sub i
					       | _ => tycon
				 val tyc2 = case !(lookTYCinTable(table,tycName tyc1))
					      of INDtyc [i] => tenv2 sub i 
					       | tyc => tyc
			      in tunion(tycStamp tyc1,tycStamp tyc2); ()
			     end
			 | _ => raise DONE)
		     handle DONE => ()

	  fun shareSig {s,t} =
	      (upto (length s) (fn i =>
		 let val STRstr{stamp,sign,table,env,kind} = senv sub i
		  in case kind
		       of STRkind => ()
			| SIGkind _ => 
			    let val stamp' = find stamp
			     in if stamp' = stamp
				then ()
				else update(s,i,
				     	    STRstr{stamp=stamp',sign=sign,table=table,
						   env=env,kind=kind});
				shareSig env
			    end
		 end);
	       upto (length t) (fn i =>
		 let val tycon = t sub i
		     val stamp = tycStamp tycon
		     val stamp' = tfind stamp
		  in if stamp = stamp'
		     then ()
		     else update(t,i,setTycStamp(stamp',tycon))
		 end))

	  val strPathPairs = ref [] : (spath*spath) list ref
	  val typePathPairs = ref [] : (spath*spath) list ref

       in for strShare (fn p as (p1,p2) =>
	    let val str1 as STRstr{stamp=s1,...} = findStr(p1,table,env)
		and str2 as STRstr{stamp=s2,...} = findStr(p2,table,env)
	     in if fixedStamp s1 orelse fixedStamp s2 then ()
		  else strPathPairs := p :: !strPathPairs;
		strMerge(str1,str2)
	    end);
	  for typeShare (fn p as (p1,p2) =>
	    let val s1 = tycStamp(findTycon(p1,table,env))
		and s2 = tycStamp(findTycon(p2,table,env))
	     in if fixedStamp s1 orelse fixedStamp s2 then ()
		  else typePathPairs := p :: !typePathPairs;
	        tunion(s1,s2)
	    end);
	  shareSig env;
	  {s= !strPathPairs, t= !typePathPairs}
      end  (* doSharing *)

  fun checkSharing(table,env,{s=strShare,t=typeShare}) =
      (for strShare (fn p as (p1,p2) =>
	 let val STRstr{stamp=s1,...} = findStr(p1,table,env)
	     and STRstr{stamp=s2,...} = findStr(p2,table,env)
	  in if s1 <> s2
	     then condemn "structure sharing violation"
	     else ()
	 end);
       for typeShare (fn (p1,p2) =>
	 let val tyc1 = findTycon(p1,table,env)
	     and tyc2 = findTycon(p2,table,env)
	  in if equalTycon(tyc1,tyc2)
	     then ()
	     else (PrintType.printTycon tyc1; print "\n";  (* DEBUGGING *)
		   PrintType.printTycon tyc2; print "\n";  (* DEBUGGING *)
		   condemn "type sharing violation")
	 end))

end (* structure Sharing *)
