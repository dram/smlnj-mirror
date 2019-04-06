(* sharing.sml *)

signature SHARING =
  sig
    structure Basics: BASICS
    val doSharing : Basics.symtable * Basics.strenv * Basics.sharespec
		    -> Basics.sharespec
    val checkSharing : Basics.symtable * Basics.strenv * Basics.sharespec -> unit
  end

structure Sharing : SHARING =
struct

  structure Basics = Basics

  open ErrorMsg PrintUtil Basics EnvAccess TypesUtil

  (* a couple of useful iterators *)

  fun for a b = app b a

  fun upto (start,finish) f = 
      let fun loop i = if i>=finish then () else (f i; loop(i+1))
       in loop start
      end

  fun getStr([],str) = str
    | getStr(id::rest,STRstr{table,env={s,...},...}) =
	let val STRvar{binding,...} = lookSTRinTable(table,id)
	 in getStr(rest,(case binding of INDstr i => s sub i | _ => binding))
	end
	handle Env.UnboundTable =>
	  condemn("unbound structure id in sharing specification: "
		  ^ Symbol.name id)

  fun findStr(id::rest,table,{s,t}) : Structure =
       (let val str = case lookSTRinTable(table,id)
			of STRvar{binding=INDstr i,...} => s sub i
			 | STRvar{binding,...} => binding
	 in getStr(rest,str)
	end
	handle Env.UnboundTable =>  (* look for global structure *)
	  let val STRvar{binding,...} =
		    lookSTR(id) handle Env.Unbound =>
		      condemn("unbound structure id in sharing specification: "
		              ^ Symbol.name id)
	   in getStr(rest,binding)
	  end)
    | findStr([],_,_) = impossible "Sharing.findStr with empty path"

  fun findTycon(path,table,env as {s,t}) : tycon =
      let val (id::rpath) = rev path 
       in case rev rpath
	    of [] => ((case !(lookTYCinTable(table,id))
			 of INDtyc [i] => t sub i
			  | tyc => tyc)
		      handle Env.UnboundTable =>
		      !(lookTYC id) 
		      handle Env.Unbound =>
		      condemn("unbound type in sharing spec: "^ Symbol.name id))
	     | path' => !(lookTYCinStr(findStr(path',table,env),id))
      end

  fun doSharing(table,env as {s=senv,t=tenv},{s=strShare,t=typeShare}) =
      let val {assoc,getassoc,union,find} =
	      Siblings.new(Stamp.fixed): Structure Siblings.siblingClass
	  val {union=tunion,find=tfind} = Unionfind.new(Stamp.fixed)

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
		of (STRkind _, STRkind _) =>
		      if s1 = s2
		      then ()
		      else condemn "sharing constraint - \
				   \incompatible fixed structures"
		 | (STRkind _, SIGkind _) => sMerge(str2,str1)
		 | (SIGkind{bindings,...},  _) =>
		     for bindings 
		       (fn STRbind(STRvar{name,binding,...}) =>
			    (let val STRvar{binding=target,...} =
				       lookSTRinTable(table,name)
			      in strMerge((case binding
					     of INDstr i => senv1 sub i 
					      | _ => binding),
					  (case target
					     of INDstr i => senv2 sub i
					      | _ => target))
			     end
			     handle Env.UnboundTable => ())
			 | TYCbind(ref tycon) =>
			    (let val tyc1 = case tycon
					      of INDtyc [i] => tenv1 sub i
					       | _ => tycon
			         val tyc2 =
				       case !(lookTYCinTable(table,tycName tyc1))
					 of INDtyc [i] => tenv2 sub i 
					  | tyc => tyc
			      in tunion(tycStamp tyc1,tycStamp tyc2); ()
			     end
			     handle Env.UnboundTable => ())
			 | _ => raise DONE)
		     handle DONE => ()

	  fun shareSig {s,t} =
	      (upto (1, Array.length s) (fn i =>
		 let val STRstr{stamp,sign,table,env,kind} = s sub i
		  in case kind
		       of STRkind _ => ()
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
	       upto (0,Array.length t) (fn i =>
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
	     in if Stamp.fixed s1 orelse Stamp.fixed s2 then ()
		  else strPathPairs := p :: !strPathPairs;
		strMerge(str1,str2)
	    end);
	  for typeShare (fn p as (p1,p2) =>
	    let val s1 = tycStamp(findTycon(p1,table,env))
		and s2 = tycStamp(findTycon(p2,table,env))
	     in if Stamp.fixed s1 orelse Stamp.fixed s2 then ()
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
