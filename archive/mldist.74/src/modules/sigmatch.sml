structure SigMatch : SIGMATCH =
struct
  open Modules ModuleUtil Types TypesUtil Variables ErrorMsg Access
       Instantiate

  (* checkSharing: check sharing constraints.  Takes a structure
     environment, a list of structure sharing constraints, a list
     of type sharing constraints, an error function, and a symbolic
     path as arguments.  The symbolic path is the path to this
     structure: it is prefixed to symbolic paths within this
     structure when error messages are printed.*)

  fun checkSharing(str : Structure, fullEnv : Modules.env,
		   sConstraints:{internal:spath list,
				 external: Structure option} list,
		   tConstraints:{internal:spath list,
				 external: Types.tycon option} list,
		    err) =
    let fun f (lookup : spath ->'a,printName : 'a -> unit,
	       eq : 'a * 'a -> bool,errName:string)
              ({internal:spath list,external : 'a option}) =
         let fun g first =
	     let fun check x =
		 if eq(lookup x,first)
		    then ()
		    else (err COMPLAIN (errName^" sharing violation");
		          print "  "; printName first;
			  print " # ";PrintUtil. prSymPath x;
			  PrintUtil.newline())
	     in app check internal
	     end
	 in case (internal,external)
	    of (l,SOME i) => g i
	     | (h::t,NONE) => g (lookup h)
             | (nil,NONE) => ()
        end
       val checkStructure =f (fn x => (fn STRvar{binding,...} => binding)
			              (lookBindingSTR(str,x)),
			      fn x=>PrintBasics.printStructureName(fullEnv,x),
			      eqOrigin,"structure")
       val checkType = f (fn x => lookBindingTYC(str,x),
			  PrintType.printTycon std_out fullEnv, 
			  equalTycon,"type")
    in app checkStructure sConstraints;
       app checkType tConstraints
    end

  fun compareTypes (env:Modules.env,err) (spec: ty, actual:ty,name) : unit =
    if TypesUtil.compareTypes{spec=spec,actual=actual} then ()
    else (err COMPLAIN "value type in structure doesn't match signature spec";
	  PrintType.resetPrintType();
	  print ("  name: " ^ Symbol.name name ^ "\n  spec:   ");
	  PrintType.printType std_out env spec;
	  print "\n  actual: ";
	  PrintType.printType std_out env actual; print "\n")
      
  fun conforming(INSTANCE{sign=SIG{stamp,kind=TOP _,...},...},
      		 SIG{stamp=stamp',kind=TOP _,...}) = stamp=stamp'
    | conforming _ = false

  fun match1 _ {str=ERROR_STR,...} = (ERROR_STR,NONE)
    | match1 (context: (Structure Array.array * tycon Array.array) option) 
              {sign as SIG{symbols,env,kind,...},
	       str,spath,scope,err,printEnv,abstract,self}
	            : Structure * thinning =
      if conforming(str,sign)
      then ((if abstract then instantiate (sign, spath, scope,err)
                    else str(* Could copy with new spath. *)),
            NONE)
      else
      let val v = Access.mkLvar() (* local lvar for accessing str *)
	  val fullEnv = Env.atop(makeEnv(str,[v]),printEnv)
	       (* used for printing error msgs *)
	  val newContext as (subStrs,types) = 
		case (context,kind)
		  of (_, TOP{strcount,typecount,...}) =>
		     (Array.array(strcount,ERROR_STR),
		      Array.array(typecount,ERRORtyc))
		   | (SOME st, EMBEDDED) => st
		   | _ => impossible "Sigmatch.match1"
	  val newstr = INSTANCE{sign=sign,
				subStrs=subStrs,
				types=types,
				path=spath,
				origin =
				   if self then SELF(Stamps.newStamp scope ())
				   else ModuleUtil.getOrigin str}

          val complain = err COMPLAIN
          val compare = compareTypes(fullEnv,err)
          val transType' = ModuleUtil.transType newContext

          (* findComponent: Given a binding specification, find the actual
             binding in a structure.  If the binding is not there, print an
             error message and raise the exception Syntax.

             We must handle exception bindings specially, since they are
             in the name space for constructors.  When we search for an
             actual exception binding, we may find a constructor binding
             instead.  For bindings in other namespaces, we will never
             accidentally find bindings in other name spaces. *)

          (* declare type *)

          val findComponent =
            let val complainMissing =
		  fn (UnboundComponent _,name,namespace) =>
                      (complain("unmatched "^namespace^" spec: "^
				Symbol.name name);
                       raise Syntax)
                   | (ErrorStructure,_,_)  => raise Syntax
                   | (exn,_,_) => raise exn
                val isExn = fn DATACON {rep,...} =>
		              case rep
			      of VARIABLE _ => true
			       | VARIABLEc _ => true
			       | _ => false
            in fn (CONbind(spec as (DATACON{rep,name,...}))) =>
                  ((case lookBinding(str,[name],[v])
		    of binding as (CONbind actual) =>
			  if isExn spec=isExn actual then binding
			  else raise UnboundComponent []
		     | _ => raise UnboundComponent [])
		   handle exn => 
		       complainMissing(exn,name,
				       if isExn spec
                                           then "exception"
                                           else "data constructor"))
	        (* we never check infix bindings; we can return anything
		    here *)
		| (spec as FIXbind _) => spec
                | spec =>
                  let val (name,namespace) =
                     case spec
                     of (STRbind(STRvar{name=id,...})) =>
			  (id,"structure")
                      | (TYCbind(FORMtyc{spec=sigTycon,...})) =>
                                (tycName sigTycon,"type")
                      | (CONbind(DATACON{name,...})) =>
                                (name,"data constructor")
                      | (VARbind(VALvar{name=[id],...})) => (id,"val")
		      | _ => impossible "SigMatch.findComponent"
                  in lookBinding (str,[name],[v])
		     handle exn => complainMissing(exn,name,namespace)
		  end
             end

	  fun checkDataconSign (name,DATACON{sign=s1,...}::_,
				    DATACON{sign=s2,...}::_)=
		  if s1=s2 then ()
		  else err COMPLAIN ("The datatype "^Symbol.name name^
				     " has different representations in the\n\
				     \signature and the structure.  \
			  \Change the definition of the datatype in the\n\
			  \signature to be more explicit about the \
			  \types of the constructors.")
             | checkDataconSign _ = impossible "SigMatch.checkDataconSign"

         fun checkTycBinding(specTycon,strTycon) =
	  let val name = fn () => Symbol.name(tycName specTycon)
          in case specTycon
             of GENtyc {stamp=s,arity,kind,eq=ref eq,...} =>
		    (if arity <> tyconArity strTycon
		     then complain ("tycon arity for "^name()^" does not match specified arity")
		     else case (!kind, strTycon)
			of (DATAtyc dcons,
			    GENtyc{arity=a',kind=ref (DATAtyc dc'),...})=>
			     if length dcons = length dc'
			     then checkDataconSign(tycName specTycon,
						   dcons,dc')
			     else complain ("datatype "^name()^
					    " does not match specification")
			 | (DATAtyc _, _) => 
			      complain ("type "^name()^" must be a datatype")
			 | (FORMtyck, _) =>
			     if eq=YES andalso not(EqTypes.isEqTycon strTycon)
				  then complain
				      ("type "^name()^
				       " must be an equality type")
				  else ()
                         | _ => impossible "checkTycBinding 1")
              | ERRORtyc => raise Syntax
              | _ => ErrorMsg.impossible "checkTycBinding 2"
           end

          (* checkSpec:  Check that a binding specification is matched by an
             actual binding in a structure.  Fill in the instantiation vectors
             for types and structures.*)

	  fun checkSpec spec =
	      case(spec,findComponent spec) 
		of (STRbind(STRvar{name=id,binding=FORMAL{pos,spec,...},...}),
		    STRbind(STRvar{access,binding=str',...})) =>
		      let val (str,thin) = match1 (SOME newContext)
			    {sign=spec,
			     str=str',
			     spath = id :: spath,
			     scope = scope,
			     err=err,
			     printEnv = printEnv,
			     abstract=false,
			     self=false}
		      in Array.update(subStrs,pos,str);
			 [case thin
			    of NONE => VALtrans access
			     | SOME(v,transl) => THINtrans(access,v,transl)]
		     end
		 | (TYCbind(FORMtyc{pos,spec=sigTycon,...}),
		    TYCbind(strTycon)) =>
		        (checkTycBinding(sigTycon,strTycon);
			 Array.update(types,pos,strTycon);
			 nil)
		 | (CONbind(DATACON{name,typ,const,...}),
		    CONbind(DATACON{typ=typ',rep,...})) =>
			(compare(transType' typ,typ',name);
                         case rep
                         of VARIABLE access => [VALtrans access]
                          | VARIABLEc access => [VALtrans access]
                          | _ => nil)
		 | (VARbind(VALvar{name=[name],typ,...}),a) =>
		    (case a
		       of VARbind(VALvar{access,typ=typ',...}) =>

			    (* no propagation of INLINE access!! *)

			   (compare(transType' (!typ),!typ',name);
			    [VALtrans access])
			| CONbind(dcon as DATACON{typ=typ',...}) =>
			    (compare(transType' (!typ),typ',name);
			     [CONtrans dcon])
			| _ => impossible "sigmatch.sml: 122")
		 | (FIXbind _,_) => nil (* nonchecked binding *)
                 | _ => impossible "sigmatch.sml: 124"

	  fun checkList (a::rest) =
		(checkSpec (Env.look(env,a)) handle Syntax => nil) @ checkList rest
	    | checkList nil = nil

	  val trans = checkList symbols
	  val _ = case kind
	          of TOP{sConstraints,tConstraints,...} =>
		        checkSharing(newstr,fullEnv,sConstraints,tConstraints,err)
		   | EMBED => ()
	  val str = if abstract then instantiate(sign,spath,scope,err)
                           else newstr
       in (str, SOME(v,trans))
      end
    | match1 _ _ = (ERROR_STR,NONE)

      val match = match1 NONE

end  (* structure SigMatch *)
