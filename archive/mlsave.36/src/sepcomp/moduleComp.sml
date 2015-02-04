(* Andrew Appel changed this July 3 1989 to fix bug related to profiling;
   in particular, the interface to the profile was changed, so that
   the pair(executable, profiling-info) must be constructed explicitly
   by the caller to transDec.  Rather than build the profiling info,
   I chose to leave it out entirely for now; there is no pair any more,
   just the executable, and separately compiled code is not profilable.
   It is easy enough (later) to make separately compiled code profilable. *)
(* Modules compiler for New Jersey ML.
   Nick Rothwell, LFCS, January 1989. *)

functor ModuleComp(structure Absyn: BAREABSYN
		     sharing Absyn = BareAbsyn	(* HACK: Translate.transDec *)
		   structure Lambda: LAMBDA
	           structure Opt: OPT sharing Opt.L = Lambda
		   structure Machm: CODEGENERATOR
		  ): MODULE_COMPILER =
   struct
      structure Lambda = Lambda
      structure Absyn = Absyn

    (*exception Bug of string
      fun bug x = raise Bug x*)

      val bug = ErrorMsg.impossible

      val DEBUG = false

      type lvar = int
       
      fun assert(msg, cond) =
	 if cond then () else bug("assert: " ^ msg)

      fun pr s = (output std_out s; flush_out std_out)

      fun debug s = if DEBUG then pr s else ()


       abstype DynModule =
	   DYNMODULE of {declarations: ModuleDecl list,
				(* One of these for each declaration. *)
	   		 declLvars: lvar list,
				(* Accumulate the lvars bound for the
				   entire module, head of list being most
				   recent. *)
			 nextSlot: int
				(* Index of next free slot (always same as
				   length(declLvars)). *)
			}
	   withtype ModuleDecl =
	       {slotsUsed: int,	(* How many slots does this declaration fill
	       			   (i.e. how many vars does it bind)? *)
	        firstSlot: int,	(* First slot this decl. fills. *)
		shift: int,	(* How far this Decl has moved under nested
				   import, compared to its original posn. *)
		code: string	(* Code for the closed lambda. *)
	       }
       with
	   val emptyDynModule =
	       DYNMODULE{declarations=[],
	                 declLvars=[],
			 nextSlot=0
			}

	   fun showModuleDecl({slotsUsed, firstSlot, shift, code}
	                         : ModuleDecl
			     ) =
	       "{slotsUsed=" ^ makestring slotsUsed
	       ^ ", firstSlot=" ^ makestring firstSlot
	       ^ ", shift=" ^ makestring shift
	       ^ ", code=[..." ^ makestring(size code)
	       ^ "...]}"

	  (*codegen: basically lifted from Interact.codegen, but I've not
	    bothered with the timers or any of that jazz. *)
	   fun codegen lambda: string =
	       let val _ = debug "codegen..."
		   val code = Machm.generate lambda
		   val _ = debug("done (" ^ makestring (size code) ^ ").\n")
	       in code
	       end
	       
	  (* This is the OLD addDeclaration function. *)
	   fun addDeclaration(lambda, newLvars: lvar list,
	                      DYNMODULE{declarations=oldDecls,
		                        declLvars=oldLvars,
			                nextSlot=oldNextSlot
			               }
			     ): DynModule =
	       let 
		   val _ =
		       if DEBUG then
			   (debug "adding declaration for Lvars";
		            app (fn i => debug(" " ^ makestring i)) newLvars;
			    debug ".\n"
			   )
		       else ()
			       
		   val _ =
		       if DEBUG then
			   (debug "open lambda:\n";
			    MCprint.printLexp lambda;
			    debug "\n"
			   )
		       else ()
			   
		   val closedLambda =
		       (debug "closing lambda...";
		        Opt.closeModDecl(lambda, rev oldLvars,
					 ProcessFile.getCore()
					)
				(* Transform lambda into the form
				   "fn lookup => fn indexer => ...". *)
		        before debug "done.\n"
		       )

		   val _ =
		       if DEBUG then
			   (debug "closed lambda:\n";
			    MCprint.printLexp closedLambda;
			    debug "\n"
			   )
		       else ()
			   
		   val code = codegen closedLambda

		   val thisDecl: ModuleDecl =
		           {slotsUsed=length newLvars,
			    firstSlot=oldNextSlot,
			    shift=0,
			    code=code
			   }

		   val _ =
		       if DEBUG then
			   debug("New ModuleDecl: "
			         ^ showModuleDecl thisDecl ^ ".\n"
				)
		       else ()
	       in
		   DYNMODULE{declarations=thisDecl :: oldDecls,
			     declLvars=(rev newLvars) @ oldLvars,
				(* lvars accumulate on front of list. *)
			     nextSlot=oldNextSlot + length newLvars
			    }
	       end

	  (* This is the NEW addDeclaration function, compatible with the
	     new signature. *)

	   exception AddDeclaration
	   val addDeclaration =
	     fn (dec: Absyn.dec, lvars, lambDynModule) =>
	       let
	         val resultVec = Lambda.RECORD(map Lambda.VAR lvars)
	         val lambda = Translate.transDec dec resultVec
	       in
	         if !ErrorMsg.anyErrors then
		   raise AddDeclaration
		 else
	           addDeclaration(lambda, lvars, lambDynModule)
	       end	

	  (*importDynModule: accumulate the "nested" module into the
	    enclosing one. Inner module declarations must be relocated. *)

	   fun relocateDecls(decls, offset: int) =
	       map (fn {slotsUsed, firstSlot, shift, code} =>
			   {slotsUsed=slotsUsed,
			    firstSlot=firstSlot+offset,
			    shift=shift+offset,
			    code=code
			   }
		   ) decls

	   fun importDynModule(DYNMODULE{declarations=innerDecls,
	                                 declLvars=innerLvars,
					 nextSlot=innerNextSlot
					},
	                       DYNMODULE{declarations=outerDecls,
			                 declLvars=outerLvars,
					 nextSlot=outerNextSlot
					}
			      ) =
	       let 
		   val relocatedInnerDecls =
		       relocateDecls(innerDecls, outerNextSlot)

		   val _ =
		       if DEBUG then
			   (debug "relocated decls:";
			    app (fn d => debug("\n" ^ showModuleDecl d))
				relocatedInnerDecls;
			    debug ".\n"
			   )
		       else ()
	       in
		   DYNMODULE{declarations=relocatedInnerDecls @ outerDecls,
		             declLvars=innerLvars @ outerLvars,
			     nextSlot=innerNextSlot + outerNextSlot
			    }
	       end

	   fun abstractDynModule(
	          DYNMODULE{declarations, declLvars=oldLvars, nextSlot},
		  newLvars
	       ) =
	      (assert("ModuleComp.replaceLvars: different # of lvars",
	              length newLvars = length oldLvars
		     );
	       DYNMODULE{declarations=declarations,
			 declLvars=rev newLvars,
			 nextSlot=nextSlot
			}
	      )

	  (* These for forward compatibility with the new ModuleComp: *)
	   exception CompileDynModule
	   fun compileDynModule opt dm = dm

	   fun executeDynModule (DYNMODULE{declarations, nextSlot=lastSlot, ...})
				(lookup: lvar -> System.Unsafe.object)
               : System.Unsafe.object array =
	       let
		   val destArray =
		      array(lastSlot, NONE: System.Unsafe.object option)

		   fun executeDecl(code, shift) =
		       let
			   val exec: (lvar -> System.Unsafe.object)
			             -> (int -> System.Unsafe.object)
				     -> System.Unsafe.object array =
			       System.Unsafe.boot code

			   fun indexer slot =
			       case destArray sub (slot+shift)
				 of SOME obj => obj
				  | NONE => bug "forward obj reference?"

			   val (indexer, lookup) =	(* DEBUG *)
			       if DEBUG then
				   (fn i =>
				        (debug("\n   indexer("
					       ^ makestring i
					       ^ ")... "
					      );
					 indexer i
					),
				    fn lv =>
				        (debug("\n   lookup("
					       ^ makestring lv
					       ^ ")... "
					      );
					 lookup lv
					)
				   )
			       else (indexer, lookup)

				(* Actual execution: if the decl. makes
				   no bindings, then it returns a null
				   pointer (I think), *not* an array of
				   length 0. *)
			   val result: System.Unsafe.object array =
			       (debug "about to exec...";
			        exec lookup indexer
			       )
		       in
			   debug "done.\n";
			   result
		       end

		   fun copyAcross(_, 0, _) = ()
		     | copyAcross(srcArray, n, destSlotBase) =
		           (debug("copying result[" ^ makestring(n-1)
				  ^ "] to destination ["
				  ^ makestring(destSlotBase + n - 1) ^ "].\n"
				 );
			    update(destArray, destSlotBase + n - 1,
			           SOME(srcArray sub (n-1))
				  );
			    copyAcross(srcArray, n - 1, destSlotBase)
			   )

		   fun runDecl{code, slotsUsed, shift, firstSlot} =
		      copyAcross(executeDecl(code, shift),
			         slotsUsed, firstSlot
				)

		   fun properObjects optionArray: System.Unsafe.object array =
		       let
			   fun proper i =
			       (case optionArray sub i
				  of SOME obj => obj :: proper(i+1)
			           | NONE => bug "unfilled slot?"
			       ) handle Subscript => nil
		       in
			   arrayoflist(proper 0)
		       end
	       in
		   app runDecl (rev declarations);
		   properObjects destArray
	       end
       end

      type LambDynModule = DynModule
      type CodeDynModule = DynModule
   end;
