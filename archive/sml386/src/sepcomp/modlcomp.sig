(* Copyright 1989 by AT&T Bell Laboratories *)
(* Modules compiler for New Jersey ML.   Nick Rothwell, LFCS, January 1989.
   Edited and re-arranged by Andrew Appel, 1990.  *)

signature MODULE_COMPILER =
   sig
       type LambDynModule	(* Dynamic module with lambdas, lvars, etc. *)
       type CodeDynModule	(* Compiled (native code) module - no lvars. *)

       exception MkLambda
       val mkLambda: ErrorMsg.inputSource * Absyn.dec * Access.lvar list 
			-> LambDynModule

       val abstractDynModule: CodeDynModule * Access.lvar list -> LambDynModule
	(* abstractDynModule takes a compiled module (presumably from an
	   "import"), and returns a lambda-based module with the code
	   module treated as an argument. We attach the lvars to the code
	   module, to allow reference from declarations below the
	   "import". *)

       exception CompileDynModule
       val compileDynModule: LambDynModule -> CodeDynModule
		(* Close and fold down all the lambdas, generate code.*)

       val executeDynModule: 
	   (Access.lvar -> System.Unsafe.object) -> 
		CodeDynModule -> System.Unsafe.object array
		(* Execute the module, given the lookup function
		   (which I'll use to get at true globals like the
		   pervasives). *)

       val importDynModule: (LambDynModule * LambDynModule) -> LambDynModule
		(* importDynModule takes a module state, and embeds it
		   into an enclosing module state.  The lvar list of the
		   "inner" module (first argument) is put BEFORE the
		   lvar list of the "outer" module, unlike previous practice.
		*)
   end;
