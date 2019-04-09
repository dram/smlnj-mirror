(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compile.sml *)

local
  exception Compile of string
in

functor CompileF(structure Machm : CODEGENERATOR
		 exception SilentException
                 structure SCS : sig type staticEnv
		                     val SC : StaticEnv.staticEnv->staticEnv
		                     val unSC : staticEnv->StaticEnv.staticEnv
				 end
	         structure Pickles : 
                         sig type pickle 
                             type hash
                             val pickUnpick : SCS.staticEnv * StaticEnv.staticEnv ->
                                  {hash: hash, pickle: pickle,
				   exportLvars: Lambda.lvar list,
				   exportPid: PersStamps.persstamp option,
				   newenv: StaticEnv.staticEnv}
		         end
		 val mkMkStamp : unit -> unit -> Stamps.stamp
                ) : COMPILE0 =
struct

local structure P = FrontEnd
      structure EM = ErrorMsg
      structure SE = StaticEnv
      structure EU = ElabUtil
in

type lvar = LambdaVar.lvar
type absyn = Absyn.dec
type lambda = Lambda.lexp
type pid = PersStamps.persstamp
type obj = Unsafe.Object.object
type csegments = { c0: Word8Vector.vector, cn: Word8Vector.vector list , name : string option ref}
type compInfo = EU.compInfo

structure SCS= SCS
type pickle = Pickles.pickle
type hash = Pickles.hash

val debugging = ref false
fun bug s = ErrorMsg.impossible ("Compile:" ^ s)
val say = Control.Print.say

fun debugmsg msg =
  if !debugging then (say msg; say "\n"; Control.Print.flush()) else ()

val architecture = Machm.architecture

exception Compile = Compile

fun fail s = raise (Compile s)

val parsePhase = Stats.makePhase "Compiler 010 Parse"

fun parse source =
  let val parser = P.parse source
      val _ = (CheckLty.fname_ref := #fileOpened(source))

      fun loop asts = 
	case parser()
         of P.EOF => Ast.SeqDec(rev asts)
	  | P.ABORT => fail "syntax error"
	  | P.ERROR => fail "syntax error"
	  | P.PARSE ast => loop(ast::asts)

   in loop nil
  end

val parse = Stats.doPhase parsePhase parse

fun parseOne (source: Source.inputSource) =
  let val parser = P.parse source
      val parser = Stats.doPhase parsePhase parser (* for correct timing *)
   in fn () =>
        case parser ()
	 of P.EOF => NONE
	  | P.ABORT => fail "syntax error"
	  | P.ERROR => fail "syntax error"
	  | P.PARSE ast => SOME ast
  end

fun showPid pid = (say(PersStamps.toHex pid); say "\n")

fun mkCompInfo(source: Source.inputSource,
	       coreEnv: StaticEnv.staticEnv,
	       transform : Absyn.dec -> Absyn.dec) : ElabUtil.compInfo =
    let val {error,errorMatch,anyErrors} = ErrorMsg.errors source
        val _ = LambdaVar.clear()
     in {mkStamp = mkMkStamp(),
	   mkLvar = (fn NONE => LambdaVar.mkLvar ()
	              | SOME sym => LambdaVar.namedLvar sym),
	   error = error,
	   errorMatch = errorMatch,
	   anyErrors = anyErrors,
	   coreEnv = coreEnv,
	   transform = transform,
           sourceName = #fileOpened source}
    end

fun anyErrors({anyErrors=ref b,...}:compInfo) = b

val pickUnpick = 
  Stats.doPhase(Stats.makePhase "Compiler 036 pickUnpick") Pickles.pickUnpick

fun elaborate {ast, compenv, compInfo as {anyErrors,...}: EU.compInfo} =
  let val compenv' = SCS.unSC compenv
      val (absyn,newenv) = ElabTop.elabTop(ast,compenv',compInfo)
      val _ = debugmsg "--elaborate: elabTop done!"
      val (absyn,newenv) = 
             if !anyErrors then (Absyn.SEQdec nil, StaticEnv.empty)
	     else (absyn, newenv)

      val _ = debugmsg "--elaborate: calling pickleEnv"
    
      val {hash,pickle,exportLvars,exportPid,newenv} = pickUnpick(compenv,newenv)
(*      val _ = debugmsg ("--elaborate: Pickled: |pickle| = " 
		        ^Int.toString(Word8Vector.length pickle)) *)

   in (* app showPid exportPids; *)
      {absyn=absyn, newenv=SCS.SC newenv,
       exportPid=exportPid, exportLvars=exportLvars,
       staticPid = hash, pickle=pickle}
  end

val elaborate = 
  Stats.doPhase(Stats.makePhase "Compiler 030 Elaborate") elaborate

fun makePid (context, se) = 
  let val bare = CoerceEnv.es2bs
   in #hash (PickMod.pickleEnv (context, bare se))
  end

fun instrument{source,compenv, compInfo as {coreEnv,...}: EU.compInfo} = 
      SProf.instrumDec (coreEnv, compInfo) source 
      o TProf.instrumDec (coreEnv, compInfo)

fun translate{absyn,exportLvars,exportPid : pid option,
              newstatenv,oldstatenv,compInfo} =
  (*** statenv used for printing Absyn in messages ***)
  let val statenv = StaticEnv.atop (SCS.unSC newstatenv, SCS.unSC oldstatenv)
      val {genLambda,importPids} = 
	    Translate.transDec(absyn,exportLvars,statenv,compInfo)
   in {genLambda=genLambda, imports=importPids} 
  end

val translate = 
  Stats.doPhase (Stats.makePhase "Compiler 040 Translate") translate 

fun symDelta (NONE, _) = SymbolicEnv.empty
  | symDelta (_, NONE) = SymbolicEnv.empty
  | symDelta (SOME pid, SOME l) = SymbolicEnv.singleton (pid, l)

fun codeopt lambda = 
  let fun prLexp (s,le) = 
        let val outS = TextIO.openAppend ((!CheckLty.fname_ref)^s);
	    val saveOut = !Control.Print.out
         in Control.Print.out := {
		    say = fn s => TextIO.output(outS,s),
		    flush = fn () => TextIO.flushOut outS
		  };
            MCprint.printLexp (le);
	    TextIO.closeOut outS;
	    Control.Print.out := saveOut
        end

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After Translation ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

      val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 1) then 
                  (prLexp(".log1",lambda); bug "lambda typing errors1 !")
                else ())
              else ()

   val lconLexp = 
     Stats.doPhase(Stats.makePhase "Compiler 052 lcontract") LContract.lcontract

   val lambda = if !Control.CG.specialize then lconLexp lambda else lambda

    val _ = if (!Control.CG.printLambda) andalso (!Control.CG.specialize)
              then (say "\n\n[After LContract ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

   val specLexp = 
     Stats.doPhase(Stats.makePhase "Compiler 053 specLexp") Specialize.specLexp

   val lambda = if !Control.CG.specialize then specLexp lambda else lambda

    val _ = if (!Control.CG.printLambda) andalso (!Control.CG.specialize)
              then (say "\n\n[After Specialization ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

      val _ = if (!Control.CG.checklty1) andalso (!Control.CG.specialize)
              then
               (if CheckLty.checkLty(lambda, 11) then 
                  (prLexp(".log2",lambda); bug "lambda typing errors2 !")
                else ())
              else ()


      val wrapLexp = 
       Stats.doPhase(Stats.makePhase "Compiler 054 wrapLexp") Wrapping.wrapLexp

      val lambda = wrapLexp lambda

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After Wrapping ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

      val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 11) then 
                  (prLexp(".log2",lambda); bug "lambda typing errors2 !")
                else ())
              else ()



      val ltyComp = 
       Stats.doPhase(Stats.makePhase "Compiler 055 ltyComp") LtyComp.ltyComp

      val lambda = ltyComp lambda
(*

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After ltycompilation ...]\n\n";
                    MCprint.printLexp lambda)
              else ()
*)
    val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 21) then 
                  (prLexp(".log3",lambda); bug "lambda typing errors3 !")
                else ())
              else ()

      val narrow = 
       Stats.doPhase(Stats.makePhase "Compiler 056 ltNarrow") LtNarrow.narrow

      val lambda = narrow lambda
(*
      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After ltynarrowing ...]\n\n";
                    MCprint.printLexp lambda)
              else ()
*)
      val _ = if !Control.CG.checklty1 then
               (if CheckLty.checkLty(lambda, 21) then 
                  (prLexp(".log4",lambda); bug "lambda typing errors4 !")
                else ())
              else ()

  val lambdaopt =
    Stats.doPhase(Stats.makePhase "Compiler 057 lambdaopt") LambdaOpt.lambdaopt

      val lambda = lambdaopt lambda
      val _ = if !Control.CG.checklty2 then
               (if CheckLty.checkLty(lambda, 21) then 
                  (prLexp(".log5",lambda); bug "lambda typing errors5 !")
                else ())
              else ()

  val reorder =
    Stats.doPhase(Stats.makePhase "Compiler 058 reorder") Reorder.reorder

      val lambda = reorder lambda
      val _ = if !Control.CG.checklty3 then
               (if CheckLty.checkLty(lambda, 31) then 
                  (prLexp(".log6",lambda); bug "lambda typing errors6 !")
                else ())
              else ()

      val _ = if !Control.CG.printLambda 
              then (say "\n\n[After lambdaopt and reorder ...]\n\n";
                    MCprint.printLexp lambda)
              else ()

   in lambda
  end 

val codeopt = Stats.doPhase (Stats.makePhase "Compiler 050 CodeOpt") codeopt

fun inline { genLambda, imports, symenv } =
      genLambda (map (SymbolicEnv.look symenv) imports)

(*
 * This is the real splitter, but we are not going to use it just yet (BLUME)
 *
 * fun split {enable = false, lambda} =
 *        {lambda_e = lambda, lambda_i = NONE}
 *   | split {enable = true, lambda} =
 *       case LambdaSplit.split lambda 
 *        of NONE => {lambda_e = lambda, lambda_i = NONE}
 *         | SOME {inline, expan} =>
 *             {lambda_e = expan, lambda_i = SOME inline}
 *
 *)

(* `conservative' splitting (i.e., none) *)
fun split { lambda, enable } = 
  let val (lambda_e, lambda_i) =
 	  (* act as if it were always disabled *)
	  (lambda, NONE)
   in { lambda_e = lambda_e, lambda_i = lambda_i }
  end

(* FIX: should just pass the compInfo parameter of Machm.codegen *)
fun codegen{lambda,
            compInfo={error,anyErrors,errorMatch,sourceName,...}: EU.compInfo} = 
    let val v = Machm.codegen({error=error,anyErrors=anyErrors,errorMatch=errorMatch},
        		  codeopt lambda)
    in
        (#name v) := SOME(sourceName); v
    end

val codegen = Stats.doPhase (Stats.makePhase "Compiler 140 CodeGen") codegen

fun csegsize { c0, cn , name} =
    foldl (fn (x, y) => (Word8Vector.length x) + y) (Word8Vector.length c0) cn

val addCode = Stats.addStat (Stats.makeStat "Code Size")

val codegen = fn x =>
  let val c = codegen x
   in addCode(csegsize c); c
  end

type ovec = obj vector

val mkCodeV : Word8Vector.vector * string option -> (Word8Vector.vector * (ovec -> obj)) =
      Unsafe.CInterface.c_function "SMLNJ-RunT" "mkCode"
val mkCodeO : Word8Vector.vector * string option -> (Word8Vector.vector * (obj -> obj)) =
      Unsafe.CInterface.c_function "SMLNJ-RunT" "mkCode"

fun applyCode { c0, cn , name} =
	let val s = case (!name) of NONE => "EMPTY COMMENT <-- check"
                               | SOME(str) => str
        in
	   foldl (fn (c, r) => (#2 (mkCodeO (c,NONE))) o r) (#2 (mkCodeV (c0,SOME(s)))) cn
        end
    
fun execute{executable,imports,exportPid,dynenv} = 
  let val result =
        executable (Vector.fromList (map (DynamicEnv.look dynenv) imports)
		     handle DynamicEnv.Unbound =>
		       (app (fn p => (print "lookup ";
				      print(PersStamps.toHex p);
				      print "\n")) imports;
                        fail "imported objects not found or inconsistent"))
   in case exportPid 
       of NONE => DynamicEnv.empty
	| SOME p => DynamicEnv.singleton (p, result)
  end

val execute = Stats.doPhase (Stats.makePhase "Execute") execute

exception TopLevelCallcc
exception TopLevelException of exn
exception SilentException = SilentException (* raised by CM *)

local val cont_stack = ref (nil : unit ref list)
in 
fun isolate f x = (* Just like f x, except that it catches
		     top-level callcc's  *)
  let val r = ref()
      val _ = cont_stack := r :: !cont_stack;
      fun pop_stack() =
	   case !cont_stack
	    of r' :: rest => (cont_stack := rest;
			      if r<>r' then raise TopLevelCallcc else ())
	     | _ => raise TopLevelCallcc (* can this ever happen? *)
      val a = f x 
       handle e => (pop_stack(); 
		    raise (case e of TopLevelException x => x | e => e))
   in pop_stack (); a
  end
end (* local of cont_stack *)

end (* local of CompileF *)
end (* functor CompileF *)

end (* local of exception Compile *)



(*
 * $Log: compile.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:43  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.10  1997/09/22 18:31:31  appel
 * Don't wrap TopLevelException around exn more than once
 *
 * Revision 1.9  1997/09/22  17:36:38  appel
 * Eliminate build/topcompile.sml by merging it with build/compile.sml
 *
 * Revision 1.8  1997/08/25  19:20:02  riccardo
 *   Added support for tagging code objects with their source/bin file name.
 *
 * Revision 1.7  1997/08/11  18:29:38  george
 *   Simplified the modmap handling by no longer paying attention to
 *   space leak problems.  Such problems don't matter in this version,
 *   because modmaps aren't used for the top-level environment.
 * 							-- blume
 *
 * Revision 1.6  1997/08/02  02:09:09  dbm
 *   Change in type of coreEnv to StaticEnv.staticEnv.
 *
 * Revision 1.5  1997/06/30  19:37:01  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.4  1997/05/05  19:55:04  george
 *    Turning off some measurement hooks - zsh
 *
 * Revision 1.3  1997/04/18  15:48:33  george
 * *** empty log message ***
 *
 * Revision 1.2  1997/02/26  21:48:52  george
 *    Fix the BUG 1116 about very slow top-level interactive loop, reported
 *    by Larry Paulson (Dave, if you have HOL with you, can you test this out?)
 *
 *)

