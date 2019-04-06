structure TimeLex =
 struct
fun lexer fname =
      let val stream = open_in fname
	  val _ = Lex.pushSource(stream, fname)
	  fun cleanup() = (print("[done lexing " ^ fname ^ "]\n");
			   close_in stream;
			   Lex.popSource())
	  fun loop() = case (Lex.advance(); !Lex.nextToken) of
		         Token.EOF => cleanup ()
		       | _ => loop ()
      in loop()
      end
val allfiles = ["boot/loader.sml",
	        "util/errormsg.sml",
		"util/dynamic.sml",
		"util/sort.sml",
		"util/sortedlist.sml",
		"util/binsort.sml",
		"util/list2.sml",
		"util/intmap.sml",
		"util/intset.sml",
		"util/topsort.sml",
		"util/union.sml",
		"basics/symbol.sml",
		"basics/token.sml",
		"util/printutil.sml",
		"lex/ascii.sml",
		"lex/symbols.sml",
		"lex/ml.lex.sml",
		"lex/hookup.sml",
		"basics/table.sml",
		"basics/access.sig",
		"basics/access.sml",
		"basics/basics.sig",
		"basics/basics.sml",
		"basics/tuples.sml",
		"basics/basictypes.sig",
		"basics/basictypes.sml",
		"basics/typesutil.sig",
		"basics/typesutil.sml",
		"env/prim.sml",
		"env/env.sml",
		"env/envaccess.sig",
		"env/envaccess.sml",
		"basics/printtype.sml",
		"basics/printbas.sml",
		"basics/conrep.sml",
		"absyn/bareabsyn.sig",
		"absyn/bareabsyn.sml",
		"absyn/absyn.sml",
		"absyn/printabsyn.sml",
		"typing/overload.sig",
		"typing/overload.sml",
		"typing/typecheck.sig",
		"typing/typecheck.sml",
		"typing/sharing.sml",
		"typing/sigmatch.sml",
		"parse/first.sml",
		"parse/misc.sml",
		"parse/parse.sig",
		"parse/parse.sml",
		"basics/lambda.sml",
		"codegen/opt.sml",
		"translate/mcprint.sml",
		"translate/mcopt.sml",
		"translate/mc.sml",
		"translate/nonrec.sml",
		"translate/equal.sml",
		"translate/unboxed.sml",
		"translate/translate.sml",
		"codegen/cgoptions.sml",
		"codegen/machine.sig",
		"codegen/backpatch.sml",
		"bignums/bitops.sml",
		"bignums/bigint.sig",
		"bignums/bigint.sml",
		"bignums/realconst.sml",
		"vax/vaxcoder.sig",
		"vax/vaxascode.sml",
		"vax/vaxmcode.sml",
		"cps/cps.sml",
		"cps/cpsoption.sml",
		"cps/size.sml",
		"cps/profile.sml",
		"cps/cpsprint.sml",
		"cps/freemap.sml",
		"cps/convert.sml",
		"cps/cpsopt.sml",
		"cps/globalfix.sml",
		"cps/spill.sml",
		"cps/cmachine.sig",
		"cps/vax2.sml",
		"cps/generic.sml",
		"cps/closure.sml",
		"build/process.sml",
		"cps/compile.sml",
		"cps/batch.sml"]
fun lexallfiles() =
 let open System.Timer
     val start = start_timer()
  in app lexer allfiles;
     print ("CPU time: " ^ makestring(check_timer(start)));
     print ("\nGC time :" ^ makestring(check_timer_gc(start)));
     print "\n"
  end

val _ = lexallfiles ()
end
