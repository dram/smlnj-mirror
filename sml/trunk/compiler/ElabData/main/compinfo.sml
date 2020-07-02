(* compinfo.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CompInfo = struct

(* only used in the form Absyn.dec compInfo, so the
 *  'absyn parameter not needed:
 *
 * type compInfo = { ... tranform: Absyn.dec -> Absyn.dec, ...}
*)
type 'absyn compInfo =
     { mkStamp: unit -> Stamps.stamp,
       mkLvar: Symbol.symbol option -> Access.lvar,
       anyErrors: bool ref,
       error: ErrorMsg.errorFn,
       errorMatch: SourceMap.region -> string,
       transform: 'absyn -> 'absyn,
       sourceName : string }

  fun mkCompInfo { source, transform : 'a -> 'a, mkStampGenerator } =
      let val { error, errorMatch, anyErrors } = ErrorMsg.errors source
          val _ = LambdaVar.clear () (* reset base lambda var to 0 *)
          fun mkLvar NONE = LambdaVar.mkLvar ()
            | mkLvar (SOME sym) = LambdaVar.namedLvar sym
      in { mkStamp = fn () => Stamps.fresh(mkStampGenerator ()),
           mkLvar = mkLvar,
           anyErrors = anyErrors,
           error = error,
           errorMatch = errorMatch,
           transform = transform,
           sourceName = #fileOpened source } : 'a compInfo
      end

  fun anyErrors (ci : 'a compInfo) = ! (#anyErrors ci)

end (* structure CompInfo *)
