(* bug615.sml *)
(* System.Symbol.makestring has incomplete implementation *)

let open Environment Symbol 
    val e = #get(EnvRef.pervasive) ()
 in List.map symbolToString (StaticEnv.symbols (staticPart(e)))
end;
