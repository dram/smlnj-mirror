(* bug926.sml *)

ArraySlice.vector (ArraySlice.slice(Array.fromList ["a","b"], 1, SOME 0));
