(* bug1047.sml *)

(* testing SML/NJ Library Rand module *)
Rand.random((Rand.randMax + Rand.randMin) div 0w2);
Rand.random it;
Rand.random it;
