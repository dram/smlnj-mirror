(* 25.sml *)
(* Rosenberg looping bug. Failure case. *)

datatype 'a rec_t = Recurse of 'a rec_t -> 'a -> 'a;

val f = 
    let val _ = print "executing\n"                                    
        val mkrec = (fn f => f (Recurse f))                            
     in mkrec (fn (Recurse F) =>
                  (fn 0 => 0 | a => (F (Recurse F) (a-1))))
    end;
