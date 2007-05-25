(* 24.sml *)
(* Rosenberg looping bug. OK case. *)

datatype 'a rec_t = Recurse of 'a rec_t -> 'a -> 'a;

val mkrec = fn f => f (Recurse f);                                             

val f = mkrec                                                              
           (fn (Recurse F) =>
               (fn 0 => 0                                   
                 | a => (F (Recurse F) (a-1))))

val x = f 10;
