(* 19.sml *)

(* resolving overloaded operator *)
(* must resolve overloading (by defaulting) before 
 * type generalization *)

type 'a t = 'a;

val f : ('a t * 'a t -> bool) -> 'a -> 'a -> bool = 
    fn (op <) => fn x => fn y => (x < y);

val x = f (op <);
