(* bug1338.1.sml *)

signature SIG4 =
sig
  structure S : 
    sig type t 
        structure A: sig structure B : sig val x : t end
                     end
    end
  type s 
  type t = s 
  
end where type S.t = t;
 
functor F(X : SIG4) = struct end;
