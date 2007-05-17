(* Generative Types and StrDefMatch0 
   The following should not pass signature matching *)

signature S0 =
sig
  structure A : sig end
  structure B : sig datatype t = A end
end

signature S1 = 
sig
  structure C : sig end
  structure B : sig datatype t = A end
end

functor F(structure M0 : S0
          structure M1 : S1 where B = M0.B) =
struct
end
		
functor G(structure T : S0
	  structure U : S1) =
struct
  structure Z = F(structure M0 = T
	          structure M1 = U)
  (* The above should fail because 
     T and U have different (generative) B.t *)
end
