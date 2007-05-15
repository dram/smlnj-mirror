(* This testcase shouldn't produce a secondary matchDefStr (2) compiler bug *)

structure R : sig type t end = struct type u = bool end

structure T = 
  struct 
    structure K = 
      struct
        type y = R.t
	type u = y
      end
    structure A = 
      struct 
        structure B = struct type t = unit end 
      end
  end 
 

signature S0 = 
sig
    structure M0 : 
	      sig 
		  structure K : sig type u = R.t end
		  structure A : sig structure B : sig type t=R.t end end 
	      end = U
end

structure W = 
  struct 
    structure K = struct type y = int end
    structure A = 
      struct 
        structure B = struct type t = R.t end 
      end 
  end

structure N : S0= 
struct
  structure M0 = W
end


