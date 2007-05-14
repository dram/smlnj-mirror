(* Test strdefspec intersection calculation 
   This should fail because MSIG.b <> Q.b *)
signature MSIG =
sig 
    type z = int 
    structure C : sig type u = unit end
    type b = bool
end

structure Q =
struct
    type z = int
    structure C = struct type u = unit end
    type b = int
end

signature S =
sig
    structure M : MSIG = Q
end

structure T : S =
struct
    structure M = 
    struct 	
	type b = bool
	type z = int
	structure C = struct type u = unit end
    end
end