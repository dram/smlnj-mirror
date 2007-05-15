(* Test matchDefStr0 loop *)

signature MSIG =
sig
	type a = int
	type b = unit
	type c = bool
end

structure Q = 
struct
	type a = int
	type b = unit
	type c = unit (* This shouldn't match MSIG.c *)
end

signature S = 
sig
	structure M : MSIG = Q
end

structure T : S =
struct
	structure M = 
	struct
		type a = int
		type b = unit
		type c = bool
	end
end
