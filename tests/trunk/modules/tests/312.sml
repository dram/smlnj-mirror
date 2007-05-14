(* Two testcases: 
     Using datatype as a unique stamp for structure def'n spec sharing
     Using datatype replication to steal the def'n spec 
*)
structure N = struct datatype u = A end

signature S =
sig
  structure M : sig datatype u = A end = N
end

(* Should typecheck *)
structure T1 : S =
struct
	structure M = N
end

(* Should typecheck *)
structure T2 : S =
struct
structure M = struct datatype u = datatype N.u end
end

(* Should fail *)
structure T0 : S =
struct
structure M = struct datatype u = A end
end

	