(* bug1038.4.sml *)

signature SIGA =
sig
  type t
end;

signature SIGB =
sig
  datatype tt = TT
  type t = tt		(*XXX*)
end;

functor F(structure B : SIGB
          structure A : SIGA where type t = B.t) = struct end;	(* fails *)
