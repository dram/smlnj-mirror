(* 230.sml *)

signature S1 =
sig
  eqtype t
  val x : t
end;

functor F(A:S1) = 
struct
  functor G(X:sig end) =
  struct
    structure B=A
  end
end;

funsig FS1(A:S1) = 
sig
  functor G(X:sig end):sig structure B:S1 where type t = A.t end
end;

functor K(functor H:FS1) = struct
  structure a:S1 = struct datatype t = c1  val x=c1 end;
  structure b=H(a);
  structure c=b.G(struct end);
  val b = a.x = c.B.x;
end;

structure d = K(functor H=F);
d.b;
