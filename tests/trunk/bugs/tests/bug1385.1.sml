(* bug1385.1.sml *)
(* should not elaborate *)

signature CAT = sig datatype t = C end;

signature L_F =
sig
  structure Cat : CAT
end;

signature TRANSLATOR =
sig
  structure Cat : CAT
  structure LF : L_F 
      where Cat = Cat
end;

functor Translator
    ( structure Cat : CAT
      structure LF : L_F ) : TRANSLATOR
				 where Cat = Cat
				 where LF = LF
				 =
struct
  structure Cat = Cat
  structure LF = LF
end;
