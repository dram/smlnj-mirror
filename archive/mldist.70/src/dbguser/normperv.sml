(* Following is to get icounting for normal pervasives; this
   isn't a big deal... *)
use "boot/fastlib.sml";

structure NORM_PERV_X = struct
   open Fastlib
   structure IO = IO
   structure Integer = Integer
   structure Real = Real
   structure Bool = Bool
   structure String = String
   structure Array = Array
   structure Ref = Ref
   structure List = List
   open IO Integer Real Bool String Array Ref List
end

structure NORM_PERV = struct
    open NORM_PERV_X
    overload makestring : ('a -> string)
	  as Bool.makestring and Integer.makestring and Real.makestring
    overload print : ('a -> unit)
	  as Bool.print and Integer.print and Real.print and String.print
    overload ~ :   ('a -> 'a)        as Integer.~   and Real.~
    overload + :   ('a * 'a -> 'a)   as Integer.+   and Real.+
    overload - :   ('a * 'a -> 'a)   as Integer.-   and Real.-
    overload * :   ('a * 'a -> 'a)   as Integer.*   and Real.*
    overload < :   ('a * 'a -> bool) as Integer.<   and Real.<  and String.<
    overload > :   ('a * 'a -> bool) as Integer.>   and Real.>  and String.>
    overload <= :  ('a * 'a -> bool) as Integer.<=  and Real.<= and String.<=
    overload >= :  ('a * 'a -> bool) as Integer.>=  and Real.>= and String.>=
    overload abs : ('a -> 'a)        as Integer.abs and Real.abs
end
