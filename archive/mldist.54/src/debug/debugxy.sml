structure DEBUGY = struct
   structure IO = HistoricalIO
   structure Integer = struct open Integer
			      val print = IO.output IO.std_out o makestring
		       end
   structure Real = struct open Real
			      val print = IO.output IO.std_out o makestring
		       end
   structure Bool = struct open Bool
			      val print = IO.output IO.std_out o makestring
		       end
   structure String = struct open String
			      val print = IO.output IO.std_out
		       end
   structure Array = HistoricalArray
   structure Ref = HistoricalRef
   open IO Integer Real Bool String Array Ref
end

structure DEBUGX = struct
    open DEBUGY
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
