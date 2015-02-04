structure Overloads =
struct
  overload ~ : ('a->'a) as Initial.Pervasives.Integer.~ 
		       and Initial.Pervasives.Real.~
  overload + : ('a*'a -> 'a) as Initial.Pervasives.Integer.+ 
			    and Initial.Pervasives.Real.+
  overload - : ('a*'a -> 'a) as Initial.Pervasives.Integer.- 
			    and Initial.Pervasives.Real.-
  overload * : ('a*'a -> 'a) as Initial.Pervasives.Integer.* 
			    and Initial.Pervasives.Real.*
  overload < : ('a*'a -> bool) as Initial.Pervasives.Integer.< 
			      and Initial.Pervasives.Real.< 
			      and Initial.Pervasives.String.<
  overload > : ('a*'a -> bool) as Initial.Pervasives.Integer.> 
			      and Initial.Pervasives.Real.> 
			      and Initial.Pervasives.String.>
  overload <= : ('a*'a -> bool) as Initial.Pervasives.Integer.<= 
			       and Initial.Pervasives.Real.<= 
			       and Initial.Pervasives.String.<=
  overload >= : ('a*'a -> bool) as Initial.Pervasives.Integer.>= 
				and Initial.Pervasives.Real.>= 
				and Initial.Pervasives.String.>=
  overload abs : ('a->'a) as Initial.Pervasives.Integer.abs 
			 and Initial.Pervasives.Real.abs
  overload length : ('a -> int) as Initial.Pervasives.String.length 
			       and Initial.Pervasives.List.length
			       and Initial.Pervasives.Array.length
  overload makestring : ('a -> string)
        as Initial.Pervasives.Bool.makestring 
       and Initial.Pervasives.Integer.makestring 
	and Initial.Pervasives.Real.makestring
  overload print : ('a -> 'a)
        as Initial.Pervasives.Bool.print 
       and Initial.Pervasives.Integer.print 
	and Initial.Pervasives.Real.print
	and Initial.Pervasives.String.print
end
