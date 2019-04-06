
signature WEAK = 
 sig type 'a weak
     val weak : 'a -> 'a weak
     val strong : 'a weak -> 'a option
 end
abstraction Weak : WEAK =
struct
  open System.Unsafe
  type 'a weak = 'a
  fun weak x = cast(delay(22,x))
  fun strong x = case (cast (subscript(cast x,0)), 
                       cast (subscript(cast x, ~1)))
                  of (y,22) => SOME y
		    | _ => NONE	
end
