let fun foo () = 3
 in let val n = foo() handle x =>
			     raise x
    in ()
    end
end
