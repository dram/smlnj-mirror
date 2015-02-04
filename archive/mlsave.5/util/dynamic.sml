signature STATIC_ARRAY =
  sig type array
      exceptionx subscript : unit
      type elem
      val array : int * elem -> array
      val sub : array * int -> elem
      val update : (array * int * elem) -> unit
      val length : array -> int
  end

signature DYNAMIC_ARRAY = 
  sig type array
      exceptionx subscript : unit
      type elem
      val array : elem -> array
      val sub : array * int -> elem
      val update : (array * int * elem) -> unit
  end


functor Dynamic( A : STATIC_ARRAY ) : DYNAMIC_ARRAY =
struct
     type array = {default: A.elem,
		   arr: A.array ref};
     type elem = A.elem;
     exceptionx subscript;
     fun array e = {default=e, arr= ref(A.array(16,e))};
     fun {default, arr as ref a} sub i = 
	A.sub(a,i)
	handlex A.subscript =>
	    if i < 0 then raisex subscript
	    else default
     fun update (me as {default,arr as ref a}, i, e) =
	A.update(a,i,e)
        handlex A.subscript =>
	  if i<0 then raisex subscript
	   else let val _ = print("doubling\n");
		    val size = A.length a
		    val newsize = 2 * size
		    val a2 = A.array(newsize, default);
		    fun copy j = (A.update(a2,j,A.sub(a,j)); copy(j-1))
		 in (copy (size - 1) handlex A.subscript => ());
		    arr := a2;
		    update(me,i,e)
	        end

end

