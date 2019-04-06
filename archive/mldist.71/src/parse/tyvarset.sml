signature TYVARSET = 
  sig type tyvarset
      val no_tyvars : tyvarset
      val singleton_tyvar : Basics.tyvar -> tyvarset
      val union_tyvars : tyvarset * tyvarset -> tyvarset
      val diff_tyvars : tyvarset * tyvarset -> tyvarset
      val get_tyvars: tyvarset -> Basics.tyvar list
  end

abstraction TyvarSet : TYVARSET =
struct
  open Basics
  type tyvarset = tyvar list

  val no_tyvars = nil
  fun singleton_tyvar t = [t]
  fun get_tyvars s = s

  fun mem(a as ref(UBOUND{name,...}), 
	  (b as ref(UBOUND{name=n,...}))::rest) =
		if a=b then true
		else if Symbol.eq(name,n)
		  then (a := INSTANTIATED(VARty b); true)
		  else mem(a,rest)
    | mem _ = false

  fun union_tyvars([],s) = s
    | union_tyvars(s,[]) = s
    | union_tyvars(a::r,s) = if mem(a,s) then union_tyvars(r,s)
				else a::union_tyvars(r,s)
  fun diff_tyvars(s,[]) = s
    | diff_tyvars([],_) = []
    | diff_tyvars(a::r,s) = if mem(a,s) then diff_tyvars(r,s) 
					else a::diff_tyvars(r,s)
end
