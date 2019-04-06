(* Bigints are stored as sequences of base-256 digits, in two's complement.
   Digit 0 is the low-order digit, digit (n-1) is the high-order digit.
   The high-order digit is >127 iff the number represented is negative;
   if the number is positive, either the msd is <128 or an extra zero is
   tacked on as the msd. *)

structure Bigint : BIGINT =
struct
  open ByteArray

  type bigint = bytearray

    (* arrayoflist for bytearrays *)
    fun arrayoflist l =  (* weird error in our comp - would compile when I
			    typed l as (l : 'a list) *)
	let val a = array(List.length l,0)
	in
	    List.revfold (fn(value,index)=>(update(a,index,value);index+1)) l 0;
	    a
	end

    (* isneg:  is a bigint negative? *)
    fun isneg a = (a sub (length a - 1)) >= 128

    (* bigint_pos:  Creates a bigint for a positive number *)
    fun bigint_pos i =
	let fun bytelist i =
		if i < 128 then [i]
		else if i < 256 then [i,0]
		else (i mod 256) :: bytelist(i div 256)
	in
	    arrayoflist(bytelist i)
	end

    fun shorten c x = (* get rid of leading x digits in a bigint *)
	let fun f i = if (c sub i) = x andalso ((c sub (i-1)) >= 128)=(x>=128)
			then f(i-1) else i + 1
	    val len = f (length c - 1) handle Subscript => 1
	in  if len = length c then c
	    else let val c' = array(len,0)
		     fun copy i = (update(c',i,c sub i); copy (i-1))
		  in copy(len-1) handle Subscript => c'
		 end
	end

    fun negate a = 
	let val alen = length a
	    val b = array(alen,0)
	    fun next(i,c) =
		let val x = 255 + c - (a sub i)
		in  if x=256 
		    then (update(b,i,0); next(i+1,1))
		    else (update(b,i,x); next(i+1,0))
		end
	in  next(0,1) handle Subscript =>
		(case (isneg a,isneg b) of
		      (true,true) => let val c = array(alen + 1,0)
				     in  update(c,alen - 1,128);
					 c
				     end
		    | (false,true) => shorten b 255
		    | (_,false) => b)
	end

(* dangerous to use this - should not be exported.  *)
    fun negate_inplace a = 
	let val nega = isneg a
	    fun next(i,c) =
		let val x = 255 + c - (a sub i)
		in  if x=256 
		    then (update(a,i,0); next(i+1,1))
		    else (update(a,i,x); next(i+1,0))
		end
	in  next(0,1) handle Subscript =>
		(case (nega,isneg a) of
		      (true,true) => let val alen = length a
					 val c = array(alen + 1,0)
				     in  update(c,alen - 1,128);
					 c
				     end
		    | (false,true) => shorten a 255
		    | (_,false) => a)
	end

    fun bigint i =
	let val bi = bigint_pos(abs i)
	in
	    if i < 0 then (negate_inplace bi) else bi
	end

(* Gives the size in bits of the bigint.  Starts counting from the first non-
   redundant 1-bit if negative, the first non-redundant 0-bit if positive.
   This gives the minimum size in bits of the signed number.  For example,
   the size of ~1 is 1 while the size of 1 is 2. *)
    fun size a = 
	let val n = length a
	    val msd = a sub (n-1)
	    val pos = msd < 128
	    fun bitsin 0 = 0
	      | bitsin i = 1 + bitsin(Bits.rshift(i,1))
	    val bits = case pos of true => bitsin msd + 1
				 | false => bitsin(256-msd)
        in  8*(n-1) + bits
	end

    exception Smallint

    (* smallint is machine dependent in that you must know the number of
       bits in order to raise smallint properly.  Here I assume 31 bits. *)
    (* inefficient for negative numbers *)
    fun smallint i =
	let val neg = isneg i
	    val n = size i
	    fun smallint_pos big =
		    fold (fn(digit,higher) => digit + 256 * higher) big 0
	in
	    if n > 31 then raise Smallint
	    else case neg of true => ~(smallint_pos(negate i))
			   | false => smallint_pos i
	end


(* Original version of smallint - relies on 31 bit ints on host machine.
    fun smallint a = 
	 let val n = length a
	     val h = a sub (n-1)
	     val neg = h>=128
	     val h = if neg then h-128 else h
	     val _ = if n>4 orelse h>63 then raise Smallint else ()
	     val i = case n of 1 => h
			| 2 => (a sub 0)+256*h
			| 3 => (a sub 0)+256*((a sub 1)+256*h)
		        | 4 => (a sub 0)+256*((a sub 1)+256*((a sub 2)+256*h))
	 in if neg then i-536870912-536870912 else i
        end
*)

    (* NOTE:  will not raise getbit on the implicit high 0-bit of positive
       numbers *)
    exception Getbit
    fun getbit (a,i) = 
	if size a < i + 1 orelse i < 0
	then raise Getbit
	else Bits.andb(Bits.rshift(a sub Bits.rshift(i,3),Bits.andb(i,7)),1)

   fun plus (a,b) =
      let val (a,b) = if length a < length b then (b,a) else (a,b)
	  val la = length a and lb = length b
	  val aneg = (a sub (la-1)) >= 128
	  val bneg = (b sub (lb-1)) >= 128
	  val vb = if bneg then 255 else 0
	  val c = array(la,0)
	  fun add(i,carry) = 
		if i=la then carry
		else let val x = (a sub i) 
			    + (b sub i handle Subscript => vb) + carry
		         val d = x div 256
		      in update(c,i, x mod 256); add(i+1,d)
		     end
	  val _ = add(0,0);
	  val cneg = (c sub (la-1)) >= 128
	  fun extend x = (* make a new bigint with msd x *)
	     let val n = length c
		 val c' = array(n+1, 0)
		 fun copy i = (update(c',i,c sub i); copy(i-1))
	      in update(c',n,x); copy(n-1) handle Subscript => c'
	     end
       in case (aneg,bneg,cneg)
	   of (true,true,false) => extend 255
	    | (false,false,true) => extend 0
	    | (_,_,true) => shorten c 255
	    | (_,_,false) => shorten c 0
      end

   fun times (a,b) =
	let val asign = isneg a
	    val a = if asign then negate a else a
            val bsign = isneg b
	    val b = if bsign then negate b else b
	    val la = length a and lb = length b
	    val lc = la + lb
	    val c = array(lc,0)
	    fun f i = if i=la then ()
		else let val av = a sub i
(* BUG - this could cause a store > 256 *)
(* Fixed.  Note this could cascade past the end of the array except that
   this will never happen due to the nature of multiply. *)
			 fun incr(j,0) = ()
			   | incr(j,x) =
			    let val new = x + (c sub (i+j))
			     in update(c,i+j,new mod 256);
				incr(j+1,new div 256)
			    end
			 fun g(j,x) = if j=lb then incr(j,x)
			    else let val m = av * (b sub j) + x
				     val mh = m div 256
				     val ml = m mod 256
				  in incr(j,ml); g(j+1,mh)
				 end
		      in g(0,0); f(i+1)
		     end
	 in f 0;
	    if asign=bsign then shorten c 0 else negate(shorten c 0)
	end

    (* Dies if you try to shift too far. *)
    fun rightshift(big,bits,bytes) =
	let val n = length big
	    val neg = isneg big
	    val s = size big mod 8
	    val lose = s <> 0 andalso s <= bits
	    val newsize = case lose of true => n-1-bytes
				     | false => n-bytes
	    val shifted = array(newsize,0)
	    fun pow2 n = Bits.lshift(1,n)
	    val mask = pow2 bits - 1
	    fun shift(0,_) = shifted
	      | shift(i,c) = let val digit = big sub (i+bytes-1)
			     in  update(shifted,i-1,Bits.rshift(digit,bits)+Bits.lshift(c,8-bits));
				 shift(i-1,Bits.andb(digit,mask))
			     end
	    val carry = case (lose,neg) of (true,_) => Bits.andb(big sub (n-1),mask)
					 | (false,true) => mask
					 | (false,false) => 0
	in  shift(newsize,carry)
	end

(* Danger: on a 0 shift, a new bigint is not made.  If in-place
   modifications are made, errors are going to happen.  If the guts of
   bigints are not exported, no need to worry. *)
local val zero = bigint 0
      val negone = bigint(~1)
in
    fun fullshift(num,0) = num
      | fullshift(num,n) =
	    let val s = size num
	    in
		if n >= s then case isneg num of true => negone
					       | false => zero
		else if n < 0 then num (* hacked as long as no leftshift *)
		else rightshift(num,Bits.andb(n,7),Bits.rshift(n,3))
	    end
end

    fun makestring b =
	let fun bytestr(_,0) = ""
	      | bytestr(bits,i) = let val more = bytestr(Bits.rshift(bits,1),i-1)
				      val bit = Integer.makestring(Bits.andb(bits,1))
				  in more ^ bit end
	    fun bytes 0 = bytestr(b sub 0,8)
	      | bytes i = bytestr(b sub i,8) ^ "." ^ bytes(i-1)
	in
	    bytes(length b - 1)
	end
    local val pr = output std_out in
    fun print b = (pr(makestring b); b)
    end

    val op + = plus and op * = times and ~ = negate and op >> = fullshift
    fun op - (a,b) = plus(a,negate b)
end
