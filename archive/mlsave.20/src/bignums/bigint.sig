signature BIGINT =
  sig type bigint			(* can be negative or positive *)
      val bigint : int -> bigint
      exception Smallint
      val smallint : bigint -> int
      exception Getbit
      val getbit : bigint * int -> int  (* get the i'th bit; low-order
				          bit is numbered 0; 2's-complement *)
      val isneg : bigint -> bool
      val size : bigint -> int		(* minimum bits in signed integer *)
      val makestring : bigint -> string
      val print : bigint -> bigint
      val + : bigint * bigint -> bigint
      val * : bigint * bigint -> bigint
      val - : bigint * bigint -> bigint
      val ~ : bigint -> bigint
      val >> : bigint * int -> bigint
  end
