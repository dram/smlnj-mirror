structure Emitters : EMITTERS = struct
    local 
        val so_far = ref nil : string list ref
        fun squirrel s = so_far := s :: !so_far
        fun emit_byte n = squirrel (chr n)
    in
        fun emit_string n s = squirrel (substring(s,0,n))
                	handle e =>
                		(print "?exception "; print (System.exn_name e);
                		 print (" in emitters.emit_string "^
        				(Integer.makestring n) ^ " \""^s^"\"\n");
                		 raise e)

        fun emit_pair_little(hi,lo) =
            let open Bits
                fun emit_word(n) =
                  (emit_byte(andb(n,255));emit_byte(andb(rshift(n,8),255)))
            in  (emit_word(lo);emit_word(hi))
            end

        fun emit_pair_big(hi,lo) =
             let open Bits
                fun emit_word(n) =
                  (emit_byte(andb(rshift(n,8),255));emit_byte(andb(n,255)))
            in  (emit_word(hi);emit_word(lo))
            end

	fun emitted_string () =
	    let val s = implode (rev (!so_far))
	    in  so_far := nil; s
	    end
    end
    structure LittleEndian = struct
			       val emit = emit_pair_little
			       val order_real = implode o rev o explode
			       val low_order_offset = 0
			       val emit_string = emit_string
			       fun emit_comment _ = ()
			     end
    structure BigEndian    = struct
			       val emit = emit_pair_big
			       fun order_real x = x
			       val low_order_offset = 1
			       val emit_string = emit_string
			       fun emit_comment _ = ()
			     end
    val address = ref 0		(* address of next instruction in words *)
    val asmstream = ref std_out
    structure MipsAsm : EMITTER = struct
        fun say s = (output (!asmstream) s; flush_out (!asmstream))
        fun printaddr addrref = 
           let val n = !addrref
           in  (if n<10 then "  " else if n < 100 then " " else "") 
                ^ (Integer.makestring n) 
           end
        local 
            open Bits
            fun hexdigit n = 
                let val d = andb(n,15)
                in  if d <= 9 then chr(d+ord("0"))
                              else chr(d-10+ord("a"))
                end
            fun hex1 n = hexdigit(rshift(n,4))^hexdigit(n)
            fun hex2 n = hex1(rshift(n,8))^hex1(n)
            fun hex4 n = hex2(rshift(n,16))^hex2(n)
        in
            fun hex(hi,lo) = hex2(hi) ^ hex2(lo)
            fun printaddr addrref = 
           	let val n = 4 * (!addrref)	(* address in bytes *)
           	in "0x" ^ (hex4 n) 
           end
        end
        fun decode x = (
                say ((printaddr address) ^ ": (" ^ (hex x) ^") " 
                 ^ (MipsDecode.decode x));
           address := !address + 1; ()
           )
        fun decode_string n s =
            if n > 0 then
                (say ((printaddr address) 
                            ^ ": \"" ^substring(s,0,4) ^"\"\n");
                   address := !address + 1;
                   decode_string (n-4) (substring(s,4,String.length(s)-4))
                   )
            else ()
    
        val emit = decode
        fun order_real x = x
        val low_order_offset = 1  (* should this be different for little-endian?*)
        val emit_string = decode_string
        val emit_comment = say
    end

end


