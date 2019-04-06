(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkMakeStruct(structure LrTable : LR_TABLE) : MAKE_STRUCT =
   struct

      structure LrTable = LrTable
      open LrTable
     
      (* lineLength = approximately the largest number of characters to allow
	 on a line when printing out an encode string *)
	  
      val lineLength = 72

      (* maxLength = length of a table entry.  All table entries are encoded
	 using two 16-bit integers, one for the terminal number and the other
	 for the entry.  Each integer is printed as two characters (low byte,
	 high byte), using the ML ascii escape sequence.  We need 4
	 characters for each escape sequence and 16 characters for each entry
      *)

      val maxLength =  16

      (* number of entries we can fit on a row *)

      val numEntries = lineLength div maxLength

      (* convert integer between 0 and 255 to the three character ascii
	 decimal escape sequence for it *)

      val chr =
	let val lookup = array(256,"\000")
	    val intToString = fn i =>
		if i>=100 then "\\" ^ (makestring i)
		else if i>=10 then "\\0" ^ (makestring i)
		else  "\\00" ^ (makestring i)
	    fun loop n = if n=256 then ()
			 else (update(lookup,n,intToString n); loop (n+1))
	in loop 0; fn i => lookup sub i
	end

      val makeStruct = fn {table,name,print} =>
       let
	 val states = numStates table

         fun printList (prEntry : 'a -> unit) l =
		revfold (fn (a,count) => 
			     let val newcount = count + 1
			     in if newcount >= numEntries then
				  (print "\\\n\\"; prEntry a; 0)
				else (prEntry a; newcount)
			     end)  l 0

	  fun printRow (prEntry,prEnd) =
	       let val printEntries = printList prEntry
	       in fn (l,default) => (printEntries l; prEnd default)
	       end

	  fun printTable (getRow,prEntry,prEnd) =
	      let val printRow = printRow(prEntry,prEnd)
		  val doRow = printRow o getRow
	      in print "\"\\\n\\";
		 let fun f i = if i=states then ()
			       else (doRow (STATE i); f (i+1))
		 in f 0
		 end;
		 print"\"\n"
	      end
	       
	  val printChar = print o chr

	  (* print an integer between 0 and 2^16-1 as a 2-byte character,
	     with the low byte first *)

	  val printInt = fn i => (printChar (i mod 256);
				  printChar (i div 256))

	 (* encode actions as integers:

		ACCEPT => 0
		ERROR => 1
		SHIFT i => 2 + i
		REDUCE rulenum => numstates+2+rulenum

	 *)

	  val printAction =
	      fn (REDUCE rulenum) => printInt (rulenum+states+2)
		 | (SHIFT (STATE i)) => printInt (i+2)
		 | ACCEPT => printInt 0
		 | ERROR => printInt 1
	
	   val printTermAction = fn (T t,action) =>
		(printInt (t+1); printAction action)

	   val printGoto = fn (NT n,STATE s) => (printInt (n+1); printInt s)

	   val endOfRow = "\\000\\000"
	   val endOfLn = "\\\n\\"

	   val defaultAction = fn a => (print endOfRow; printAction a;
					print endOfLn)
	   val defaultGoto = fn _ => (print endOfRow; print endOfRow;
					print endOfLn)

	   val getActions = describeActions table 
	   val getGoto = fn state => (describeGoto table state,())

	in print "val ";
	   print name;
	   print "=";
	   print "let val actionT =\n";
	   printTable(getActions,printTermAction,defaultAction);
	   print "val gotoT =\n";
	   printTable(getGoto,printGoto,defaultGoto);
	   print "val numstates = ";
	   print (makestring states);
	   print "\n\
\val string_to_int = fn(s,index) => (ordof(s,index) + \n\
\			ordof(s,index+1)*256,index+2)\n\
\	val convert_string_to_row = fn (conv_key,conv_entry) =>\n\
\	     fn(s,index) =>\n\
\		let fun f (r,index) =\n\
\			let val (num,index) = string_to_int(s,index)\n\
\			    val (i,index) = string_to_int(s,index)\n\
\			in if num=0 then ((rev r,conv_entry i),index)\n\
\			   else f((conv_key (num-1),conv_entry i)::r,index)\n\
\			end\n\
\		in f(nil,index)\n\
\		end\n\
\	 val convert_string_to_row_list = fn conv_funcs => fn s =>\n\
\		    let val convert_row =convert_string_to_row conv_funcs\n\
\		 	fun f(r,index) =\n\
\			  if index < String.length s then\n\
\			    let val (newlist,index) = convert_row (s,index)\n\
\			    in f(newlist::r,index)\n\
\			    end\n\
\			  else rev r\n\
\		    in f(nil,0)\n\
\		    end\n\
\	 val entry_to_action = fn j =>\n\
\		       if j=0 then ACCEPT\n\
\		       else if j=1 then ERROR\n\
\		       else if j >= (numstates+2) then REDUCE (j-numstates-2)\n\
\		       else SHIFT (STATE (j-2))\n\
\	 val make_goto_table = convert_string_to_row_list(NT,STATE)\n\
\	 val make_action_table=convert_string_to_row_list(T,entry_to_action)\n\
\	 val gotoT = map (fn (a,b) => a) (make_goto_table gotoT)\n\
\	 val actionT = make_action_table actionT\n\
\     in LrTable.mkLrTable {actions=actionT,gotos=gotoT,\n\
\	  numStates=numstates,initialState=STATE ";
print (makestring ((fn (STATE i) => i) (initialState table)));
print "}\n\
\     end\n"
	end
end;
