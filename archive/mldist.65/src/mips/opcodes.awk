# <*>=
# The sections BEGIN and END are drawn from 
#  our universal model of an awk program:

BEGIN {
  # <BEGIN>=
  # \chapter{Handling the MIPS opcodes}
  # \section{Introduction}
  # 
  # This file generates the code necessary to handle MIPS instructions
  # in a natural, mnemonic way from within ML.
  # All MIPS instructions occupy 32 bits, and since ML has no simple
  # 32~bit data type, we use pairs of integerss to represent MIPS instructions.
  # A pair [[(hi,lo)]] of 16-bit integers holds the most and least significant
  # halfwords of the MIPS word.
  # ML integers are 31 bits, so this is more than adequate.
  # 
  # The biggest hassle in converting between these integer pairs and more
  # mnemonic representations is that it is too easy to make mistakes
  # (especially typographical errors) in writing the code.
  # For that reason, I have added an extra level of indirection to the
  # whole business by putting all of the instruction descriptions in
  # tables.
  # These tables are read by an awk script, which writes two ML files:
  # {\tt opcodes.sml} and {\tt mipsdecode.sml}.
  # The {\tt opcodes.sml} file contains the code needed to convert from
  # a mnemonic like [[add(3,4,9)]] (add the contents of register~3 to
  # the contents of register~4, placing the result in register~9) to 
  # the integer pair representation of the actual bits in that add instruction
  # (in this case [[(137,6176)]]).
  # The {\tt mipsdecode.sml} file contains a [[decode]] function that converts
  # from the integer pair representation of instructions to a string
  # representation.
  # The string representation is a little hokey at the moment (that is,
  # it's different from the one used in the MIPS book), but it represents
  # a nice compromise between being readable and easy to generate.
  # 
  # I have contemplating generating a third file to test the whole
  # business.
  # The idea would be to have a function that would write out (to files)
  # two
  # parallel representations of the same instruction stream (presumably
  # one copy of each known instruction).
  # One representation would be the binary one understood by the MIPS.
  # The other representation would be a string representation.
  # We could then use a tool like {\tt gdb} or {\tt adb} to print out
  # the binary as an instruction sequence (i.e. convert back to
  # a second string representation) and compare the string representations
  # to see if they make sense.
  # 
  # \paragraph{Possible bugs}
  # This code should be gone over with care to make sure that negative
  # operands (e.g. in [[offset]]) won't break the code.
  # 
  # 
  
  # 
  # We need a special line in the Makefile to handle this file, since
  # it writes both an awk program and that program's input.  The input
  # is in module {\tt <<opcodes-table>>} so the line is
  # $$\hbox{[[	$(NOTANGLE) '-Ropcodes-table' opcodes.ow > opcodes]]}$$
  # The input is nothing but a sequence of tables, each labelled, and
  # processed one after anothing according to the label.
  # The label is always a single word on a line by itself.
  # Tables end with blank lines.
  
  # The opcode-to-pair code is written to the standard output, in 
  # [[structure Opcodes]].
  # The pair-to-string code is written to [["mipsdecode.sml"]], in
  # [[structure MipsDecode]].
  # 
  # We begin by defining and and shift functions.
  # We make pessimistic assumptions about shifting, trying always to
  # keep the arguments between 0 and 31 inclusive.
  
  print "structure Opcodes = struct"
  print "val andb = Bits.andb"
  print "fun lshift(op1,amt) = "
  print "    if amt<0 then Bits.rshift(op1,0-amt)"
  print "    else Bits.lshift(op1,amt)"
  print "nonfix sub"	# bug fixes; want [[sub]] to be a MIPS opcode
  print "nonfix div"	# bug fixes; want [[div]] to be a MIPS opcode
  
  decode = "mipsdecode.sml";
  print "structure MipsDecode = struct" > decode
  print "val andb = Bits.andb" > decode
  print "fun rshift(op1,amt) = " > decode
  print "    if amt<0 then Bits.lshift(op1,0-amt)" > decode
  print "    else Bits.rshift(op1,amt)" > decode
  # <BEGIN>=
  # The types are just constants set at the beginning.
  
  OPCODE = 1 ; SPECIAL = 2 ; BCOND = 3 ; COP1 = 4
  # <BEGIN>=
  stderr="/dev/tty"
}
# <functions>=
# ML uses a strange minus sign ([[~]] instead of [[-]]), 
# so we print numbers that might be negative like this:

function mlnumber(n, s) {
	if (n<0) s = sprintf("~%d", -n)
	else s = sprintf("%d", n)
	return s
}
# <functions>=
# For reasons best known to its designers, awk has no [[min]] function.

function min(x,y){
	if (x<y) return x
	else return y
}
# <functions>=
# \section{testing}
# One day someone will have to modify the instruction handler so that
# it generates a test invocation of each instruction.
# Then the results can be handed to something like adb or dbx and we can
# see whether the system agrees with us about what we're generating.
# 

# \section{Defining ML functions}
# The awk function [[function_definition]] is used to
# come up with ML function definitions.
# It takes as arguments the name of the function and the number of arguments
# to that function, and returns a string containing the initial part of
# the function definition.
# Writing an expression following that string will result in a complete
# ML function.
# 
# If we ever wanted to define these things as C preprocessor macros instead,
# we could do it by substituting [[macro_definition]].
# I'm not sure it would ever make sense to do so, but I'm leaving the
# code here anyway.

function function_definition(name, argc,  i, temp) {
	if (argc==0) {
		temp = sprintf("val %s = ", name)
	} else {
		temp = sprintf( "fun %s(", name)
		for (i=1; i< argc; i++) temp = sprintf("%sA%d,", temp,i)
		temp = sprintf( "%sA%d) = ", temp, argc)
	}
	return temp
}
# <functions>=
function insist_fields(n) {
	if (NF != n) {
		print "Must have", n, "fields on line",NR ":", $0 > stderr
		return 0
	} else {
		return 1
	}
}
# <statements>=
# Seeing the right table header causes us to set the right variable.
# We also remember the line number, because we use the positions of later
# lines to help extract the bit patterns from the table.

NF == 1 && $1 == "opcode" {
	startline = NR
	opcodes = 1
	next
}
NF == 1 && $1 == "special" {
	startline = NR
	specials = 1
	next
}
NF == 1 && $1 == "bcond" {
	startline = NR
	bconds = 1
	next
}
NF == 1 && $1 == "cop1" {
	startline = NR
	cop1s = 1
	next
}
# <statements>=
# Any time we see a blank line, that ends the appropriate table.

NF == 0 {opcodes = 0; specials = 0; bconds = 0; cop1s = 0
	# <blank line resets>=
        fields = 0
        # <blank line resets>=
        instructions= 0
}
# <statements>=
# Here is the code that actually extracts the bit patterns from
# the opcode tables.
# The code is the same for each of the three tables.
# 
# The [[insist_fields(8)]] issues an error message and returns false (0)
# unless there are exactly 8 fields on the input line.

opcodes || specials || bconds || cop1s {
	if (!insist_fields(8)) next
	# <set [[type]]>=
        # We determine the type by scanning the header word that precedes
        # each table.
        # Once we see the appropriate table header, we set one of [[opcodes]],
        # [[specials]], and [[bconds]], so that determining the type is easy:
        
        type = OPCODE * opcodes + SPECIAL * specials + BCOND * bconds + COP1 * cop1s
	major = NR - startline - 1		# major octal digit from row
	for (i=1; i<= NF; i++) {
		minor = i-1			# minor octal digit from column
		code = minor + 8 * major
		# <store opcode information>=
	        # 
	        # Now we have to deal with reading these tables, and extracting the
	        # information stored therein.
	        # First of all, for each mnemonic [[$i]] we store the corresponding bit
	        # pattern (as an integer, [[code]]) in the array [[numberof[$i] ]].
	        # Then, we store the type of the mnemonic (ordinary [[OPCODE]], 
	        # [[SPECIAL]], [[BCOND]], of [[COP1]]) in the array [[typeof[$i] ]].
	        # Finally, we store inverse (a map from type and bit pattern to mnemonic)
	        # in the [[opcode]] array.
	        
	        if ($i != "*") {
	        	numberof[$i] = code
	        	typeof[$i] = type
	        	opcode[type,code] = $i
	        } else {
	        	opcode[type,code] = "reserved"
	        }
	}
}
# <statements>=
# The setup for the fields is similar to that used for the opcodes.

NF == 1 && $1 == "fields" {
	startline = NR
	fields = 1
	# <write format info>=
        print "val S_fmt = 16+0"
        print "val D_fmt = 16+1"
        print "val W_fmt = 16+4"

	next
}
# <statements>=
fields {
	if (!insist_fields(3)) next
	fieldname = $1;  low = $2; high = $3
	# <look for sign in [[fieldname]] and set [[signed]]>=
        if (substr(fieldname,1,1)=="+") {
        	signed = 1
        	fieldname = substr(fieldname,2)
        } else {
        	signed = 0
        }
	fieldnames[fieldname]= 1	# rememeber all the field names

	# <write to standard output a function to convert bit-pattern to pair>=
        # 
        # The idea is that for each of these fields, we want to write a function
        # that will take an integer argument and shift it by the right amount.
        # Since we have to represent the 32-bit quantities as pairs of integers,
        # we actually use two functions, one for the high half and one for the low.
        # So, for example, for the [[rd]] field we will produce two function definitions,
        # [[rdHI]] and [[rdLO]].
        # 
        # The awk function [[function_definition]] is used to compute ML function
        # definitions.
        # It takes as arguments the name of the function and the number of arguments
        # to that function.
        # The arguments are numbered [[A1]], [[A2]], et cetera.
        # 
        # The functions themselves are all tedious combinations of ands and shifts.
        # At one time I had convinced myself that this worked.
        
        if (low >= 16) {
        	printf "%s", function_definition(fieldname "LO",1); print "0"
        } else {
        	printf "%s", function_definition(fieldname "LO",1)
                printf "andb(lshift(A1,%d),65535)\n", low
        }
        if (high < 16) {
        	printf "%s", function_definition(fieldname "HI",1); print "0"
        } else {
        	printf "%s", function_definition(fieldname "HI",1)
                printf "lshift(A1,%s)\n", mlnumber(low - 16)
        }
	# <write to [[decode]] a function to extract field from pair>=
        # The inverse operation is
        # to extract a bit pattern from a pair.
        # We'll want that if we ever care to decode instructions.
        # This time, the function to extract e.g.\ field [[rd]] from a pair
        # is the function [[THErd]] applied to that pair.
        # 
        # The functions work first by extracting from the low part, then
        # from the high part, and adding everything together.
        # If the field is signed, we make the value negative if it is too high.
        
        printf "%s", function_definition("THE" fieldname,2) > decode
        if (signed) printf "let val n = " > decode
        # <print expression for unsigned value>=
        if (low >= 16) {
        	printf "0" > decode
        } else {
                printf "andb(rshift(A2,%d),%d)", low,
        			(2**(min(15,high)-low+1)-1) > decode
        }
        printf " + " > decode
        if (high < 16) {
        	printf "0\n" > decode
        } else {
                printf "rshift(andb(A1,%d),%s)\n", (2**(high-16+1)-1),
        			mlnumber(low - 16) > decode
        }
        if (signed) {
        	printf "in if n < %d then n else n - %d\nend\n",
        		2**(high-low), 2**(high-low+1) > decode
        }


}
# <statements>=
# 
# Setup is as before.

NF == 1 && $1 == "instructions" {
	startline = NR
	instructions = 1
	next
}
# <statements>=
instructions && $0 !~ /^#/ {
	opname = $1

	# <compute string displayed when this instruction is decoded>=
        # \paragraph{Decoding instructions}
        # When we've decoded an instruction, we have to display some sort of
        # string representation that tells us what the instruction is.
        # Ideally we should display either just what the assembler expects,
        # or perhaps just what dbx displays when asked about actual instructions
        # in memory images.
        # 
        # For now, we just give the mnemonic for the instruction, followed
        # by a description of each field (followed by a newline).
        # The fields are described as name-value pairs.
        # 
        # We rely on the fact that for a field e.g.\ [[rd]], the string
        # representation of the value of that field is in [[Srd]].
        
        temp = "\"" opname " \""
        for (i=2; i<=NF; i++) {
        	temp = sprintf( "%s ^ \"%s = \" ^ S%s", temp, $i, $i)
        	if (i<NF) temp = sprintf("%s ^ \",\" ", temp)
        }
        displayof[opname]=temp " ^ \"\\n\""

########	gsub("[^a-z']+"," ")   ### ill-advised

	# <compute function that generates this instruction>=
        # 
        # 
        # 
        
        # For each instruction, we define an ML function with the appropriate
        # number of arguments.
        # When that function is given an integer in each argument,
        # it converts the whole thing to one MIPS instruction, represented as an
        # integer pair.
        # 
        # The implementation is a bit of a grubby mess.
        # Doing the fields is straightforward enough, but
        # for each mnemonic we have to do something different based
        # on its type, because each type of opcode goes in a different
        # field.
        # Moreover, for mnemonics of type [[SPECIAL]], [[BCOND]], and [[COP1]] we
        # have to generate [[special]], [[bcond]], and [[cop1]] in the [[op']] field.
        # Finally, we have to do it all twice; once for the high order
        # halfword and once for the low order halfword.
        
        	printf "%s", function_definition(opname, NF-1)
        	printf "("	# open parenthesis for pair
        	for (i=2; i<= NF; i++) {
        		if (!($i in fieldnames)) # <bad field name>=
						 {
						 	print "unknown field", $i, "on line", NR > stderr
						 	next
						 }
        		printf "%sHI(A%d)+", $i, i-1
        	}
        	if (typeof[opname]==OPCODE) {
        		printf "op'HI(%d)", numberof[opname]
        	} else if (typeof[opname]==SPECIAL) {
        		printf "op'HI(%d)+", numberof["special"]
        		printf "functHI(%d)", numberof[opname]
        	} else if (typeof[opname]==BCOND) {
        		printf "op'HI(%d)+", numberof["bcond"]
        		printf "condHI(%d)", numberof[opname]
        	} else if (typeof[opname]==COP1) {
        		printf "op'HI(%d)+", numberof["cop1"]
        		printf "functHI(%d)", numberof[opname]
        	} else # <bad operator name>=
		       # \section{Handling error conditions}
		       # Here are a bunch of uninteresting functions and modules
		       # that handle error conditions.
		       
		       {
		       	print "unknown opcode", opname, "on line", NR > stderr
		       	next
		       }
        	printf ", "
        	for (i=2; i<= NF; i++) {
        		if (!($i in fieldnames)) # <bad field name>=
						 {
						 	print "unknown field", $i, "on line", NR > stderr
						 	next
						 }
        		printf "%sLO(A%d)+", $i, i-1
        	}
        	if (typeof[opname]==OPCODE) {
        		printf "op'LO(%d)", numberof[opname]
        	} else if (typeof[opname]==SPECIAL) {
        		printf "op'LO(%d)+", numberof["special"]
        		printf "functLO(%d)", numberof[opname]
        	} else if (typeof[opname]==BCOND) {
        		printf "op'LO(%d)+", numberof["bcond"]
        		printf "condLO(%d)", numberof[opname]
        	} else if (typeof[opname]==COP1) {
        		printf "op'LO(%d)+", numberof["cop1"]
        		printf "functLO(%d)", numberof[opname]
        	} else # <bad operator name>=
		       # \section{Handling error conditions}
		       # Here are a bunch of uninteresting functions and modules
		       # that handle error conditions.
		       
		       {
		       	print "unknown opcode", opname, "on line", NR > stderr
		       	next
		       }
        	printf ")\n"
}

END {
  # <END>=
  # <write out the definitions of the decoding functions>=
  # The implementation of the decoding function is split into several parts.
  # First, we have to be able to extract any field from an instruction.
  # Then, we have to be able to decode four kinds of opcodes:
  # [[OPCODE]]s, [[BCOND]]s,  [[SPECIAL]]s, and [[COP1]]s.
  # The main function is the one that does ordinary opcodes.
  # The others are auxiliary.
  
  printf "%s", function_definition("decode",2) > decode
  print "let" > decode
    # <write definitions of integer and string representations of each field>=
    # We give each field its own name for an integer version, and its name
    # preceded by [[S]] for its string version.
    # These values are all computed just once, from the arguments to the
    # enclosing function ([[decode]]).
    
    for (f in fieldnames) {
    	printf "val %s = THE%s(A1,A2)\n", f, f  > decode
    	printf "val S%s = Integer.makestring %s\n", f, f  > decode
    }
    # <write expression that decodes the [[funct]] field for [[special]]s>=
    print "val do_special ="  > decode
    print "(case funct of" > decode
    for (code=0; code<256; code++) {
    	name = opcode[SPECIAL,code]
    	# <if [[name]] is known, display a case for it>=
	    # The next three functions are very much of a piece.
	    # They are just enormous [[case]] expressions that match up integers
	    # (bit patterns) to strings.
	    # The fundamental operation is printing out a decimal value and a string
	    # for each opcode:
	    
	    if (name != ""  && name != "reserved") {
	    	# <print space or bar ([[|]])>=
		    # Cases must be separated by vertical bars.
		    # We do the separation by putting a vertical bar before each case except
		    # the first.
		    # We use a hack to discover the first; we assume that code~0 is always
		    # defined, and so it will always be the first.
		    
		    if (code!=0) printf " | "  > decode # hack but it works
		    else printf "   " > decode
	    	disp = displayof[name]
	    	if (disp=="") disp="\"" name "(??? unknown format???)\\n\""
	    	printf "%d => %s\n", code, disp > decode
	    }
    }
    printf " | _ => \"unknown special\\n\"\n" > decode
    print "   ) " > decode
    # <write expression that decodes the [[cond]] field for [[bcond]]s>=
    print "val do_bcond =" > decode
    print "(case cond of" > decode
    for (code=0; code<256; code++) {
    	name = opcode[BCOND,code]
    	# <if [[name]] is known, display a case for it>=
	    # The next three functions are very much of a piece.
	    # They are just enormous [[case]] expressions that match up integers
	    # (bit patterns) to strings.
	    # The fundamental operation is printing out a decimal value and a string
	    # for each opcode:
	    
	    if (name != ""  && name != "reserved") {
	    	# <print space or bar ([[|]])>=
		    # Cases must be separated by vertical bars.
		    # We do the separation by putting a vertical bar before each case except
		    # the first.
		    # We use a hack to discover the first; we assume that code~0 is always
		    # defined, and so it will always be the first.
		    
		    if (code!=0) printf " | "  > decode # hack but it works
		    else printf "   " > decode
	    	disp = displayof[name]
	    	if (disp=="") disp="\"" name "(??? unknown format???)\\n\""
	    	printf "%d => %s\n", code, disp > decode
	    }
    }
    printf " | _ => \"unknown bcond\\n\"\n" > decode
    print "   ) " > decode
    # <write expression that decodes the [[funct]] field for [[cop1]]s>=
    print "val do_cop1 =" > decode
    print "(case funct of" > decode
    for (code=0; code<256; code++) {
    	name = opcode[COP1,code]
    	# <if [[name]] is known, display a case for it>=
	    # The next three functions are very much of a piece.
	    # They are just enormous [[case]] expressions that match up integers
	    # (bit patterns) to strings.
	    # The fundamental operation is printing out a decimal value and a string
	    # for each opcode:
	    
	    if (name != ""  && name != "reserved") {
	    	# <print space or bar ([[|]])>=
		    # Cases must be separated by vertical bars.
		    # We do the separation by putting a vertical bar before each case except
		    # the first.
		    # We use a hack to discover the first; we assume that code~0 is always
		    # defined, and so it will always be the first.
		    
		    if (code!=0) printf " | "  > decode # hack but it works
		    else printf "   " > decode
	    	disp = displayof[name]
	    	if (disp=="") disp="\"" name "(??? unknown format???)\\n\""
	    	printf "%d => %s\n", code, disp > decode
	    }
    }
    printf " | _ => \"unknown cop1\\n\"\n" > decode
    print "   ) " > decode
  print "in" > decode
    # <write [[case]] expression that decodes the [[op']] field for each instruction>=
    # The major expression is a little more complicated, because it has to
    # check for [[special]], [[bcond]], and [[cop1]] and handle those separately.
    
    print "(case op' of" > decode
    for (code=0; code<256; code++) {
    	name = opcode[OPCODE,code]
    	if (name=="special") {
    		# <print space or bar ([[|]])>=
		    # Cases must be separated by vertical bars.
		    # We do the separation by putting a vertical bar before each case except
		    # the first.
		    # We use a hack to discover the first; we assume that code~0 is always
		    # defined, and so it will always be the first.
		    
		    if (code!=0) printf " | "  > decode # hack but it works
		    else printf "   " > decode
    		printf "%d => %s\n", code, "do_special" > decode
    	} else if (name=="bcond") {
    		# <print space or bar ([[|]])>=
		    # Cases must be separated by vertical bars.
		    # We do the separation by putting a vertical bar before each case except
		    # the first.
		    # We use a hack to discover the first; we assume that code~0 is always
		    # defined, and so it will always be the first.
		    
		    if (code!=0) printf " | "  > decode # hack but it works
		    else printf "   " > decode
    		printf "%d => %s\n", code, "do_bcond" > decode
    	} else if (name=="cop1") {
    		# <print space or bar ([[|]])>=
		    # Cases must be separated by vertical bars.
		    # We do the separation by putting a vertical bar before each case except
		    # the first.
		    # We use a hack to discover the first; we assume that code~0 is always
		    # defined, and so it will always be the first.
		    
		    if (code!=0) printf " | "  > decode # hack but it works
		    else printf "   " > decode
    		printf "%d => %s\n", code, "do_cop1" > decode
    	} else # <if [[name]] is known, display a case for it>=
		   # The next three functions are very much of a piece.
		   # They are just enormous [[case]] expressions that match up integers
		   # (bit patterns) to strings.
		   # The fundamental operation is printing out a decimal value and a string
		   # for each opcode:
		   
		   if (name != ""  && name != "reserved") {
		   	# <print space or bar ([[|]])>=
			   # Cases must be separated by vertical bars.
			   # We do the separation by putting a vertical bar before each case except
			   # the first.
			   # We use a hack to discover the first; we assume that code~0 is always
			   # defined, and so it will always be the first.
			   
			   if (code!=0) printf " | "  > decode # hack but it works
			   else printf "   " > decode
		   	disp = displayof[name]
		   	if (disp=="") disp="\"" name "(??? unknown format???)\\n\""
		   	printf "%d => %s\n", code, disp > decode
		   }
    }
    printf " | _ => \"unknown opcode\\n\"\n" > decode
    print "   ) " > decode
  print "end" > decode
  print "end (* Opcodes *)"
  print "end (* Decode *)" > decode
}
