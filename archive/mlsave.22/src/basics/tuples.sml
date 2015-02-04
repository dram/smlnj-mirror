(* tuples.sml *)

structure Tuples : TUPLES = struct

structure Basics = Basics

structure Labels =
  struct datatype labelOpt = NOlabel | SOMElabel of Basics.label
         datatype tyconOpt = NOtycon | SOMEtycon of Basics.tycon
  end

open Labels Basics

structure LabelArray =
    Dynamic (struct
	       type array = Labels.labelOpt Array.array
	       exception Subscript = Array.Subscript
	       type elem = Labels.labelOpt
	       val array = Array.array
	       val op sub = Array.sub
	       val update = Array.update
	       val length = Array.length
	     end)

structure TyconArray =
    Dynamic (struct
	       type array = Labels.tyconOpt Array.array
	       exception Subscript = Array.Subscript
	       type elem = Labels.tyconOpt
	       val array = Array.array
	       val op sub = Array.sub
	       val update = Array.update
	       val length = Array.length
	     end)

fun labelsToSymbol(labels: label list) : Symbol.symbol =
    let fun wrap [] = ["}"]
	  | wrap [id] = [Symbol.name id, "}"]
	  | wrap (id::rest) = Symbol.name id :: "," :: wrap rest
     in Symbols.stringToSymbol(implode("{" :: wrap labels))
    end

val numericLabels = LabelArray.array(NOlabel)
val tupleTycons = TyconArray.array(NOtycon)

fun numlabel i =
    case LabelArray.sub(numericLabels,i)
      of NOlabel =>
	   let val newlabel = Symbols.stringToSymbol(makestring i)
	    in LabelArray.update(numericLabels,i,SOMElabel(newlabel));
	       newlabel
	   end
       | SOMElabel(label) => label

fun numlabels n =
    let fun labels i = 
        if i > n
	  then []
	  else (numlabel i) :: (labels(i+1))
     in labels 1
    end

fun mkRECORDtyc(labels: label list) : tycon =
    let val recordName = labelsToSymbol labels
     in TYCON{stamp = stampBase+Symbol.number(recordName),
	      arity = length labels,
	      eq    = ref YES,
	      path  = [recordName],
	      kind  = RECORDtyc labels}
    end

fun mkTUPLEtyc n =
    case TyconArray.sub(tupleTycons,n)
      of NOtycon =>
           let val tycon = mkRECORDtyc(numlabels n)
	    in TyconArray.update(tupleTycons,n,SOMEtycon(tycon));
	       tycon
	   end
       | SOMEtycon(tycon) => tycon

fun checklabels (n,nil) = true
  | checklabels (n, lab::labs) = 
	Symbol.eq(lab, numlabel n) andalso checklabels(n+1,labs)

fun isTUPLEtyc(TYCON{kind=RECORDtyc labels,...}) = checklabels(1,labels)
  | isTUPLEtyc _ = false

    
end (* structure Tuples *)
