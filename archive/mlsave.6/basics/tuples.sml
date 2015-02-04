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
	       exceptionx subscript = Array.subscript
	       type elem = Labels.labelOpt
	       val array = Array.array
	       val op sub = Array.sub
	       val update = Array.update
	       val length = Array.length
	     end)

structure TyconArray =
    Dynamic (struct
	       type array = Labels.tyconOpt Array.array
	       exceptionx subscript = Array.subscript
	       type elem = Labels.tyconOpt
	       val array = Array.array
	       val op sub = Array.sub
	       val update = Array.update
	       val length = Array.length
	     end)

fun hashLabels(labels: label list) : int =
(* OPT -- this conses up a big hairy string for every record expression *)
    let fun labelToString(id) = Symbol.name(id) ^ "#"
     in Symbol.number(
	  SymbolTable.stringToSymbol(fold (op ^) (map labelToString labels) ""))
    end

val numericLabels = LabelArray.array (NOlabel)
val tupleTycons = TyconArray.array(NOtycon)

fun numlabel i =
    case LabelArray.sub(numericLabels,i)
      of NOlabel =>
	   let val newlabel = SymbolTable.stringToSymbol(makestring i)
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

fun mkTUPLEtyc n =
    case TyconArray.sub(tupleTycons,n)
      of NOtycon =>
           let val labels = numlabels n
	       val tycon = RECORDtyc{stamp=stampBase+hashLabels(labels),
			   		    labels=labels}
	    in TyconArray.update(tupleTycons,n,SOMEtycon(tycon));
	       tycon
	   end
       | SOMEtycon(tycon) => tycon

val first = numlabel 1

fun checklabels (n,nil) = true
  | checklabels (n, lab::labs) = 
	Symbol.eq(lab, numlabel n) andalso checklabels(n+1,labs)

fun isTUPLEtyc(RECORDtyc{labels,...}) =  checklabels(1,labels)
  | isTUPLEtyc _ = false

fun mkRECORDtyc(labels: label list) : tycon =
    RECORDtyc{stamp = stampBase+hashLabels(labels), labels = labels}
    
end (* structure Tuples *)
