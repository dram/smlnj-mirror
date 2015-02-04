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

fun hashLabels(labels: label list) : int =
(* OPT -- this conses up a big hairy string for every record expression *)
    let fun labelToString(id) = Symbol.name(id) ^ ","
     in Symbol.number(
	  Symbols.stringToSymbol(fold (op ^) (map labelToString labels) ""))
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

val recordName = Symbols.stringToSymbol ""

fun mkRECORDtyc(labels: label list) : tycon =
    TYCON{stamp = stampBase+hashLabels(labels),
	  name = recordName,
	  arity = length labels,
	  home = NONE,
	  kind = RECORDtyc labels}

fun mkTUPLEtyc n =
    case TyconArray.sub(tupleTycons,n)
      of NOtycon =>
           let val tycon = mkRECORDtyc(numlabels n)
	    in TyconArray.update(tupleTycons,n,SOMEtycon(tycon));
	       tycon
	   end
       | SOMEtycon(tycon) => tycon

val first = numlabel 1

fun checklabels (n,nil) = true
  | checklabels (n, lab::labs) = 
	Symbol.eq(lab, numlabel n) andalso checklabels(n+1,labs)

fun isTUPLEtyc(TYCON{kind=RECORDtyc labels,...}) =  checklabels(1,labels)
  | isTUPLEtyc _ = false

    
end (* structure Tuples *)
