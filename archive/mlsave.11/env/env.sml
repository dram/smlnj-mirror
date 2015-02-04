(* env.sml *)

signature BIND = sig 
  type binding
  type info
  val defaultInfo: info ref
end

signature ENV = sig
  structure Table : TABLE
  type binding
  type info
  type symtable
  type marker
  exception Unbound of Table.Symbol.symbol
  exception Unboundrec of Table.Symbol.symbol
  val debugLook : bool ref
  and debugCollect : bool ref
  and openOld :  info * symtable -> unit
  and openRec : unit -> unit
  and mark : unit -> marker      (* was openNew *)
  and openStr : unit -> unit
  and close : marker -> unit
  and collectTable : marker * ((Table.Symbol.symbol*binding)*info -> unit)
		    -> unit
  and splice : marker * marker -> unit
  and add : Table.Symbol.symbol * binding -> unit
  and look : bool * (symtable * Table.Symbol.symbol -> 'a)
	      -> Table.Symbol.symbol -> 'a * info
  and lookLocalRec : (symtable * Table.Symbol.symbol -> 'a) *
		     (symtable * Table.Symbol.symbol -> 'a) option 
		       -> Table.Symbol.symbol -> 'a * info
  and restore : unit -> unit
  and commit : unit -> unit
  and reset : unit -> unit
end

functor NewEnv (Bind : BIND) : ENV = struct

  structure Table = Table  (* coincidently agrees with Table in BASESTR *)

  local
    open PrintUtil ErrorMsg Symbol Access
  in

    type binding = Bind.binding
    type info = Bind.info
    type symtable = binding Table.table
    type marker = int

    exception Unbound of Table.Symbol.symbol
          and Unboundrec of Table.Symbol.symbol

    exception Next = Table.Next

    datatype layer
      = CLOSED of Bind.info * symtable   (*Bind. can be removed when bug is fixed*)
      | OPEN of symtable 
      | RECLAYER 
      | STRLAYER
    datatype remark 
      = CLOSEDr 
      | ENTRY of int 
      | RECLAYERr 
      | STRLAYERr
      | MARKER of marker

    fun printLayer(CLOSED _) = prstr "CLOSED"
      | printLayer(OPEN _) = prstr "OPEN"
      | printLayer(RECLAYER) = prstr "RECLAYER"
      | printLayer(STRLAYER) = prstr "STRLAYER"

    fun printRemark(CLOSEDr) = prstr "CLOSEDr"
      | printRemark(ENTRY n) = (prstr "ENTRY("; print n; prstr ")")
      | printRemark(RECLAYERr) = prstr "RECLAYERr"
      | printRemark(STRLAYERr) = prstr "STRLAYERr"
      | printRemark(MARKER s) = (prstr "MARKER("; print s; prstr ")")

    fun printLayers(l) = (prstr "Layers:\n  "; printSequence "\n  " printLayer l)
    fun printRemarks(r) = (prstr "Remarks:\n  "; 
			   printSequence "\n  " printRemark r)

    val layers = ref nil;
    val remarks = ref nil;
    val markCount = ref 0;

    val debugLook = System.Control.debugLook
    val debugCollect = System.Control.debugCollect

    fun printEnv() =
	(printRemarks(!remarks); newline(); printLayers(!layers); newline())

    fun mark () : marker = 
	(inc markCount;
	 remarks := MARKER(!markCount) :: (!remarks);
	 !markCount)

(* Bind. can be removed when bug is fixed *)
    fun openOld (info: Bind.info, table: symtable) : unit =
	(layers := CLOSED(info,table) :: (!layers);
	 remarks := CLOSEDr :: (!remarks))

    fun popClosed(CLOSED(x)::l) = (layers := l; x)
      | popClosed(_::l) = popClosed(l)
      | popClosed [] = impossible "popClosed"

    fun openRec () = 
	(layers := RECLAYER :: (!layers);
	 remarks := RECLAYERr :: (!remarks))

    fun popRec(RECLAYER::l) = layers := l
      | popRec(_::l) = popRec(l)
      | popRec [] = impossible "popRec"

    fun popStr(STRLAYER::l) = layers := l
      | popStr(_::l) = popStr(l)
      | popStr [] = impossible "popStr"

    fun openStr () =
	(layers :=  STRLAYER :: (!layers);
	 remarks := STRLAYERr :: (!remarks))

    fun close (marker) =
	let fun close1 (remark) =
		case remark
		  of ENTRY i =>
		       let val OPEN s ::_ = !layers
			in Table.pop(s,i); true
		       end
		   | CLOSEDr => (popClosed(!layers); true)
		   | RECLAYERr => (popRec(!layers); true)
		   | STRLAYERr => (popStr(!layers); true)
		   | MARKER s => s <> marker
	    fun loop nil = impossible "marker lost in Env.close"
	      | loop (r as remark::rest) =
		  if close1(remark) then loop rest else r
	 in remarks := loop(!remarks)
	end

    fun add (binder) =
	case !layers
	  of OPEN s :: _ => remarks := ENTRY(Table.add(s,binder))::(!remarks)
	   | lays =>
	       let val s = Table.new()
		   val bucket = Table.add(s,binder)
		in layers := OPEN s :: lays;
		   remarks := ENTRY bucket :: !remarks
	       end

    fun collectTable (marker,collector) = 
	let fun save rems =
		case rems
		  of ENTRY i :: r =>
		       let val OPEN s ::_ = !layers
			   val binder as (id,_) = Table.pop(s,i)
			in save r; 
			   if !debugCollect
			     then (prstr "collecting: "; printSym id; newline())
			     else ();
			   collector(binder,!Bind.defaultInfo)
		       end
		   | CLOSEDr ::r =>
		       let val (info,table) = popClosed(!layers)
		        in save r;
			   if !debugCollect
			     then (prstr "collecting CLOSED"; newline())
			     else ();
			   Table.app(table,(fn (binder) => collector(binder,info)))
		       end
		   | RECLAYERr :: r =>
		       (if !debugCollect
			  then (prstr "poping RECLAYERr"; newline())
			  else ();
			popRec(!layers);
			save r)
		   | l as (MARKER s :: r) =>
		       if s=marker
		         then (if !debugCollect
			         then (prstr "collecTable--marker found"; newline();
				       printRemarks(r); newline();
				       printLayers(!layers); newline())
				 else ();
			       remarks := l)
		         else (if !debugCollect
				 then (prstr "poping MARKER "; print s; newline())
				 else ();
			       save r)
		   | _ => impossible "marker lost in Env.collectTable"
	 in save(!remarks)
	end

    fun splice (local',in') = 
	(* remove bindings between marker in' and marker local' *)
	let fun save rems =
		case rems
		  of ENTRY i :: r =>
		       let val OPEN s ::_ = !layers
			   val binder = Table.pop(s,i)
		        in save r; add binder
		       end
		   | CLOSEDr ::r =>
		       let val ps = popClosed(!layers)
			in save r; openOld ps
		       end
		   | RECLAYERr :: r =>
		       impossible "3483 in env"
		   | MARKER s :: r =>
		       if s=in'
		         then (remarks := r; close local')
		         else save r
		   | _ => impossible "marker lost in Env.splice"
	 in save(!remarks)
	end

   (* OPT: lookup functions below could calculate the bucket index just
      once for a each id *)

    fun lookAux (deep,lays,tblSearch,id) =
	let fun look1 (layer :: rest) =
		(case layer
		   of CLOSED(info,tbl) => 
		        ((tblSearch(tbl,id),info)
			 handle Table.Notfound_Table => look1 rest)
		    | OPEN tbl => 
		        ((tblSearch(tbl,id),!Bind.defaultInfo)
			 handle Table.Notfound_Table => look1 rest)
		    | RECLAYER => look1 rest
		    | STRLAYER =>
		        if deep
			  then look1 rest 
			  else (ErrorMsg.flaggedmsg(debugLook,
			       	  "lookAux failed (struct): "^name id^"\n");
			        raise Unbound id))
	      | look1 nil = 
		  (if !debugLook
		      then (prstr "lookAux failed (global): ";
			    printSym id; newline())
		      else ();
		   raise Unbound id)
	 in look1(lays)
	end

    fun look (deep,tblSearch) id = 
	lookAux(deep,!layers,tblSearch,id)

    fun lookLocalRec (tblSearch,tblSearchCont) id = 
	(* switches searches at reclayer; used for initial var/dcon lookup *)
	let fun look1 (layer :: rest) =
		  (case layer
		    of CLOSED(info,tbl) =>
			 ((tblSearch(tbl,id),info)
			  handle Table.Notfound_Table => look1 rest)
		     | OPEN tbl => 
			 ((tblSearch(tbl,id),!Bind.defaultInfo)
			  handle Table.Notfound_Table => look1 rest)
		     | RECLAYER => 
			 (case tblSearchCont
			    of NONE => raise Unboundrec id
			     | SOME tblSearch2 =>
				 lookAux(false,rest,tblSearch2,id)
				 handle Unbound id
				     => raise Unboundrec id)
		     | STRLAYER =>
			 (if !debugLook
			    then (prstr "lookLocalRec failed (struct): ";
				  printSym id; newline())
			    else ();
			  raise Unbound id))
	      | look1 nil = 
		  (if !debugLook
		      then (prstr "lookLocalRec failed (global): ";
			    printSym id; newline())
		      else ();
		   raise Unbound id)
	 in look1(!layers)
	end

    (* environment management for toplevel interactive loop *)
    val restoreMark : marker ref = ref 0
    fun restore () = close(!restoreMark)
    fun commit () = restoreMark := mark()

    fun reset() =
	(layers := nil;
	 remarks := nil;
	 markCount := 0)   

end (* local *)

end (* NewEnv *)


structure EnvBind = struct
  type binding = Basics.binding
  type info = int list * Basics.strenv
  val defaultInfo = ref([]:int list,{s=arrayoflist[],t=arrayoflist[]})
end

structure Env = NewEnv(EnvBind)
