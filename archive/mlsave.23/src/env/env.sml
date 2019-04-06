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
  exception Unbound
  exception Unboundrec
  val debugLook : bool ref
  and debugCollect : bool ref
  and openOld :  info * symtable -> unit
  and openRec : unit -> unit
  and mark : unit -> marker      (* was openNew *)
  and close : marker -> unit
  and openStr : unit -> unit
  and closeStr : unit -> unit
  and collectTable : ((Table.Symbol.symbol*binding)*info -> unit) -> unit
  and splice : marker * marker -> unit
  and add : Table.Symbol.symbol * binding -> unit
  and look : bool * (symtable * Table.Symbol.symbol -> 'a)
	      -> Table.Symbol.symbol -> 'a * info
  and lookLocalRec : bool * (symtable * Table.Symbol.symbol -> 'a) *
		     (symtable * Table.Symbol.symbol -> 'a) option 
		       -> Table.Symbol.symbol -> 'a * info
  and restore : unit -> unit
  and commit : unit -> unit
  and reset : unit -> unit
end

functor EnvFunc (Bind : BIND) : ENV = struct

  structure Table = Table  (* coincidently agrees with Table in BASESTR *)

  local open PrintUtil ErrorMsg Symbol Access
  in

    type binding = Bind.binding
    type info = Bind.info
    type symtable = binding Table.table
    type marker = int

    exception Unbound
          and Unboundrec

    exception Next = Table.Next

    datatype layer
      = CLOSED of info * symtable
      | OPEN of symtable 
      | RECLAYER 
      | STRLAYER
    datatype remark 
      = CLOSEDr 
      | ENTRY of Table.Symbol.symbol
      | RECLAYERr 
      | STRLAYERr
      | MARKER of marker

    fun printLayer(CLOSED _) = prstr "CLOSED"
      | printLayer(OPEN _) = prstr "OPEN"
      | printLayer(RECLAYER) = prstr "RECLAYER"
      | printLayer(STRLAYER) = prstr "STRLAYER"

    fun printRemark(CLOSEDr) = prstr "CLOSEDr"
      | printRemark(ENTRY s) = (prstr "ENTRY("; prstr(Symbol.name s); prstr ")")
      | printRemark(RECLAYERr) = prstr "RECLAYERr"
      | printRemark(STRLAAYERr :: (!remarks))

    fun popRec(RECLAYER::l) = layers := l
      | popRec(_::l) = popRec(l)
      | popRec [] = impossible "popRec"

    fun popStr(STRLAYER::l) = layers := l
      | popStr(_::l) = popStr(l)
      | popStr [] = impossible "popStr"

    fun close (marker) =
	let fun close1 (remark) =
		case remark
		  of ENTRY i =>
		       let val OPEN s ::_ = !layers
			in Table.pop(s,i); true
		       end
		   | CLOSEDr => (popClosed(!layers); true)
		   | RECLAYERr => (popRec(!layers); true)
		   | (!remarks); newline(); printLayers(!layers); newline())

    fun mark () : marker = 
	(inc markCount;
	 remarks := MARKER(!markCount) :: (!remarks);
	 !markCount)

    fun openOld (info: info, table: symtable) : unit =
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

    fun openStr () =
	(layers :=  STRLAYER :: (!layers);
	 remarks := STRLAYERr :: (!remarks))

    fun closeStr () =
	let fun popr (STRLAYERr :: r) = remarks := r
	      | popr (_ :: r) = popr r
	      | popr [] = impossible "closeStr/popr"
	 in popr(!remarks); popStr(!layers)
	end

    fun add (binder as (id,b)) =
	case !layers
	  of OPEN s :: _ =>
	       (Table.add(s,binder); remarks := (ENTRY id)::(!remarks))
	   | lays =>
	       let val s = Table.new() : symtable
	       in Table.add(s,binder);
		  layers := OPEN s :: lays;
		  remarks := ENTRY id :: !remarks
	       end

    fun collectTable (collector) = 
	let fun save rems =
		case rems
		  of ENTRY id :: r =>  
		       let val OPEN s ::_ = !layers
			   val binder = Table.pop(s,id)
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
		   | MARKER s :: r =>
		       (if !debugCollect
			  then (prstr "poping MARKER "; print s; newline())
			  else ();
			save r)
		   | STRLAYERr :: r =>
		       (if !debugCollect
		          then (prstr "structure complete"; newline())
			  else ();
			popStr(!layers);
			remarks := r)
		   | _ => impossible "Env.collectTable"
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

    fun lookLayers (deep,layers,tblSearch,id) =
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
			else (ErrorMsg.flaggedmsg debugLook
			        ("lookLayers failed (struct): "^name id^"\n");
			      raise Unbound))
	      | look1 nil = 
		  (ErrorMsg.flaggedmsg debugLook
		     ("lookLayers failed (global): "^name id^"\n");
		   raise Unbound)
	 in look1(layers)
	end

    fun look (deep,tblSearch) id = 
	lookLayers(deep,!layers,tblSearch,id)

    fun lookLocalRec (deep,tblSearch,tblSearchCont) id = 
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
			    of NONE => raise Unboundrec
			     | SOME tblSearch2 =>
				 lookLayers(deep,rest,tblSearch2,id)
				 handle Unbound
				     => raise Unboundrec)
		     | STRLAYER =>
		         if deep
			 then lookLayers(deep,rest,tblSearch,id)
			 else (ErrorMsg.flaggedmsg debugLook
			        ("lookLocalRec failed (struct): "^name id^"\n");
			       raise Unbound))
	      | look1 nil = 
		  (ErrorMsg.flaggedmsg debugLook
		     ("lookLocalRec failed (global): "^name id^"\n");
		   raise Unbound)
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

end (* EnvFunc *)


structure EnvBind = struct
  type binding = Basics.binding
  type info = int list * Basics.strenv
  val defaultInfo = ref([]:int list,Basics.emptyStrenv)
end

structure Env = EnvFunc(EnvBind)
