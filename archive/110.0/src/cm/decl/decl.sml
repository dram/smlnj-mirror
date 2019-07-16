(*
 * decl/decl.sml: caching `MD.decl's in the file system
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor DeclFun (structure Convert: CONVERT
		 structure Control: CONTROL): DECL = struct

    structure MD = Convert.MD
    structure MN = MD.ModuleName
    structure Compiler = Convert.Compiler

    exception InternalError and FormatError

    val openTextOut = AbsPath.openTextOut Control.vsay

    val version = "Decl 8\n"

    fun write_decl (s, d) = let

	(* We are consing up the whole output as a list of strings
	 * before concatenating it to form the final result and
	 * wrinting it out using one single `output' call. *)
	fun w_name (n, r) = let
	    val ns = MN.namespaceOf n
	    val prefix =
		if ns = MN.STRspace then ""
		else if ns = MN.SIGspace then "$"
		     else if ns = MN.FCTspace then "%"
			  else if ns = MN.FSIGspace then "&"
			       else raise InternalError
	in
	    prefix :: (MN.nameOf n) :: "." :: r
	end

	fun w_list w (l, r) = foldr w (";" :: r) l

	fun w_path (p, r) = w_list w_name (MN.mnListOfPath p, r)

	fun w_option w (NONE, r) = "-" :: r
	  | w_option w (SOME x, r) = "+" :: w (x, r)

	fun w_decl (MD.StrDecl l, r) =
	    let
		fun w_item ({ name, def, constraint }, r) =
		    w_name (name,
			    w_strExp (def,
				      w_option w_strExp (constraint, r)))
	    in
		"s" :: w_list w_item (l, r)
	    end
	  | w_decl (MD.FctDecl l, r) = let
		fun w_item ({ name, def }, r) =
		    w_name (name, w_fctExp (def, r))
	    in
		"f" :: w_list w_item (l, r)
	    end
          | w_decl (MD.LocalDecl (x, y), r) = "l" :: w_decl (x, w_decl (y, r))
	  | w_decl (MD.SeqDecl l, r) = "q" :: w_list w_decl (l, r)
 	  | w_decl (MD.OpenDecl l, r) = "o" :: w_list w_strExp (l, r)
	  | w_decl (MD.DeclRef s, r) = "r" :: w_list w_name (MN.makelist s, r)

	and w_strExp (MD.VarStrExp p, r) = "v" :: w_path (p, r)
	  | w_strExp (MD.BaseStrExp d, r) = "s" :: w_decl (d, r)
	  | w_strExp (MD.AppStrExp (p, l), r) =
	    "a" :: w_path (p, w_list w_strExp (l, r))
	  | w_strExp (MD.LetStrExp (d, se), r) =
	    "l" :: w_decl (d, w_strExp (se, r))
	  | w_strExp (MD.AugStrExp (se, s), r) =
	    "g" :: w_strExp (se, w_list w_name (MN.makelist s, r))
 	  | w_strExp (MD.ConStrExp (s1, s2), r) =
 	    "c" :: w_strExp (s1, w_strExp(s2, r))

	and w_fctExp (MD.VarFctExp (p, fe), r) =
	    "v" :: w_path (p, w_option w_fctExp (fe, r))
	  | w_fctExp (MD.BaseFctExp { params, body, constraint }, r) = let
		fun w_item ((mn, se), r) =
		    w_option w_name (mn, w_strExp (se, r))
	    in
		"f" ::
		w_list w_item (params,
			       w_strExp (body,
					 w_option w_strExp (constraint, r)))
	    end
	  | w_fctExp (MD.AppFctExp (p, sel, feo), r) =
	    "a" ::
	    w_path (p, w_list w_strExp (sel, w_option w_fctExp (feo, r)))
	  | w_fctExp (MD.LetFctExp (d, fe), r) =
	    "l" :: w_decl (d, w_fctExp (fe, r))

    in
	TextIO.output (s, concat (version :: w_decl (d, ["\n"])))
    end

    fun complain () = raise FormatError

    fun read_decl s = let

	fun rd () = TextIO.input1 s

	local
	    fun get (ns, first) = let
		fun loop (accu, NONE) = complain ()
		  | loop ([], SOME #".") = complain ()
		  | loop (accu, SOME #".") =
		    MN.create (ns, String.implode (rev accu))
		  | loop (accu, SOME s) = loop (s :: accu, rd ())
	    in
		loop ([], first)
	    end
	in
	    fun r_name (SOME #"$") = get (MN.SIGspace, rd ())
	      | r_name (SOME #"%") = get (MN.FCTspace, rd ())
	      | r_name (SOME #"&") = get (MN.FSIGspace, rd ())
	      | r_name first = get (MN.STRspace, first)
	end

	fun r_list r = let
	    fun loop (accu, NONE) = complain ()
	      | loop (accu, SOME #";") = rev accu
	      | loop (accu, cur) = loop ((r cur) :: accu, rd ())
	in
	    fn first => loop ([], first)
	end

	fun r_path first = MN.pathOfMNList (r_list r_name first)

	fun r_option r (SOME #"-") = NONE
	  | r_option r (SOME #"+") = SOME (r (rd ()))
	  | r_option r _ = complain ()

	fun r_decl (SOME #"s") =
	    let
		fun r_item first = {
				    name = r_name first,
				    def = r_strExp (rd ()),
				    constraint = r_option r_strExp (rd ())
				   }
	    in
		MD.StrDecl (r_list r_item (rd ()))
	    end
	  | r_decl (SOME #"f") =
	    let
		fun r_item first = {
				    name = r_name first,
				    def = r_fctExp (rd ())
				   }
	    in
		MD.FctDecl (r_list r_item (rd ()))
	    end
	  | r_decl (SOME #"l") = MD.LocalDecl (r_decl (rd ()), r_decl (rd ()))
	  | r_decl (SOME #"q") = MD.SeqDecl (r_list r_decl (rd ()))
 	  | r_decl (SOME #"o") = MD.OpenDecl (r_list r_strExp (rd ()))
	  | r_decl (SOME #"r") = MD.DeclRef (MN.makeset (r_list r_name(rd ())))
	  | r_decl _ = complain ()

	and r_strExp (SOME #"v") = MD.VarStrExp (r_path (rd ()))
	  | r_strExp (SOME #"s") = MD.BaseStrExp (r_decl (rd ()))
	  | r_strExp (SOME #"a") =
	    MD.AppStrExp (r_path (rd ()), r_list r_strExp (rd ()))
	  | r_strExp (SOME #"l") =
	    MD.LetStrExp (r_decl (rd ()), r_strExp (rd ()))
	  | r_strExp (SOME #"g") =
	    MD.AugStrExp (r_strExp (rd ()), MN.makeset (r_list r_name (rd ())))
 	  | r_strExp (SOME #"c") =
 	    MD.ConStrExp (r_strExp (rd ()), r_strExp (rd ()))
	  | r_strExp _ = complain ()

	and r_fctExp (SOME #"v") =
	    MD.VarFctExp (r_path(rd()), r_option r_fctExp(rd()))
	  | r_fctExp (SOME #"f") =
	    let
		fun r_param first = (r_option r_name first, r_strExp (rd ()))
	    in
		MD.BaseFctExp {
			       params = r_list r_param (rd ()),
			       body = r_strExp (rd ()),
			       constraint = r_option r_strExp (rd ())
			      }
	    end
	  | r_fctExp (SOME #"a") =
	    MD.AppFctExp (r_path (rd ()),
			  r_list r_strExp (rd ()),
			  r_option r_fctExp (rd ()))
	  | r_fctExp (SOME #"l") =
	    MD.LetFctExp (r_decl (rd ()), r_fctExp (rd ()))
	  | r_fctExp _ = complain ()

	val firstline = TextIO.inputLine s
	val r = if firstline = version then r_decl (rd ()) else complain ()
	val nl = rd ()
    in
	if nl = SOME #"\n" then r else complain ()
    end

    datatype member =
	ENTITY of { name: AbsPath.t, class: string }
      | FILE of { name: AbsPath.t, decl: MD.decl }

    fun write_fname (s, { name, rigid, context }) = let
	fun esc (#"\\", r) = #"\\" :: #"\\" :: r
	  | esc (#"\n", r) = #"\\" :: #"n" :: r
	  | esc (c, r) = c :: r
	val chars = foldr esc [#"\n"] (explode name)
	val chars = (if rigid then #"!" else #"?") :: chars
    in
	TextIO.output (s, implode chars)
    end

    fun read_fname s = let
	fun unesc [] = complain ()
	  | unesc [#"\n"] = []
	  | unesc (#"\\" :: #"\\" :: l) = #"\\" :: unesc l
	  | unesc (#"\\" :: #"n" :: l) = #"\n" :: unesc l
	  | unesc (c :: l) = c :: unesc l
	val process = implode o unesc
    in
	case explode (TextIO.inputLine s) of
	    [] => NONE
	  | #"!" :: line => SOME { name = process line, rigid = true }
	  | #"?" :: line => SOME  { name = process line, rigid = false }
	  | _ => complain ()
    end

    fun write_member s (ENTITY { name, class }) =
	(write_fname (s, AbsPath.spec name);
	 TextIO.output (s, concat ["Class ", class, "\n"]))
      | write_member s (FILE { name, decl }) =
	(write_fname (s, AbsPath.spec name);
	 TextIO.output (s, "Stable\n");
	 write_decl (s, decl))

    fun write_members (s, ml) = app (write_member s) ml

    fun read_member (s, canon) =
	case read_fname s of
	    NONE => NONE
	  | SOME fname => let
		val t = TextIO.inputLine s
		val name = canon fname
	    in
		case t of
		    "Stable\n" =>
			SOME (FILE { name = name, decl = read_decl s })
		  | _ => SOME (ENTITY { name = name,
				        class = substring (t, 6, size t - 7) })
	    end

    fun read_members (s, canon) = let
	fun loop () =
	    case read_member (s, canon) of
		NONE => []
	      | SOME m => m :: loop ()
    in
	loop ()
    end

    fun recover (declfile, sourcetime) =
	if TStamp.earlier (TStamp.modtime declfile, sourcetime) then NONE
	else let
	    val s = AbsPath.openTextIn declfile
	    val r = read_decl s
		handle exn => (TextIO.closeIn s; raise exn)
	in
	    TextIO.closeIn s; SOME r
	end handle _ => NONE

    fun deleteFile name = OS.FileSys.remove name handle _ => ()

    fun create (ast, declfile, isource) = let
	fun warn (s, e) = let
	    val sp = Compiler.Source.filepos isource s
	    val ep = Compiler.Source.filepos isource e
	    fun pos (_, l, c) = concat [Int.toString l, ".", Int.toString c]
	    val loc = concat [#1 sp, ":", pos sp, "-", pos ep]
	in
	    Control.say (concat [loc, ": declaration not tracked by CM\n"])
	end
	val decl = Convert.convert { ast = ast, warn = warn }
    in
	let val s = openTextOut declfile
	in
	    (Interrupt.guarded (fn () => write_decl (s, decl));
	     TextIO.closeOut s;
	     decl)
	    handle exn => (TextIO.closeOut s; raise exn)
	end handle exn => let
	    val declstring = AbsPath.elab declfile
	in
	    deleteFile declstring;
	    Control.say (concat ["[writing ", declstring, " failed]\n"]);
	    case exn of
		Interrupt.Interrupt => raise exn
	      | _ => decl
	end
    end

    fun recover_stable (stablefile, sourcetime, canon) = let
	val stabletime = AbsPath.modTime stablefile
    in
	if Time.< (sourcetime, stabletime) then
	    let
		val s = AbsPath.openTextIn stablefile
		val ml = read_members (s, canon)
		    handle exn => (TextIO.closeIn s; raise exn)
	    in
		TextIO.closeIn s; SOME ml
	    end handle _ => NONE
	else
	    NONE
    end

    fun create_stable (ml, stablefile) = let
	val s = openTextOut stablefile
	val _ = Interrupt.guarded (fn () => write_members (s, ml))
	    handle exn => (TextIO.closeOut s; raise exn)
    in
	TextIO.closeOut s
    end handle exn => let
	val stablestring = AbsPath.elab stablefile
    in
	deleteFile stablestring;
	Control.say (concat ["writing ", stablestring, " failed]\n"]);
	raise exn
    end

end
