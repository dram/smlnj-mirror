(* userlevel library redefinitions *)

use "hiouser.sml";
use "hstoreuser.sml";
use "debugxy.sml";

(* userlevel mappings for interface function *)
open System.Control.Debug

val _ = debugging := true

(*local *)

(* shared with DebugCommands: *)
type when = int
type filename = string
type lineno = int
type seqno = int
type location = filename * lineno * seqno
type where = int * int
type wherewhen = where * when
datatype break = NILbreak |
		 TIMEbreak of when |
	         EVENTbreak of location
type value = System.Unsafe.object
type valtype = System.Unsafe.object
datatype valinfo = UNBOUND |
     	           BOUND of value * valtype * wherewhen

(* shared with Interact: *)
datatype debuglevel = NODEBUG | FULLDEBUG of string | LIVEDEBUG of string

val Xuse_file:(debuglevel->string->unit) = !interface 1
val Xuse_stream:(debuglevel->instream->unit) = !interface 2
val XbreakWhen:(when->int) = !interface 3
val XbreakWhere:(location->int) = !interface 4
val XdeleteBreak:(int->bool) = !interface 5
val XgetBreak:(int->break) = !interface 6
val Xcurrent:(unit->wherewhen option) = !interface 7
val Xforwards:(unit->wherewhen option) = !interface 8
val Xbackwards:(unit->wherewhen option) = !interface 9
val XcallTrace:(int->(((wherewhen*wherewhen*(((string*valtype)*value) list)) list) option)) = !interface 10 
val XgetVal:(string->valinfo) = !interface 11
val XprintVal:((value*valtype)->unit) = !interface 12
val XisFn:(valtype->bool) = !interface 13
val XprintBind:((wherewhen*int)->unit) = !interface 14
val Xtoggle:(unit->unit) = !interface 15
(* history store stuff uses interface 16 *)
val XprintAbsyn:((where*int)->unit) = !interface 17
val XgetEvents:((filename*lineno)->(where list)) = !interface 18
val Xreset:(unit->unit) = !interface 19
val Xcomplete:(unit->unit) = !interface 20
val Xabort:(unit->unit) = !interface 21
val Xindebug:(unit->bool) = !interface 22
val Xexception:(unit->exn option) = !interface 23
(* history io stuff uses interface 24 *)
val XeventDesc:(wherewhen->string*(location option)) = !interface 25
val Xdeltas:(int ref * int ref) = System.Unsafe.cast !interface 26

(* Printing utilities *)
fun prWhereWhen (ww as (whr:where,whn:when)) =
  (prWhere ww; print "\t(time "; print whn; print ")")

and prLoc (file:filename,lineno:lineno,seqno:seqno) =
  (print "file \""; print file; print "\" line "; print lineno; 
   if seqno > ~1 then
     (print " # "; print seqno)
   else ())

and prWhere ww =
  let val (s,loc) = XeventDesc ww
  in
     case loc of
       SOME loc => (print (s ^ " at "); prLoc loc)
     | NONE => print s
  end

fun prExn () =
  let val exn = Xexception() 
  in case exn of
       SOME exn => print ("Execution blocked by exception: " ^
			System.exn_name exn ^ "\n")
     | NONE => ()
  end

(*in*)

fun toggle () = Xtoggle ()

fun breakWhen time = XbreakWhen time

fun breakWhere loc = XbreakWhere loc

fun deleteBreak (bn:int) =
  if (not (XdeleteBreak bn)) then
    (print "Error: breakpoint #"; print bn; print " doesn't exist.\n")
  else ()

fun clearBreaks () =
  while (XdeleteBreak 1) do ()

fun showBreaks () =
  let 
    fun p n =
      case (XgetBreak n) of
        TIMEbreak whn => 
		(print n; print "\t"; print "Time "; print whn; print "\n";
		 p (n+1))
        | EVENTbreak loc =>
		(print n; print "\t"; prLoc loc; print "\n"; 
		 p (n+1))
        | NILbreak => ()
  in print "Breakpoints:\n"; 
     p 1
  end

fun showEvents(filename,lineno) =
  let val evl = XgetEvents (filename,lineno) 
      fun p whr =
	let val (s,SOME(_,_,seqno)) = XeventDesc(whr,0)
        in print "# "; print seqno; print ("\t" ^ s ^ "\t");
		XprintAbsyn(whr,8); print "\n"
	end
  in if (null evl) 
     then print "No events at that line number.\n"
     else (print "Events at file \""; print filename; 
	   print "\" line "; print lineno; print " :\n";
	   app p evl)
  end

fun current() = 
   case (Xcurrent()) of
     SOME ww => (print "At "; prWhereWhen ww; print "\n"; prExn())
   | NONE => print "Not executing under debugger\n" 
 
fun forwards() = 
   case (Xforwards()) of
     SOME ww => (print "Stopped at "; prWhereWhen ww; print "\n"; prExn())
   | NONE => print "Not executing under debugger\n" 
 
fun complete() = 
   (Xcomplete(); (* doesn't return if successful *)
    print "Not executing under debugger\n")

fun abort() = 
   (Xabort();    (* doesn't return if successful *)
    print "Not executing under debugger\n")

fun backwards() =
   case (Xbackwards()) of
     SOME ww => (print "Stopped at "; prWhereWhen ww; print "\n")
   | NONE => print "Not executing under debugger\n" 

fun ss () =
  case (Xcurrent()) of
    SOME (_,now) =>
      (XbreakWhen (now+1);
       forwards();
       XdeleteBreak 1;
       now+1)
  | NONE => (print "Not executing under debugger\n"; 0)


fun showCalls maxdepth = 
  case (XcallTrace (max(maxdepth,1)-1)) of
    SOME (top::rest) =>
      let fun prvar ((n:string,t:valtype),v:value) = (print "\t"; print n; print " = "; XprintVal(v,t))
          fun prcall (w as (whr,whn),vw as (vwhr,vwhn),bvlist) = 
	   (prWhereWhen w; print "\n";
            if (whn > 0) then
              (if (vwhn < whn andalso vwhn > 0) then
	         (print "via\t"; prWhereWhen vw; print "\n")
	       else ();
	       print "  bound values:"; app prvar bvlist; print "\n";
	       if (vwhn > 0) then
	         (print "  call: "; XprintBind (vw,8); print "\n")
	       else ())
	    else ())
      in print "At\t"; prcall top;
         app (fn c => (print "From\t"; prcall c)) rest
      end
   | NONE => print "Not executing under debugger\n"

fun showVal n =
  case (XgetVal n) of
    BOUND(v,t,w as (whr,whn)) =>
      if XisFn(t)
      then (print n; print "\tfunction bound by code:\n\t\t";
            XprintBind(w,16); print "\n";
            print "\t\t["; prWhereWhen w; print "]\n")
      else (print n; print "\t"; XprintVal(v,t); 
  	    print "\t["; prWhereWhen w; print "]\n")
  | UNBOUND => (print n; print "\tNot bound\n")


val reset = Xreset

fun usedbg file = if Xindebug() then
		print "Already running under debugger.\n"
	     else Xuse_file (FULLDEBUG "DEBUGX") file
fun uselive file = if Xindebug() then
		print "Already running under debugger.\n"
	     else Xuse_file (LIVEDEBUG "DEBUGX") file
fun usedbg_stream s = if Xindebug() then
		print "Already running under debugger.\n"
	     else Xuse_stream (FULLDEBUG "DEBUGX") s
fun run s = if Xindebug() then
		print "Already running under debugger.\n"
	    else Xuse_stream (LIVEDEBUG "DEBUGX") (open_string s)

(*end*)
