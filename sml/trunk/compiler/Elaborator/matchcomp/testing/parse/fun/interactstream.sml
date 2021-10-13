(* stdinstream.sml *)

(* StdinStream 
 * instreams that obtain their characters from TextIO.stdIn *)

structure InteractStream :> CHAR_STREAM
			    where type source = unit  =
struct

  type instream = GeneratorStream.instream    (* lazy list of characters *)

  type source = unit

  (* initial stream is empty, first call of inputc will force
   * a line to be read from stdIn *)

  (* refs used for buffering input line and for position in buffer *)
  val buffer : string ref = ref ""  (* one line of input *)
  val limit : int ref = ref 0       (* number of chars in buffer *)
  val pos : int ref = ref 0         (* current position in buffer *)

  fun reset() = 
      (buffer := "";
       limit := 0;
       pos := 0)

  fun charGen () : char option = 
      if !pos < !limit
      then (* at least one unused character remains in buffer *)
        SOME(String.sub(!buffer,!pos)) before pos := !pos + 1
      else (* buffer exhaused. attempt to reload from stdIn *)
	(print "# ";  (* print prompt string requesting input *)
	 case TextIO.inputLine TextIO.stdIn
           of NONE => NONE  (* stdIn exhausted *)
            | SOME line =>   (* size line >= 1 *)
               (buffer := line; limit := size line; pos := 1;
                SOME(String.sub(line, 0))))

  fun mkInstream () = (reset(); GeneratorStream.mkInstream charGen)

  val charRdr = GeneratorStream.charRdr

end  (* structure InteractStream *)
