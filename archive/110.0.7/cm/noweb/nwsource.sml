(*#line 35 "noweb/nwsource.nw"*)signature NOWEB_SOURCE = sig
  type class = string
  val registerLineNumbering : class * string option -> string option
     (* returns old numbering *)
  val alwaysOverwrite : bool ref
end
(*#line 43 "noweb/nwsource.nw"*)functor NowebSourceFun (structure Tools: CMTOOLS) : NOWEB_SOURCE = struct
  local
    val notangle = "notangle -t"
    val class = "noweb"
    val alwaysOverwrite = ref false

    
(*#line 128 "noweb/nwsource.nw"*)fun split_at name =
  let fun split (pre, #"@" :: post) = (SOME (implode (rev pre)), implode post)
        | split (pre, c    :: post) = split(c::pre, post)
        | split (pre, [])           = (NONE, implode (rev pre))
  in  split([], explode name)
  end
(*#line 136 "noweb/nwsource.nw"*)fun remove_extension (name, ext') =
  let val {base, ext} = OS.Path.splitBaseExt name
  in  if ext = SOME ext' then base else name
  end
(*#line 50 "noweb/nwsource.nw"*)    fun rule name =
      let val (pre, post) = split_at name
          val base = remove_extension(post, "nw")
          fun join ext = OS.Path.joinBaseExt{base=base, ext=SOME ext}
      in  case pre
            of NONE => [(join "sml", SOME "sml"), (join "sig", SOME "sml")]
             | SOME prefix => [(prefix, NONE)]
      end
  
    fun validator {source, targets} = 
      Tools.stdTStampValidator {source = #2 (split_at source), targets=targets}
  
    
(*#line 112 "noweb/nwsource.nw"*)local
  val lineNumberings = ref [("sml", SOME "(*#line %L \"%F\"*)")]
in
  fun lineNumbering class =
    let fun l [] = NONE
          | l ((c, n)::t) = if class = c then n else l t
    in  l (!lineNumberings)
    end
  fun registerLineNumbering (class, numbering) =
    let val old = lineNumbering class
    in  lineNumberings := (class, numbering) :: !lineNumberings;
        old
    end
end

(*#line 64 "noweb/nwsource.nw"*)    fun extract_file (source, (target, class)) =
      let val (_, srcFile) = split_at source
          val class = case class of SOME c => class
                                  | NONE   => Tools.defaultClassOf target
          val numbering = case class of NONE => NONE | SOME c => lineNumbering c
          val format_option = case numbering
                                of NONE     => ""
                                 | SOME fmt => concat ["-L'", fmt, "'"]
          val overwrite = if !alwaysOverwrite then ">" else "| cpif"
          val cmd = concat [notangle, " ", format_option, " -R'", target, "' ", 
                            srcFile, " ", overwrite, " ", target]
          val _ = app print ["[", cmd, "]\n"]
      in
          if (OS.Process.system cmd) = OS.Process.success then ()
          else (
            OS.FileSys.remove target;
            raise Tools.ToolError { tool = "Noweb", msg = cmd }
          )
      end

    fun processor {source, targets} =
      let fun process target = 
            if validator {source=source, targets=[target]} then 
              ()
            else 
              extract_file(source, target)
      in  app process targets
      end
  
    fun sfx s = Tools.addClassifier 
                      (Tools.stdSfxClassifier { sfx = s, class = class })
  in
    val _ = Tools.addToolClass { class = class,
                                 rule = Tools.dontcare rule,
                                 validator = validator,
                                 processor = processor }
    val _ = sfx "nw"
    val alwaysOverwrite = alwaysOverwrite
    val registerLineNumbering = registerLineNumbering
    type class = string
  end
end
