(*
 * Compile the machine description into an internal digestable form
 *)
functor MDCompile
   (structure Env     : MD_ENV
    structure AstPP   : MD_PP
    structure AstUtil : MD_AST_UTIL
    structure Trans   : MD_TRANS
      sharing Env.Ast = AstPP.Ast = Trans.Ast = AstUtil.Ast
   ) : MD_COMPILE =
struct

   structure Env   = Env
   structure Ast   = Env.Ast
   structure Trans = Trans
   structure Error = MDError
   structure AstPP = AstPP
   structure Util  = AstUtil
      
   open Error Ast Util

   type filename = string

   infix ++
  
   val op ++ = Env.++

   datatype 'a slot = EMPTY of string | SLOT of string * 'a 

   (* machine description *)
   datatype md = MD of 
      {env       : Env.env ref,
       endianess : Ast.endianess slot ref, 
       archKind  : Ast.archKind slot ref,
       asmCase   : Ast.assemblycase slot ref,
       name      : string slot ref,
       filename  : filename, 
       version   : string slot ref,
       cells     : Ast.storagedecl list ref,
       locations : Ast.locbind list ref,
       delaySlots: int slot ref,
       formats   : (int * Ast.formatbind) list ref,
       instructions: Ast.consbind list slot ref,
       debug     : string list ref,
       cpus      : Ast.cpubind list slot ref,
       pipelines : Ast.pipelinebind list slot ref,
       resources : Ast.id list slot ref,
       latencies : Ast.latencybind list slot ref
      }

   fun getSlot(ref(EMPTY name)) = Error.fail(name^" has not been declared") 
     | getSlot(ref(SLOT(_,x))) = x

   fun getSlot'(ref(EMPTY _)) = []
     | getSlot'(ref(SLOT(_,x))) = x

   fun putSlot(s as ref(EMPTY name),x) = s := SLOT(name,x)
     | putSlot(s as ref(SLOT(name,_)),x) =
         Error.error("duplicate declaration of "^name)

   fun putSlot'(s as ref(EMPTY name),x) = s := SLOT(name,x)
     | putSlot'(s as ref(SLOT(name,_)),x) = s := SLOT(name,x)

   (* Extract info from a machine description *)
   fun endianess(MD{endianess, ...}) = getSlot(endianess)
   fun archKind(MD{archKind, ...}) = getSlot(archKind)
   fun asmCase(MD{asmCase, ...}) = getSlot(asmCase)
   fun name(MD{name, ...}) = getSlot(name)
   fun version(MD{version, ...}) = getSlot(version)
   fun filename(MD{filename, ...}) = filename
   fun env(MD{env, ...}) = !env
   fun cells(MD{cells, ...}) = !cells
   fun locations(MD{locations, ...}) = !locations
   fun formats(MD{formats, ...}) = !formats
   fun instructions(MD{instructions, ...}) = getSlot instructions
   fun delayslots(MD{delaySlots=ref(EMPTY _), ...}) = 0
     | delayslots(MD{delaySlots=ref(SLOT(_,x)), ...}) = x
   fun debugging(MD{debug, ...}) x = List.exists (fn x' => x = x') (!debug) 
   fun resources(MD{resources, ...}) = getSlot' resources
   fun latencies(MD{latencies, ...}) = getSlot' latencies
   fun cpus(MD{cpus, ...}) = getSlot' cpus
   fun pipelines(MD{pipelines, ...}) = getSlot' pipelines

   fun cellSets(MD{cells, ...}) =
       ListMergeSort.sort
         (fn (CELLdecl{from=f1, ...}, CELLdecl{from=f2, ...}) => !f1 > !f2)
            (List.filter (fn CELLdecl{cellset=true, alias=NONE, ...} => true
                           | CELLdecl _ => false) (!cells)
         )

   fun cellSetsAliases(MD{cells, ...}) = 
        ListMergeSort.sort
         (fn (CELLdecl{from=f1, ...}, CELLdecl{from=f2, ...}) => !f1 > !f2)
           (List.filter (fn CELLdecl{cellset=true, ...} => true
                          | CELLdecl{alias=SOME _, ...} => true
                          | _ => false) (!cells))

   fun lookupCellKind(MD{cells, ...}) k = 
   let fun loop [] = fail("cellkind "^k^" not found")
         | loop((c as CELLdecl{id, nickname, ...})::cs) =
            if k = id orelse k = nickname then c else loop cs
   in  loop (!cells) end

   fun lookupDatatype(MD{env, ...}) t = 
   let val instrEnv  = Env.lookupStr (!env) (IDENT([],"Instruction"))
       val datatypes = Env.datatypeDefinitions instrEnv
       fun loop [] = fail("datatype "^t^" not found")
         | loop((db as DATATYPEbind{id, ...})::dbs) =
            if t = id then db else loop dbs
   in  loop datatypes end

   fun hasCopyImpl md =
       List.exists(fn CONSbind{id="COPY",ty=SOME(RECORDty fields),...} =>
                      List.exists(fn (id,_) => id = "impl") fields
                    | _ => false
                   ) (instructions md)

   (* Extract info from the environment *)
   val declOf  = Env.declOf o env
   val fctArgOf = Env.fctArgOf o env
   val typeOf = Env.typeOf o env

   (* Simplification *)
   local
      fun NIL f x = x

      fun hasBindings ps = 
      let val bindings = ref false
          fun pat _ (p as IDpat x) = (bindings := true; p) 
            | pat _ p = p
      in  app (fn p => 
            (#pat(Trans.rewrite{pat=pat,decl=NIL,exp=NIL,ty=NIL}) p; ())) ps;
          !bindings
      end

      fun allTheSame [] = true
        | allTheSame (x::xs) = List.all (fn x' => x = x') xs

      exception Don'tApply
 
      fun reduceExp ==> (exp as CASEexp(e,[])) = exp
        | reduceExp ==> (exp as CASEexp(e,allCs as (c as CLAUSE(p1,e1))::cs)) = 
          let fun collect(CLAUSE([p],e),Ps) = 
                  let fun ins [] = [([p],e)]
                        | ins((ps,e')::Ps) = 
                          if e = e' then (p::ps,e)::Ps
                          else (ps,e')::ins Ps
                  in  ins Ps end
              val Ps = foldr collect [] (c::cs)
              fun orPat ps =
                  if List.all (fn WILDpat => true | _ => false) ps then
                     WILDpat
                  else ORpat ps  
              fun join([p],e) = CLAUSE([p],e)
                | join(ps,e)  = 
                  let val xs = map (fn TUPLEpat(p::ps) => (p,ps)
                                          | _ => raise Don'tApply) ps
                      val firstPats = map #1 xs
                      val restPats  = map #2 xs
                  in  if allTheSame (map TUPLEpat restPats) then 
                         CLAUSE([TUPLEpat(orPat firstPats::hd restPats)],e)
                      else raise Don'tApply
                  end handle Dont'Apply => CLAUSE([orPat ps],e)
              val cs = map join (rev Ps)
          in  case cs of
                [CLAUSE([TUPLEpat []],body)] => body
              | [CLAUSE([_],body as LISTexp([],NONE))] => body
              | [CLAUSE([TUPLEpat(ps)],body)] => 
                if hasBindings ps then 
                let fun elimOr(pat as ORpat p) =
                         if hasBindings p then pat else WILDpat
                      | elimOr pat = pat
                in  CASEexp(e,[CLAUSE([TUPLEpat(map elimOr ps)],body)])
                end 
                else body
              | [CLAUSE(ps,body)] => 
                 if hasBindings ps then CASEexp(e,cs) else body
              | _ => CASEexp(e,cs) 
          end
        | reduceExp ==> (exp as IFexp(a,b,c)) = if b = c then b else exp
        | reduceExp ==> e = e

      val simplifier = Trans.rewrite{pat=NIL,decl=NIL,exp=reduceExp,ty=NIL}
   in
      fun simpExp e = #exp simplifier e
      fun simpDecl e = #decl simplifier e

      fun stripMarks d =
      let fun decl ==> (MARKdecl(_,d)) = d
            | decl ==> d = d
      in  #decl (Trans.rewrite{pat=NIL,decl=decl,exp=NIL,ty=NIL}) d end
   end

   (* Compile an AST into a machine description *)

   fun compile(filename, decls) = 
   let val endianess   = ref(EMPTY "endianess")
       val archKind    = ref(EMPTY "architecture")
       val asmCase     = ref(EMPTY "assembly case")
       val name        = ref(EMPTY "module name")
       val version     = ref(EMPTY "version")
       val delaySlots  = ref(EMPTY "delaySlots")
       val instructions= ref(EMPTY "instructions")
       val pipelines   = ref(EMPTY "pipelines")
       val resources   = ref(EMPTY "resources")
       val latencies   = ref(EMPTY "latencies")
       val cpus        = ref(EMPTY "cpus")
       val env         = ref Env.empty
       val cells       = ref []
       val locations   = ref []
       val debug       = ref []
       val formats     = ref []
       val md = MD{env      =env,
                   endianess=endianess,
                   archKind =archKind,
                   asmCase  =asmCase,
                   name     =name,
                   filename =filename,
                   version  =version,
                   cells    =cells,
                   locations=locations,
                   delaySlots=delaySlots,
                   formats  =formats,
                   instructions=instructions,
                   debug    =debug,
                   cpus     =cpus,
                   resources=resources,
                   pipelines=pipelines,
                   latencies=latencies
                  }
       fun decl d = env := ((!env) ++ Env.elab (!env) d)
       fun D d =
           case d of
           (* ML code *)
             DATATYPEdecl _ => decl d
           | FUNdecl _      => decl d
           | VALdecl _      => decl d
           | VALSIGdecl _   => decl d
           | TYPESIGdecl _  => decl d
           | LOCALdecl _    => decl d
           | STRUCTUREdecl _ => decl d
           | INFIXdecl _     => decl d
           | INFIXRdecl _    => decl d
           | NONFIXdecl _    => decl d
           | OPENdecl _      => decl d
           | SEQdecl ds      => Ds ds
           | $ _             => ()
           | MARKdecl(l,d)   => (setLoc l; D d)

           (* MD Gen specific constructions *)
           | FORMATdecl(bits,f) => formats :=  !formats @ 
                                      map (fn f => (bits,f)) f
           | STORAGEdecl d      => cells := !cells @ d 
           | LOCATIONSdecl d    => locations := !locations @ d
           | INSTRUCTIONdecl c  => (putSlot(instructions,c); decl d)
           | ARCHdecl(n,ds)     => (putSlot(name,n); Ds(ds))
           | BITSORDERINGdecl _ => error "bitsordering"
           | ARCHKINDdecl k     => putSlot(archKind, k)
           | ENDIANESSdecl e    => putSlot(endianess, e)
           | NAMEdecl n         => putSlot'(name, n)
           | VERSIONdecl v      => putSlot(version, v)
           | ASSEMBLYCASEdecl c => putSlot(asmCase, c)
           | DELAYSLOTdecl d    => putSlot(delaySlots, d)
           | DEBUGdecl id       => (debug := id :: !debug)
           | PIPELINEdecl p     => putSlot(pipelines, p)
           | CPUdecl c          => putSlot(cpus, c)
           | RESOURCEdecl r     => putSlot(resources, r)
           | LATENCYdecl l      => putSlot(latencies, l)
           | _ => error "compile"

       and Ds [] = ()
         | Ds (d::ds) = (D d; Ds ds)

   in  Error.init();
       Ds decls;
       md 
   end


   (*
    * Code Generation methods
    *)

   type module = string
   type arguments = string list
   type signatureName = string

   infix ++

   val op ++ = PP.++

   val toupper = String.map Char.toUpper
   val tolower = String.map Char.toLower

   fun signame md suffix = toupper(name md)^suffix
   fun strname md suffix = name md^suffix
   fun fctname md suffix = name md^suffix

   fun mkSigCon "" = PP.nop
     | mkSigCon sign = PP.sp ++ PP.! ":" ++ PP.! sign 

   fun mkSig md name body =
       PP.line(PP.! "signature" ++ PP.! (signame md name) ++ PP.! "=") ++
       PP.line(PP.! "sig") ++
       PP.block(AstPP.decls body) ++
       PP.line(PP.! "end") ++ PP.nl

    fun mkFct' md name args sign body =
       PP.line(PP.! "functor" ++ PP.! (fctname md name) ++
               PP.group("(",")") (AstPP.decl args) ++
               mkSigCon sign ++ PP.! "=") ++
       PP.line(PP.! "struct") ++
       PP.block(AstPP.decls body) ++
       PP.line(PP.! "end") ++ PP.nl

   fun mkFct md name args sign body = mkFct' md name ($ args) sign body

   fun mkStr md name sign body =
       PP.line(PP.! "structure" ++ PP.! (strname md name) ++ 
               mkSigCon sign ++ PP.!"=") ++
       PP.line(PP.! "struct") ++
       PP.block(AstPP.decls body) ++
       PP.line(PP.! "end") ++ PP.nl

   fun mkCode body = PP.block(AstPP.decls body)


   fun pathName md module suffix =
       let fun getName m = 
           OS.Path.concat(OS.Path.dir m,tolower(name md)^OS.Path.file m)
           val pathname = OS.Path.concat(
                        OS.Path.dir(filename md),getName(module^suffix)) 
       in  pathname end
 
   (* Emit text into a file *)
   fun outfile md module suffix text =
   if !errorCount > 0 then () else
   let val file = pathName md module suffix
       (* val file = moduleName(module^".sml") *) (* For testing *)
       val oldText =
           let val stream = TextIO.openIn file
           in  TextIO.inputN(stream,1024*1024) before TextIO.closeIn stream
           end handle _ => ""
       val header =
       "(*\n"^
       " * This file was automatically generated by MDGen (v2.0)\n"^
	       " * from the machine description file \""^(filename md)^"\".\n"^
	       (* " * on "^Date.toString(Date.fromTimeLocal(Time.now()))^".\n"^ *)
	       " *)\n\n\n"
       val newText = header^text
   in  if !errorCount = 0 andalso oldText <> newText then
	  let val dir = OS.Path.dir file
	      val _   = OS.FileSys.mkDir dir handle _ => ()
	      val stream = TextIO.openOut file
                 in  print("   Generating module "^file^" ... ");
	      TextIO.output(stream,newText);
	      TextIO.closeOut stream;
	      print("done\n")
	  end
       else ()
   end

   (* Emit code into a file *)
   fun codegen md module code =
   let val newText = PP.text(PP.setmode "code" ++ PP.concat code)
   in  outfile md module ".sml" newText
   end

   (*
    * Generate compile time constants
    *)
   abstype constTable = TABLE of (id * exp) list ref * int ref
   with fun newConstTable()  = TABLE(ref [], ref  0)
        fun const(TABLE(entries, counter)) e = 
        let fun lookup [] = 
                let val name = "TMP"^ Int.toString(!counter)
                in  counter := !counter + 1;
                    entries := (name, e) :: !entries;
                    ID name
                end
              | lookup((x,e')::rest) = if e = e' then ID x else lookup rest
        in  lookup(!entries) end
        fun genConsts(TABLE(entries, _)) = map VAL (rev(!entries))
        fun withConsts f =
        let val tbl    = newConstTable()
            val decl   = f(const tbl)
            val consts = genConsts tbl
        in  case consts of 
               [] => decl
            |  _  => LOCALdecl(consts,[decl])
        end
   end 
end
