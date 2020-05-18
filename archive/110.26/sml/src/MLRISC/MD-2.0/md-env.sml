(*
 * Machine description environment.
 *)
functor MDEnv(TypeUtils : MD_TYPE_UTILS) : MD_ENV =
struct

   structure Ast   = TypeUtils.Ast
   structure AstPP = TypeUtils.AstPP
   structure Error = MDError

   datatype env = 
      ENV of
        { TE : Ast.ty Env.env,                (* type environment *)
          VE : (Ast.exp * Ast.ty) Env.env,    (* value environment *)
          EE : (Ast.decl list * env) Env.env, (* structure environment *)  
          DE : Ast.decl list,                 (* declarations environment *)
          SE : Ast.decl list                  (* signature environment *)
        }

   open Ast

   infix ++
   infix $$
   infix ==>

   val op $$ = Env.union
   val op ==> = Env.bind
   val O = Env.empty

   val empty = ENV{TE=O, VE=O, EE=O, DE=[], SE=[]}
   fun (ENV{TE=TE1, VE=VE1, EE=EE1, DE=DE1, SE=SE1}) ++ 
       (ENV{TE=TE2, VE=VE2, EE=EE2, DE=DE2, SE=SE2}) =
        ENV{TE=TE1 $$ TE2, VE=VE1 $$ VE2, EE=EE1 $$ EE2, DE=DE1@DE2, SE=SE1@SE2}

   fun mkDECL d     = ENV{TE=O, VE=O, EE=O, DE=[d], SE=[]}
   fun mkSIG  d     = ENV{TE=O, VE=O, EE=O, DE=[], SE=[d]}
   fun mkVALs vbs   = ENV{TE=O, VE=vbs, EE=O, DE=[], SE=[]}

   fun VALbind(id,e,t)= mkVALs(id ==> (e,t))
   fun TYPEbind(id,t) = ENV{TE=id ==> t, VE=O, EE=O, DE=[], SE=[]}
   fun STRbind(id,args,E) = ENV{TE=O, VE=O, EE= id ==> (args,E), DE=[], SE=[]}

   (* Create a new free variable; instantiation and generalization *)
   fun var(ENV _) = TypeUtils.newVar 0 (* XXX *)
   fun inst(ENV _) t = TypeUtils.inst 0 t  (* XXX *)
   fun gen(ENV _) t  = TypeUtils.gen 0 t   (* XXX *)
   fun lambda(ENV _) t  = TypeUtils.lambda 0 t   (* XXX *)

   (* Extract components *)
   fun DE(ENV{DE, ...}) = DE 
   fun SE(ENV{SE, ...}) = SE   
   fun datatypeDefinitions(ENV{DE,...}) =  
   let fun collect(DATATYPEdecl(dbs, _), dbs') = dbs @ dbs'
         | collect(MARKdecl(_, d), dbs') = collect(d, dbs')
         | collect(_, dbs') = dbs'
   in  List.foldr collect [] DE
   end

   (* Lookup components from the environment *) 
   fun lookupTy (E as ENV{TE,EE,...}) (IDENT([],id)) =
       (Env.look TE id
        handle _ => (Error.error("undefined type '"^id^"'"); var E))
     | lookupTy (ENV{EE,...}) (IDENT(s::ss,id)) =
        lookupTy (lookupStr' EE (IDENT(ss,s))) (IDENT([],id))

   and lookupVal' err (E as ENV{VE,EE,...}) (IDENT([],id)) =
        (inst E (Env.look VE id)
        handle _ => (err id; (INTexp 0, var E)))
     | lookupVal' err (ENV{EE,...}) (IDENT(s::ss,id)) =
        lookupVal' err (lookupStr' EE (IDENT(ss,s))) (IDENT([],id))

   and lookupVal E x = lookupVal' 
        (fn x => Error.error("undefined value '"^x^"'")) E x

   and lookupStr (ENV{EE,...}) id = lookupStr' EE id

   and lookupStr' EE (IDENT([],id)) =
       (#2(Env.look EE id)
        handle _ => 
          (Error.error("undefined structure '"^id^"'"); empty))
     | lookupStr' EE (IDENT(s::ss,id)) =
        lookupStr (lookupStr' EE (IDENT(ss,s))) (IDENT([],id))

   (* Interators *)
   fun foldVal f x (ENV{VE, ...}) = 
       Env.fold (fn (id,(e,ty),l) => f(id,e,ty,l)) x VE

   (* 
    * Elaborate a declaration in an environment.
    * Return an environment.
    *)
   fun elab E d = 
   let (* elaborate a declaration *)
       val mkDECL = fn(l,d) => mkDECL(MARKdecl(l,d))
       val mkSIG = fn(l,d) => mkSIG(MARKdecl(l,d))
       fun D E l (d as DATATYPEdecl(dbs,tys)) = mkDECL(l,d) ++ mkSIG(l,d)
         | D E l (d as INSTRUCTIONdecl cbs) = mkDECL(l,d)
         | D E l (d as FUNdecl _) = mkDECL(l,d)
         | D E l (d as RTLdecl _) = mkDECL(l,d)
         | D E l (d as RTLSIGdecl _) = mkDECL(l,d)
         | D E l (d as VALdecl _) = mkDECL(l,d)
         | D E l (d as TYPESIGdecl _) = mkSIG(l,d)
         | D E l (d as VALSIGdecl _) = mkSIG(l,d)
         | D E l (d as LOCALdecl(d1,d2)) = mkDECL(l,d)
           (* let val E' = Ds E l d1 in Ds (E ++ E') l d2 end *)
         | D E l (d as SEQdecl ds) = Ds E l ds
         | D E l (d as OPENdecl ids) = mkDECL(l,d) ++ openStrs E ids
         | D E l (d as STRUCTUREdecl(id,args,DECLsexp ds)) = 
           let val E' = Ds E l ds
           in  STRbind(id,args,E') ++ mkDECL(l,d) end
         | D E l (STRUCTURESIGdecl _) = empty
         | D E l (d as INFIXdecl _) = mkDECL(l,d)
         | D E l (d as INFIXRdecl _) = mkDECL(l,d)
         | D E l (d as NONFIXdecl _) = mkDECL(l,d)
         | D E _ (MARKdecl(l,d)) = (Error.setLoc l; D E l d)
         | D E l _ = Error.fail "unknown declaration"

       and Ds E l [] = empty
         | Ds E l (d::ds) = let val E' = D E l d
                          in  E' ++ Ds (E ++ E') l ds end

           (* open up a list of structures *)
       and openStrs E ids = 
           List.foldr (fn (id,E') => lookupStr E id ++ E') empty ids
        
   in  D E SourceMap.dummyLoc d
   end

   (*
    * Treat a type expression as a pattern and
    * compute its set of bindings.  Duplicated names are assigned 
    * unique suffixes.
    *)
   fun bindingsInType ty = 
   let val names = Env.envir "names"
       fun count id = let val (n,total) = Env.lookup names id
                      in  total := !total + 1 end
                      handle _ => Env.update names (id,(ref 0,ref 1))
       fun getName id = let val (n,total) = Env.lookup names id
                        in  if !total = 1 then id else
                            (n := !n + 1; id^Int.toString(!n))
                        end
       fun f(IDty(IDENT(_,id))) = count id
         | f(APPty(_,[ty])) = f ty
         | f(CELLty id) = count id
         | f(TUPLEty tys) = app f tys
         | f(RECORDty ltys) = app (fn (id,_) => count id) ltys
         | f _ = ()
   in  f ty; (!names,getName) end

   (* Translate a type into a pattern expression *)

   fun ident("", id) = IDENT([], id)
     | ident(prefix, id) = IDENT([prefix], id)

   fun typeToPat ty = 
   let val (env, getName) = bindingsInType ty
       fun g(IDty(IDENT(_,id))) = IDpat(getName id)
         | g(APPty(_,[ty])) = g ty
         | g(CELLty id) = IDpat(getName id)
         | g(TUPLEty tys) = TUPLEpat(map g tys)
         | g(RECORDty ltys) = RECORDpat(map h ltys, false) 
         | g t = Error.fail("Can't translate "^PP.text(AstPP.ty t))
       and h(lab, ty) = (lab, IDpat(getName lab))
   in  g ty end

   fun typeToExp f ty = 
   let val (env, getName) = bindingsInType ty
       fun g(ty as IDty(IDENT(_,id))) = f(getName id,ty)
         | g(APPty(_,[ty])) = g ty
         | g(ty as CELLty id) = f(getName id, ty)
         | g(TUPLEty tys) = TUPLEexp(map g tys)
         | g(RECORDty ltys) = RECORDexp(map h ltys)
         | g t = Error.fail("Can't translate "^PP.text(AstPP.ty t))
       and h(lab, ty) = (lab, f(getName lab, ty))
   in  g ty end


   (* Translate a constructor into a pattern *)
   fun consToPat {prefix,cons=CONSbind{id, ty, ...}} = 
       CONSpat(ident(prefix,id), 
               case ty of NONE => NONE | SOME ty => SOME(typeToPat ty))
   

   (* Translate a constructor into an action clause *)
   fun consToClause {prefix, cons, pat, exp} =
       CLAUSE([pat(consToPat {prefix=prefix,cons=cons})], exp)

   fun consToExp {prefix, cons=CONSbind{id, ty, ...}, exp=f} =
       CONSexp(ident(prefix,id),
               case ty of NONE => NONE | SOME ty => SOME(typeToExp f ty))

   (*
    * Given a constructor, return an environment contains the pattern 
    * variables and its types as bindings. 
    *)
   fun consBindings (CONSbind{ty=NONE, ...}) = empty
     | consBindings (CONSbind{ty=SOME ty, ...}) = 
       let val (_, getName) = bindingsInType ty
           val env = ref Env.empty
           fun enter(id,ty) =
               env :=
                ((!env) $$ (id ==> 
                             (IDexp(IDENT([], (getName id))),ty)))
           fun P ty =
               case ty of
                 IDty(IDENT(_,id)) => enter(id,ty)
               | APPty(_,[ty])     => P ty
               | CELLty id         => enter(id,ty)
               | TUPLEty tys       => app P tys
               | RECORDty ltys     => app enter ltys
               | _                 => ()
       in  P ty; mkVALs (!env) 
       end


   (* Lookup from nested environment *)
   fun declOf(ENV{EE, ...}) id =
       let val (_, ENV{DE,...}) = Env.look EE id 
       in  SEQdecl DE
       end handle _ => $ []

   fun fctArgOf(ENV{EE, ...}) id = 
       let val (args, _) = Env.look EE id
       in  SEQdecl args
       end handle _ => $ []

   fun typeOf(ENV{EE, ...}) id = 
       let val (_, ENV{SE,...}) = Env.look EE id 
       in  SEQdecl SE
       end handle _ => $ []

end
