(*
 * analysis/imp-exp.sml:
 *   Compute the imports and exports for one SML source file.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor ImpExpFun (structure MD: MODDECL
		   structure Control: CONTROL): IMP_EXP =
  struct

    structure MD = MD
    structure ModuleName = MD.ModuleName
    structure Compiler = ModuleName.Compiler
    structure CBareEnv = Compiler.BareEnvironment
    structure CEnv = Compiler.Environment
    structure Symbol = Compiler.Symbol

    type symbol = Symbol.symbol

    type name = ModuleName.t

    datatype env =
	EMPTY				(* nothing *)
      | FCTENV of { look: name -> value,(* functional environment *)
		    names: unit -> name list }
      | BINDING of (name * value)	(* one variable *)
      | LAYER of env * env		(* layering *)
      | UNKNOWN				(* workaround for global signatures *)
    withtype value = env		(* variables are bound to envs *)

    exception Undefined of name
    and IllegalToplevelOpen
    and InternalError of string

    val say = Control.say

    (* like LAYER, but ignore UNKNOWN layers *)
    fun layer (EMPTY, e) = e
      | layer (UNKNOWN, e) = e
      | layer (e1, e2) = LAYER (e1, e2)

    fun mkBaseLookup senv = let
	fun coerce looker name =
	    case looker (ModuleName.symbolOf name) of
		CBareEnv.CM_NONE => raise Undefined name
	      | CBareEnv.CM_ENV { look, symbols } =>
		    FCTENV { look = coerce look,
			     names = ModuleName.filterSymbols o symbols }
    in
	coerce (CBareEnv.cmEnvOfModule senv)
    end

    fun bindSetOf EMPTY = ModuleName.empty
      | bindSetOf (FCTENV { names, ... }) = ModuleName.makeset (names ())
      | bindSetOf (BINDING (name, _)) = ModuleName.singleton name
      | bindSetOf (LAYER (e1, e2)) =
	ModuleName.union (bindSetOf e1, bindSetOf e2)
      | bindSetOf UNKNOWN = ModuleName.empty

    fun imports (dcl, none, glob, combine, sourcename) = let

	(* look for name in environment:
	 * look: (name -> env * 'info) -> env -> name -> (env * 'info) *)
	fun look otherwise = let
	    fun lk EMPTY name = otherwise name
	      | lk (BINDING (n, v)) name =
		if ModuleName.equal (name, n) then
		    (v, none)
		else
		    otherwise name
	      | lk (FCTENV { look = f, ... }) name =
		((f name, none) handle Undefined name => otherwise name)
	      | lk (LAYER (e1, e2)) name = look (lk e2) e1 name
	      | lk UNKNOWN name = (UNKNOWN, none)
	in
	    lk
	end

	(* lookup_name: name * env -> env * 'info,
	 * resolve undefined names globally *)
	fun lookup_name (name, env) = let
	in
	    look glob env name
	    handle Undefined name =>
		(
		 (*
		  say (sourcename ^
		  ": Undefined " ^
		  (ModuleName.makestring name) ^
		  "\n");
		  *)
		 (UNKNOWN, none))
	end

	(* lookup_path: ModuleName.path * env -> env * 'info
	 * first component is looked up in env, undefined things are
	 * resolved globally; all subsequent components are looked up in
	 * the environment denoted by the current prefix *)
	fun lookup_path (path, env) = let
	    val (e1, il) = lookup_name (ModuleName.pathFirstModule path, env)
	    fun loop (NONE, env) = (env, il)
	      | loop (SOME p, env) = let
		    val(e, _) = look
			(fn n =>
			 (say (sourcename ^
			       ": Undefined " ^
			       (ModuleName.makestring n) ^
			       " in path " ^
			       (ModuleName.nameOfPath path) ^
			       "\n");
			  (UNKNOWN, none)))
			env
			(ModuleName.pathFirstModule p)
		in
		    loop (ModuleName.restOfPath p, e)
		end
	in
	    loop (ModuleName.restOfPath path, e1)
	end

	(* get import information from set of modulenames
	 *
	 * to trigger necessary global lookup operations just
	 * touch each name; collect import information  *)
	fun mns2il (nl, env, il0) = let
	    fun touch (n, il) = let
		val (_, ril) = lookup_name (n, env)
	    in
		combine (ril, il)
	    end
	in
	    ModuleName.fold touch il0 nl
	end

	(*
	 * i_decl: decl * env * ``import list'' -> env * ``import'' list
	 *
	 * i_decl analyzes decl within the context of env
	 * it returns a new env, which contains exactly those bindings
	 * introduced by decl.
	 * The input ``import'' list will be augmented as necessary and
	 * returned to the caller.
	 *)
	fun i_decl (dcl, env, il0) =
	    case dcl of
		MD.StrDecl l => let
		    fun bind ({ name, def, constraint = NONE}, (e, il)) =
			let
			    val (v, il) = i_strExp (def, env, il)
			in
			    (LAYER (BINDING (name, v), e), il)
			end
		      | bind ({ name, def, constraint = SOME c }, (e, il)) =
			let
			    val (_, il) = i_strExp (def, env, il)
			    val (v, il) = i_strExp (c, env, il)
			in
			    (LAYER (BINDING (name, v), e), il)
			end
		in
		    foldl bind (EMPTY, il0) l
		end
	      | MD.FctDecl l => let
		    fun bind ({ name, def }, (e, il)) = let
			val (v, il) = i_fctExp (def, env, il)
		    in
			(LAYER (BINDING (name, v), e), il)
		    end
		in
		    foldl bind (EMPTY, il0) l
		end
	      | MD.LocalDecl (d1, d2) => let
		    (* first gather the local stuff, build a tmp env for
		     * evaluating the body -- do that; throw tmp env away *)
		    val (e1, il) = i_decl (d1, env, il0)
		    val lenv = layer (e1, env)
		    val (e2, il) = i_decl (d2, lenv, il)
		in
		    (e2, il)
		end
	      | MD.SeqDecl l => let
		    (* simultaneously build two envs -- one ``big'' env for
		     * maintaining the env argument for all the i_decl
		     * sub-calls,the other one for keeping track of what's
		     * new *)
		    fun lay (dcl, (small_e, big_e, il)) = let
			val (de, il) = i_decl (dcl, big_e, il)
		    in
			(layer (de, small_e), layer (de, big_e), il)
		    end
		    val (e, _, il) = foldl lay (EMPTY, env, il0) l
		in
		    (e, il)
		end
 	      | MD.OpenDecl sel => let
 		    fun open' (se, (e, il)) = let
			val (oe, il') = i_strExp (se, env, il)
		    in
			(layer (oe, e), il')
		    end
		in
		    foldl open' (EMPTY, il0) sel
		end
	      | MD.DeclRef nl => (EMPTY, mns2il (nl, env, il0))

	and i_strExp (se, env, il) =
	    case se of
		MD.VarStrExp p => let
		    val (e, pil) = lookup_path (p, env)
		in
		    (e, combine (pil, il))
		end
	      | MD.BaseStrExp dcl => i_decl (dcl, env, il)
	      | MD.AppStrExp (p, sel) => let
		    fun touch (se, il) = #2 (i_strExp (se, env, il))
		    val (e, pil) = lookup_path (p, env)
		in
		    (e, combine (pil, foldl touch il sel))
		end
	      | MD.LetStrExp (dcl, se) => let
		    val (e, il) = i_decl (dcl, env, il)
		    val env = layer (e, env)
		in
		    i_strExp (se, env, il)
		end
	      | MD.AugStrExp (se, s) => i_strExp (se, env, mns2il (s, env, il))
 	      | MD.ConStrExp (stre, sige) => let
 		    val (_, il') = i_strExp (stre, env, il)
		in
		    i_strExp (sige, env, il')
 		end

	and i_fctExp (fe, env, il) =
	    case fe of
		MD.VarFctExp (p, NONE) => let
		    val (e, pil) = lookup_path (p, env)
		in
		    (e, combine (pil, il))
		end
	      | MD.VarFctExp (p, SOME fe) => let
		    val (_, pil) = lookup_path (p, env)
		in
		    i_fctExp (fe, env, combine (pil, il))
		end
	      | MD.BaseFctExp { params, body, constraint } => let
		    fun param ((n_opt, se), (e, il)) = let
			val (e1, il) = i_strExp (se, env, il)
			val e = case n_opt of
			    NONE => layer (e1, e)
			  | SOME n => LAYER (BINDING (n, e1), e)
		    in
			(e, il)
		    end
		    val (benv, il) = foldl param (env, il) params
		    val (r as (_, il)) = i_strExp (body, benv, il)
		in
		    case constraint of
			NONE => r
		      | SOME se => i_strExp (se, benv, il)
		end
	      | MD.AppFctExp (p, sel, constraint ) => let
		    fun touch (se, il) = #2 (i_strExp (se, env, il))
		    val (pe, pil) = lookup_path (p, env)
		    val il = combine (pil, (foldl touch il sel))
		in
		    case constraint of
			NONE => (pe, il)
		      | SOME fe => let
			    val (ce, il) = i_fctExp (fe, env, il)
			in
			    (ce, il)
			end
		end
	      | MD.LetFctExp (dcl, fe) => let
		    val (e, il) = i_decl (dcl, env, il)
		    val env = layer (e, env)
		in
		    i_fctExp (fe, env, il)
		end

	(* prepare final result *)
	val (e, i) = i_decl (dcl, EMPTY, none)
	fun get_ext n =
	    #1 (look (fn _ => raise InternalError "ImportExport.get_ext") e n)
    in
	(get_ext, i, fn () => bindSetOf e)
    end

    fun exports d = let
	fun e (MD.StrDecl l, a) = ModuleName.addl (map #name l, a)
	  | e (MD.FctDecl l, a) = ModuleName.addl (map #name l, a)
	  | e (MD.LocalDecl (l, b), a) = e (b, a)
	  | e (MD.SeqDecl l, a) = foldl e a l
	  | e (MD.OpenDecl _, _) = raise IllegalToplevelOpen
	  | e (MD.DeclRef _, a) = a
    in
	e (d, ModuleName.empty)
    end

  end
