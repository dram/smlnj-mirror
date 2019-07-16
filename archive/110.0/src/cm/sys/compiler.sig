(*
 * sys/compiler.sig: signature matching SML/NJ's visual compiler interface
 *
 *   Copyright (c) 1992, 1995 by AT&T Bell Laboratories
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
signature AST = sig

    type fixity
    type symbol
    val infixleft:  int -> fixity
    val infixright: int -> fixity
    type 'a fixitem
    type literal			(* = IntInf.int *)

    type srcpos = int			(* to mark positions in files *)

    datatype 'a sigConst
	= NoSig
      | Transparent of 'a
      | Opaque of 'a

    (* EXPRESSIONS *)

    datatype exp =
	VarExp of symbol list		(* variable *)
      | FnExp of rule list		(* abstraction *)
      | FlatAppExp of { item: exp, region: srcpos * srcpos,
		        fixity: symbol option } list
      | AppExp of { function: exp, argument: exp }
					(* application *)
      | CaseExp of { expr: exp, rules: rule list }
					(* case expression *)
      | LetExp of { dec: dec, expr: exp }
					(* let expression *)
      | SeqExp of exp list		(* sequence of expressions *)
      | IntExp of literal		(* integer *)
      | WordExp of literal		(* word literal *)
      | RealExp of string		(* floating point coded as a string *)
      | StringExp of string		(* string *)
      | CharExp of string		(* char *)
      | RecordExp of (symbol * exp) list
					(* record *)
      | ListExp of exp list		(* list (derived form) *)
      | TupleExp of exp list		(* tuple (derived form) *)
      | SelectorExp of symbol		(* selector of a record field *)
      | ConstraintExp of { expr: exp, constraint: ty }
					(* type constraint *)
      | HandleExp of { expr: exp, rules: rule list }
					(* exception handler *)
      | RaiseExp of exp			(* raise an exception *)
      | IfExp of { test: exp, thenCase: exp, elseCase: exp }
					(* if expression (derived form) *)
      | AndalsoExp of exp * exp		(* andalso (derived form) *)
      | OrelseExp of exp * exp		(* orelse (derived form) *)
      | VectorExp of exp list		(* vector *)
      | WhileExp of { test: exp, expr: exp }
					(* while (derived form) *)
      | MarkExp of exp * (srcpos * srcpos)

    (* RULE for case functions and exception handler *)
    and rule = Rule of { pat: pat, exp: exp }

    (* PATTERN *)
    and pat =
	WildPat				(* empty pattern *)
      | VarPat of symbol list		(* variable pattern *)
      | IntPat of literal		(* integer *)
      | WordPat of literal		(* word literal *)
      | StringPat of string		(* string *)
      | CharPat of string		(* char *)
      | RecordPat of { def: (symbol * pat) list, flexibility: bool }
					(* record *)
      | ListPat of pat list		(* list *)
      | TuplePat of pat list		(* tuple *)
      | FlatAppPat of { item: pat, region: srcpos * srcpos,
		        fixity:symbol option } list
      | AppPat of { constr: pat, argument: pat }
					(* application *)
      | ConstraintPat of { pattern: pat, constraint: ty }
					(* constraint *)
      | LayeredPat of { varPat: pat, expPat: pat }
					(* as expressions *)
      | VectorPat of pat list		(* vector pattern *)
      | MarkPat of pat * (srcpos * srcpos)
      | OrPat of pat list		(* or-pattern *)

    (* STRUCTURE EXPRESSION *)
    and strexp =
	VarStr of symbol list		(* variable structure *)
      | BaseStr of dec			(* defined structure *)
      | ConstrainedStr of strexp * sigexp sigConst (* signature constrained *)
      | AppStr of symbol list * (strexp * bool) list
					(* application (external) *)
      | AppStrI of symbol list * (strexp * bool) list
					(* application (internal) *)
      | LetStr of dec * strexp		(* let in structure *)
      | MarkStr of strexp * (srcpos * srcpos)

    (* FUNCTOR EXPRESSION *)
    and fctexp =
	VarFct of symbol list * fsigexp sigConst
					(* functor variable *)
      | BaseFct of  {			(* definition of a functor *)
		    params:     (symbol option * sigexp) list,
		    body:       strexp,
		    constraint: sigexp sigConst
		   }
      | LetFct of dec * fctexp
      | AppFct of symbol list * (strexp * bool) list * fsigexp sigConst
					(* application *)
      | MarkFct of fctexp * (srcpos * srcpos)

    (* WHERE SPEC *)
    and wherespec =
	WhType of symbol list * tyvar list * ty
      | WhStruct of symbol list * symbol list

    (* SIGNATURE EXPRESSION *)
    and sigexp =
	VarSig of symbol		(* signature variable *)
      | AugSig of sigexp * wherespec list (* sig augmented with where spec *)
      | BaseSig of spec list		(* defined signature *)
      | MarkSig of sigexp * (srcpos * srcpos)

    (* FUNCTOR SIGNATURE EXPRESSION *)
    and fsigexp =
	VarFsig of symbol		(* funsig variable *)
      | BaseFsig of {param: (symbol option * sigexp) list, result:sigexp}
					(* defined funsig *)
      | MarkFsig of fsigexp * (srcpos * srcpos)

    (* SPECIFICATION FOR SIGNATURE DEFINITIONS *)
    and spec =
	StrSpec of (symbol * sigexp * symbol list option) list
					(* structure *)
      | TycSpec of ((symbol * tyvar list * ty option) list * bool)
      | FctSpec of (symbol * fsigexp) list
					(* functor *)
      | ValSpec of (symbol * ty) list	(* value *)
      | DataSpec of { datatycs: db list, withtycs: tb list }
					(* datatype *)
      | ExceSpec of (symbol * ty option) list
					(* exception *)
      | FixSpec of  { fixity: fixity, ops: symbol list }
					(* fixity *)
      | ShareStrSpec of symbol list list (* structure sharing *)
      | ShareTycSpec of symbol list list (* type sharing *)
      | IncludeSpec of sigexp		(* include specif *)
      | MarkSpec of spec * (srcpos * srcpos)

    (* DECLARATIONS (let and structure) *)
    and dec =
 	ValDec of (vb list * tyvar list) (* values *)
      | ValrecDec of (rvb list * tyvar list) (* recursive values *)
      | FunDec of (fb list * tyvar list) (* recurs functions *)
      | TypeDec of tb list		(* type dec *)
      | DatatypeDec of { datatycs: db list, withtycs: tb list }
					(* datatype dec *)
      | AbstypeDec of { abstycs: db list, withtycs: tb list, body: dec }
					(* abstract type *)
      | ExceptionDec of eb list		(* exception *)
      | StrDec of strb list		(* structure *)
      | AbsDec of strb list		(* abstract struct *)
      | FctDec of fctb list		(* functor *)
      | SigDec of sigb list		(* signature *)
      | FsigDec of fsigb list		(* funsig *)
      | LocalDec of dec * dec		(* local dec *)
      | SeqDec of dec list		(* sequence of dec *)
      | OpenDec of symbol list list	(* open structures *)
      | OvldDec of symbol * ty * exp list
					(* overloading (internal) *)
      | FixDec of { fixity: fixity, ops: symbol list }
					(* fixity *)
      | ImportDec of string list	(* import (unused) *)
      | MarkDec of dec * (srcpos * srcpos)

  (* VALUE BINDINGS *)
    and vb =
	Vb of {pat:pat, exp:exp}
      | LVb of {pat:pat, exp:exp}
      | MarkVb of vb * (srcpos * srcpos)

    (* RECURSIVE VALUE BINDINGS *)
    and rvb =
	Rvb of { var:symbol, exp:exp, resultty: ty option,
		 fixity: (symbol * (srcpos*srcpos)) option}
      | LRvb of { var:symbol, exp:exp, resultty: ty option,
		  fixity: (symbol * (srcpos*srcpos)) option}
      | MarkRvb of rvb * (srcpos * srcpos)

    (* RECURSIVE FUNCTIONS BINDINGS *)
    and fb =
	Fb of clause list
      | LFb of clause list
      | MarkFb of fb * (srcpos * srcpos)

    (* CLAUSE: a definition for a single pattern in a function binding *)
    and clause = Clause of { pats: { item: pat, region: srcpos * srcpos,
				     fixity: symbol option } list, 
			    resultty: ty option, exp:exp }

    (* TYPE BINDING *)
    and tb =
	Tb of { tyc: symbol, def: ty, tyvars: tyvar list }
      | MarkTb of tb * (srcpos * srcpos)

    (* DATATYPE BINDING *)
    and db =
	Db of {tyc : symbol, tyvars : tyvar list, rhs : dbrhs}
      | LDb of {tyc : symbol, tyvars : tyvar list, rhs : dbrhs}
      | MarkDb of db * (srcpos * srcpos)
  
    (* DATATYPE BINDING RIGHT HAND SIDE *)
    and dbrhs =
	Constrs of (symbol * ty option) list
      | Repl of symbol list
 
    (* EXCEPTION BINDING *)
    and eb =
	EbGen of { exn: symbol, etype: ty option }  (* Exception definition *)
      | EbDef of { exn: symbol, edef: symbol list } (* defined by equality *)
      | MarkEb of eb * (srcpos * srcpos)

    (* STRUCTURE BINDING *)
    and strb =
	Strb of { name: symbol, def: strexp, constraint: sigexp sigConst }
      | MarkStrb of strb * (srcpos * srcpos)

    (* FUNCTOR BINDING *)
    and fctb =
	Fctb of { name: symbol, def: fctexp }
      | MarkFctb of fctb * (srcpos * srcpos)

    (* SIGNATURE BINDING *)
    and sigb =
	Sigb of { name: symbol, def: sigexp }
      | MarkSigb of sigb * (srcpos * srcpos)

    (* FUNSIG BINDING *)
    and fsigb =
	Fsigb of { name: symbol, def: fsigexp }
      | MarkFsigb of fsigb * (srcpos * srcpos)

    (* TYPE VARIABLE *)
    and tyvar =
	Tyv of symbol
      | MarkTyv of tyvar * (srcpos * srcpos)

    (* TYPES *)
    and ty = 
	VarTy of tyvar			(* type variable *)
      | ConTy of symbol list * ty list	(* type constructor *)
      | RecordTy of (symbol * ty) list 	(* record *)
      | TupleTy of ty list		(* tuple *)
      | MarkTy of ty * (srcpos * srcpos)
end (* structure Ast *)

signature ENVIRONMENT = sig
    type environment
    type staticEnv
    type dynenv
    type symenv
    type symbol

    val primEnv: staticEnv

    val emptyEnv:         environment
    val staticPart:       environment -> staticEnv
    val dynamicPart:      environment -> dynenv
    val symbolicPart:     environment -> symenv
    val mkenv:            { static: staticEnv,
			    dynamic: dynenv,
			    symbolic: symenv } -> environment
    val layerStatic:      staticEnv * staticEnv -> staticEnv
    val layerSymbolic:    symenv * symenv -> symenv
    val layerEnv:         environment * environment -> environment
    val concatEnv:        environment * environment -> environment
    val filterStaticEnv:  staticEnv * symbol list -> staticEnv
    val filterEnv:        environment * symbol list -> environment
    val catalogEnv:       staticEnv -> symbol list
    val consolidateEnv:   environment -> environment
    val consolidateStatic: staticEnv -> staticEnv
    val consolidateSymbolic: symenv -> symenv

    datatype cmEnv =
	CM_NONE
      | CM_ENV of { look: symbol -> cmEnv,
		    symbols: unit -> symbol list }

    val cmEnvOfModule: staticEnv -> symbol -> cmEnv
end

signature ENVREF = sig
    type staticEnv			(* = StaticEnv.staticEnv *)
    type SCstaticEnv			(* = SCStaticEnv.staticEnv *)
    type environment			(* = Environment.environment *)
    type SCenvironment			(* = SCEnv.Env.environment *)

    type senvref = {get: unit-> staticEnv, set: staticEnv -> unit}
    type envref = {get: unit -> environment, set: environment -> unit}
    type SCenvref = {get: unit -> SCenvironment, set: SCenvironment -> unit}

    val core: senvref
    val topLevel : envref		(* interactive top level env *)
    val pervasive : SCenvref		(* pervasive environment *)
    val unSC : SCenvref -> envref
    val unSCenv : SCenvironment -> environment
    val unSCstaticEnv : SCstaticEnv -> staticEnv
end

signature COMPILER = sig

    structure Control: sig
	structure Print: sig
	    val out:       { say: string -> unit, flush: unit -> unit } ref
	    val say:       string -> unit
	    val flush:     unit -> unit
	    val linewidth: int ref
	end
    end

  structure Ast: AST

  structure Symbol: sig
      type symbol
      datatype namespace =
	  VALspace | TYCspace | SIGspace 
	| STRspace | FCTspace | FIXspace 
	| LABspace | TYVspace | FSIGspace

      val eq: symbol * symbol -> bool
      val symbolCMLt: symbol * symbol -> bool
      val nameSpace: symbol -> namespace
      val name: symbol -> string
      val symbolToString: symbol -> string
      val strSymbol: string -> symbol
      val sigSymbol: string -> symbol
      val fctSymbol: string -> symbol
      val fsigSymbol: string -> symbol
  end

  structure SourceMap: sig
      type charpos = Ast.srcpos
  end

  structure Source: sig
      type inputSource

      val newSource:
	  string * int * TextIO.instream * bool *
	  {
	   consumer: string -> unit,
	   linewidth: int,
	   flush: unit -> unit
	  }
	  -> inputSource

      val closeSource: inputSource -> unit

      val filepos: inputSource -> SourceMap.charpos -> string * int * int
  end

  structure BareEnvironment: ENVIRONMENT
  structure Environment: ENVIRONMENT
  sharing
  type Symbol.symbol = Ast.symbol = Environment.symbol = BareEnvironment.symbol

  structure EnvRef: ENVREF
  sharing type Environment.environment = EnvRef.SCenvironment
  sharing type BareEnvironment.environment = EnvRef.environment
  sharing type Environment.staticEnv = EnvRef.SCstaticEnv
  sharing type BareEnvironment.staticEnv = EnvRef.staticEnv

  structure PersStamps: sig
      type persstamp
      val fromBytes: Word8Vector.vector -> persstamp
      val toBytes: persstamp -> Word8Vector.vector
      val toHex : persstamp -> string
      val compare : persstamp * persstamp -> order
  end

  structure DynamicEnv: sig
      type object
      type dynenv
      exception Unbound
      val look: dynenv -> PersStamps.persstamp -> object
  end

  sharing type DynamicEnv.dynenv = Environment.dynenv

  structure ErrorMsg: sig
      type errors
      val errors: Source.inputSource -> errors
      val anyErrors: errors -> bool
  end

  structure Interact: sig
      val useFile: string -> unit
      val installCompManager:
	  (Ast.dec *
	   { get: unit -> Environment.environment,
	     set: Environment.environment -> unit } *
	   { get: unit -> BareEnvironment.environment,
	     set: BareEnvironment.environment -> unit }
	   -> unit) option
	  -> unit
  end

  structure Compile: sig

      exception Compile of string
      exception TopLevelException of exn
      exception SilentException

      type lvar
      type absyn
      type pid = PersStamps.persstamp
      type lambda
      type obj = Unsafe.Object.object
      type csegments =
	  { c0: Word8Vector.vector,
	    cn: Word8Vector.vector list,
	    name: string option ref }

      type compInfo

      val mkCompInfo:
	  Source.inputSource * BareEnvironment.staticEnv * (absyn -> absyn)
	  -> compInfo

      val parse: Source.inputSource -> Ast.dec

      val parseOne:
	  Source.inputSource -> unit -> Ast.dec option

      val elaborate:
	  {
	   compInfo: compInfo,
	   compenv: Environment.staticEnv,
	   ast: Ast.dec
	  } ->
	  {
	   absyn: absyn, 
	   newenv: Environment.staticEnv,
	   exportLvars: lvar list,
	   exportPid: pid option,
	   staticPid: pid,
	   pickle: Word8Vector.vector
	  }

      val makePid: Environment.staticEnv * Environment.staticEnv -> pid

      val instrument:
	  {
	   source: Source.inputSource,
	   compInfo: compInfo,
	   compenv: Environment.staticEnv
	  } ->
	  absyn -> absyn
		
      val translate:
	  {
	   compInfo: compInfo,
	   absyn: absyn,
	   exportLvars: lvar list,
	   exportPid: pid option,
	   oldstatenv: Environment.staticEnv,
	   newstatenv: Environment.staticEnv
	  } ->
	  {
	   genLambda: lambda option list -> lambda,
	   imports: pid list
	  }

      val symDelta: pid option * lambda option
	  -> Environment.symenv

      val inline: { genLambda: lambda option list -> lambda,
		    imports: pid list,
		    symenv: Environment.symenv }
	  -> lambda

      val split: { lambda: lambda, enable: bool }
 	  -> { lambda_e: lambda, lambda_i: lambda option }

      val architecture : string

      val codegen: { compInfo: compInfo, lambda: lambda } -> csegments

      val applyCode: csegments -> obj vector -> obj

      (* the functions above raise ONLY the exception Compile;
       execute can raise other exceptions *)

      val execute:
	  {
	   executable: obj vector -> obj,
	   imports: pid list,
	   exportPid: pid option,
	   dynenv: Environment.dynenv
	  } ->
	  Environment.dynenv		(* new "delta" dynEnv *)
  end

  structure CUnitUtil: sig
      exception FormatError
      exception NoCodeBug

      type 'iid cunit
      type pid = PersStamps.persstamp

      type senv = Environment.staticEnv
      type symenv = Environment.symenv
      type env = Environment.environment

      type obj = Unsafe.Object.object

      val readUnit: { name: string,
		      stream: BinIO.instream,
		      pids2iid: pid list -> 'iid,
		      senv: senv,
		      keep_code: bool }
	  -> 'iid cunit

      val writeUnit: { stream: BinIO.outstream,
		       cunit: 'iid cunit,
		       keep_code: bool,
		       iid2pids: 'iid -> pid list }
	  -> unit
   
      val makeUnit: { imports: pid list,
		      exportPid: pid option,
                      references: 'iid,

                      staticPid: pid,
                      newenv: senv,
		      newenvPickle: Word8Vector.vector,

		      lambda_i: Compile.lambda option,
                      code: Compile.csegments } -> 'iid cunit

      val staticPidCU: 'iid cunit -> pid
      val lambdaPidCU: 'iid cunit -> pid
      val senvCU: 'iid cunit -> senv
      val symenvCU: 'iid cunit -> symenv
      val envCU: 'iid cunit -> env option ref
      val importsCU: 'iid cunit -> pid list
      val exportCU: 'iid cunit -> pid option
      val referencesCU: 'iid cunit -> 'iid

      val nocodeCU: 'iid cunit -> bool
  
      val codeClosure: 'iid cunit -> obj vector -> obj
      val discardCode: 'iid cunit -> unit
  end

  val architecture: string
  val version: { system: string, version_id: int list, date: string }
  val banner: string

end
