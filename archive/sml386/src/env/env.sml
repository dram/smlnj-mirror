(* Copyright 1989 by AT&T Bell Laboratories *)
structure Env : ENV =
struct
  open PrtUtil ErrorMsg Symbol Access (* also uses: IntStrMp, Basics *)
  type binding = Basics.binding

  type info = {path: int list, strenv: Basics.strenv}
  type symtable = binding IntStrMp.intstrmap
  type key = int * string

  fun debugmsg  (msg : string) =
	let val printit = !System.Control.debugLook
	in  if printit then (print msg; print "\n")
	    else ();
	    printit
	end
  val debugCollect = System.Control.debugCollect

  val defaultInfo = {path = []:int list, strenv = Basics.DIR}

  fun name(_,s) = s

  exception Unbound
  exception UnboundTable = System.Unsafe.Assembly.UnboundTable

  datatype env
    = TBL of
       {info: info, table: symtable, prev: env ref, isopen: bool}
    | STR of env ref
    | NIL

  fun prevEnv(TBL{prev,...}) = prev
    | prevEnv(STR prev) = prev
    | prevEnv _ = impossible "prevEnv"

  fun printEnv(TBL{prev,...}) = (print "TBL\n  "; printEnv(!prev))
    | printEnv(STR prev) = (print "STR\n  "; printEnv(!prev))
    | printEnv(NIL) = ()

  fun printEnvs(e) = (print "Env:\n  "; printEnv e)

  fun eqEnv(NIL,NIL) = true
    | eqEnv(STR r, STR r') = r = r'
    | eqEnv(TBL{prev=r,...}, TBL{prev=r',...}) = r = r'
    | eqEnv _ = false

  fun appenv f (newenv,oldenv) =
      let fun loop env =
	  if eqEnv(env,oldenv)
	  then ()
	  else case env
		 of TBL{table,prev=ref env',...} => (IntStrMp.app f table; loop env')
		  | STR(ref env') => loop env'
		  | NIL => ()
       in loop newenv
      end

 (* the global variable containing the current environment *)
  val env = ref(NIL)
  fun current() = !env

  fun closeCurrentNewEnv() =
      case current()
       of TBL{info,table,prev,isopen=true} =>
	    TBL{info=info,table=table,prev=prev,isopen=false}
        | _ => impossible "Env.closeCurrentNewEnv()"

  fun newTable() = IntStrMp.new(32, UnboundTable) : symtable

  fun openOld (info: info, table: symtable) : unit =
      env := TBL{info=info, table=table, prev=ref(!env), isopen=false}

  fun openNew (info: info, table: symtable) : unit =
      env := TBL{info=info, table=table, prev=ref(!env), isopen=true}

  fun openStr () : unit =
      env := STR(ref(!env))

  fun popSTR(STR(ref e)) = e
    | popSTR(TBL{prev=ref e,...}) = popSTR e
    | popSTR NIL = impossible "popSTR"

  fun closeStr () = env := popSTR(!env)

  fun openScope () : env = 
      let val oldenv = !env
       in env := TBL{info=defaultInfo,table=newTable(),prev=ref oldenv,isopen=true};
	  oldenv
      end

  fun resetEnv (e) = env := e

  fun add (binder) =
      case !env
	of TBL{table,isopen=true,...} => IntStrMp.add table binder
	 | e =>
	     let val table = newTable() : symtable
	      in IntStrMp.add table binder;
		 env := TBL{info=defaultInfo,table=table,prev=ref e,isopen=true}
	     end

  fun collectTable (collector) = 
      let fun save (TBL{info,table,prev=ref e,...}) =
		(save e;
		 IntStrMp.app (fn (binder) => collector(binder,info)) table)
            | save (STR(ref e)) = env := e
	    | save _ = impossible "Env.collectTable.save"
       in save(!env)
      end

  fun splice (local':env, in':env) = 
      (* remove bindings between env in' and env local' *)
      prevEnv(in') := local'

  fun lookEnv (e:env, (key: key)) =
      let fun look1 (TBL{info,table,prev=ref e,...}) =
	        ((IntStrMp.map table key, info)
		 handle UnboundTable => look1 e)
	    | look1 (STR(ref e)) = look1 e
	    | look1 NIL = 
		(debugmsg ("lookEnv failed (global): "^name(key)^"\n");
		 raise Unbound)
       in look1(e)
      end

  fun look k = lookEnv (!env, k)

  fun lookStrLocal (key: key) = 
      let fun look1 (TBL{info,table,prev=ref e,...}) =
		     ((IntStrMp.map table key, info)
		       handle UnboundTable => look1 e)
	    | look1 (STR(ref e)) =
		(debugmsg ("lookStrLocal failed (structure): "^name(key)^"\n");
		 raise Unbound)
	    | look1 NIL = 
		(debugmsg ("lookStrLocal failed (global): "^name(key)^"\n");
		 raise Unbound)
       in look1(!env)
      end

  (* environment management for toplevel loop *)
  val restoreEnv : env ref = ref NIL
  fun restore () = resetEnv(!restoreEnv)
  fun commit () = restoreEnv := openScope()
  fun previous () = !restoreEnv

  (* consolidating adjacent open tables *)
  fun consolidateEnv(e as TBL{table,prev=ref(pre),isopen=true,...}) =
        (case pre
	   of TBL{isopen=true,...} =>
		let val ans as (_, adder) = consolidateEnv pre
		 in IntStrMp.app adder table; ans
		end
	    | _ => (e,IntStrMp.add table))
    | consolidateEnv(e) = (e,(fn x => ()))

  fun consolidate () = 
      env := let val (x,_) = consolidateEnv(!env) in x end

  fun foldEnv (f: env * 'a -> 'a) (newenv:env) (oldenv:env) (base:'a) : 'a =
      if eqEnv(newenv,oldenv)
      then base
      else f(newenv, foldEnv f (!(prevEnv newenv)) oldenv base)

  fun collectEnv(newenv, oldenv) = 
      foldEnv (fn (TBL{table,...},acc) =>
		    (IntStrMp.app (IntStrMp.add acc) table; acc)
		| (STR _,acc) => acc
		| (NIL,acc)   => acc
	      ) newenv oldenv (newTable())

  fun reset() = (env := NIL; restoreEnv := NIL)

  fun popModule(oldenv:env): symtable =
      (* Extract prefix of environment above oldenv (corresponding
	 to module bindings).  oldenv will normally be pervasiveEnv.
	 Then reset env to savedEnv. *)
      collectEnv(!env,oldenv)
            
end (* EnvFunc *)
