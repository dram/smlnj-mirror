signature NORMALIZE =
  sig
     exception Unbound
     type env
     val empty : env
     val add : env * Symbol.symbol * Modules.spath -> env
     val look : env * Modules.spath -> Modules.spath
     val openStr : env * Modules.spath * env * Modules.Signature -> env
   end

structure Normalize : NORMALIZE =
  struct
     open ErrorMsg Modules
     exception Unbound

     (* invariant: symbol list is in *reverse* order *)

     type env = Symbol.symbol list Env.env  
     val empty = Env.empty
     fun look (env,syms) =
       let fun f nil= impossible "Normalize.look"
             | f [h] = rev(Env.look(env,h))
             | f (h :: t) = rev(Env.look(env,h)) @ t
       in f syms handle Env.Unbound => raise Unbound
       end
     fun add (env,sym,fullname) = Env.bind(sym,fullname,env)
     val convertEnv : Modules.spath * Modules.Signature -> env =
	 fn (_,ERROR_SIG) => Env.empty
          | (names,SIG{env,symbols,...}) =>
	       fold (fn (s,env) => add(env,s,s::names)) symbols Env.empty
     fun openStr (bindEnv,spath,baseEnv,sign) =
	   Env.atop(convertEnv(rev(look(bindEnv,spath)),sign),baseEnv)

  end


       


