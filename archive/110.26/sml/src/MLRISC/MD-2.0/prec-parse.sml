(* A really stupid but working precedence parser *) 

signature PRECEDENCE_PARSER =
sig

   type precedence_stack

   datatype fixity = INFIX of int 
                   | INFIXR of int 
                   | NONFIX 
   datatype 'a token  = ID  of string
                      | EXP of 'a

   val empty : precedence_stack 
   val declare : precedence_stack * string * fixity -> precedence_stack
   val parse   : { stack  : precedence_stack,
                   app    : 'a * 'a -> 'a,
                   tuple  : 'a list -> 'a,
                   id     : string -> 'a,
                   error  :  string * 'a * 'a list -> unit
                 } -> 'a token list -> 'a
end

structure PrecedenceParser : PRECEDENCE_PARSER =
struct


   datatype fixity = INFIX of int 
                   | INFIXR of int 
                   | NONFIX 
   datatype 'a token  = ID of string
                      | EXP of 'a

   type precedence_stack = (string * fixity) list

   val empty = []
   fun declare(stack,id,fixity) = (id,fixity)::stack

   exception Fail

   fun parse {stack,tuple,app,id,error} tokens =
   let fun fixity x =
       let fun f [] = NONFIX
             | f ((y,fix)::S) = if x = y then fix else f S
       in  f stack end

       val toks = map (fn ID x => (id x,fixity x)
                        | EXP e => (e,NONFIX)) tokens

       fun err(msg,y) = error(msg,y,map #1 toks)

       (* Parse with precedence *)
       fun scan(p, [(e,NONFIX)]) = (e, [])
         | scan(p, (f,NONFIX)::(x,NONFIX)::rest) = (* application *)
             scan(p, (app(f,x), NONFIX)::rest)
         | scan(p, (x,NONFIX)::(L as (f,INFIX q)::rest)) =
             if p < q then
                let val (y, L) = scan(q, rest)
                in  scan(p, (app(f, tuple[x,y]), NONFIX)::L) end
             else 
                (x, L)
         | scan(p, (x,NONFIX)::(L as (f, INFIXR q)::rest)) = 
             if p <= q then
                let val (y, L) = scan(q, rest)
                in  scan(p, (app(f, tuple[x,y]), NONFIX)::L) end
             else 
                (x,L)
         | scan(p, (y,_)::_) = (err("infix symbol", y); raise Fail)

       fun scanAll [(e,_)] = e
         | scanAll toks = 
            let val (x,toks) = scan(0,toks)
            in  case toks of
                  [] => x
                | _  => scanAll((x,NONFIX)::toks)
            end
   in  scanAll toks end

end
