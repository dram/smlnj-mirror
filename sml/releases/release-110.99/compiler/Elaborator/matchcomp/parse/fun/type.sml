(* type.sml *)

(* type checker for Fun *)

structure Type =
struct

open Syntax

exception TypeError of string

(* type environments *)
type tyenv = ty Env.env

fun typeError (msg: string) = raise TypeError msg

(* We make due with generic structural equality for type expressions.
 * This does not respect alpha-equivalence of recursive types. *)
fun eqType(ty1: ty, ty2: ty) = ty1 = ty2

(* newVar : unit -> variable 
 *  generates a new variable whose name is derived from the base string parameter *)
local
  val count = ref 0  (* a counter; an updateable integer reference *)
  fun next () = !count before count := !count + 1
in fun newTVar () =
       "t." ^ Int.toString(next()) 
       (* concatenate "t." with string representation of next count value *)
end

(* substituted type expressions may contain free type variables,
 * so we have to avoid free variable capture by alpha-converting bound
 * variables during substitution. *)
fun substType(tv, ty, ty1 as VARty tv1) =
     if tv = tv1 then ty else ty1
  | substType(tv,ty,FUNty(t1,t2)) = FUNty(substType(tv,ty,t1),substType(tv,ty,t2))
  | substType(tv,ty,PRODty(t1,t2)) = PRODty(substType(tv,ty,t1),substType(tv,ty,t2))
  | substType(tv,ty,SUMty(t1,t2)) = SUMty(substType(tv,ty,t1),substType(tv,ty,t2))
  | substType(tv,ty,RECty(tv1,ty1)) =
     let val ntv = newTVar()  (* alpha-convert to avoid free variable capture *)
      in RECty(ntv,substType(tv,ty,substType(tv1,VARty ntv,ty1)))
     end
  | substType(tv,ty,POLYty(tv1,ty1)) =
     let val ntv = newTVar()  (* alpha-convert to avoid free variable capture *)
      in POLYty(ntv,substType(tv,ty,substType(tv1,VARty ntv,ty1)))
     end
  | substType(_,_,ty) = ty


(* setting up initial type environment *)

val binArithTy = FUNty(PRODty(INTty,INTty),INTty)  (* type of arithmetic primops *)
val binBoolTy = FUNty(PRODty(INTty,INTty),BOOLty)  (* type of arithmetic primops *)

val arithops = ["+","*","-"]
val relnops = ["==","<",">"]

(* initial type environment, binding primop variables *)
val tenv0 = 
    foldr
      (fn (x,e) => Env.bind(x,binBoolTy,e))
      (foldr (fn (x,e) => Env.bind(x,binArithTy,e)) Env.empty arithops)
      relnops

(* add polymorphic bindings of product projections and sum injections *)
val tyenvInit =
    Env.bind("Fst",POLYty("t1",POLYty("t2",FUNty(PRODty(VARty "t1",VARty "t2"), VARty "t1"))),
    Env.bind("Snd",POLYty("t1",POLYty("t2",FUNty(PRODty(VARty "t1",VARty "t2"), VARty "t2"))),
    Env.bind("Inl",POLYty("t1",POLYty("t2",FUNty(VARty "t1", SUMty(VARty "t1",VARty "t2")))),
    Env.bind("Inr",POLYty("t1",POLYty("t2",FUNty(VARty "t2", SUMty(VARty "t1",VARty "t2")))),
    tenv0))))

(* verifying relative closure of type expressions *)
fun member(tv: variable, tvs) = List.exists (fn tv' => (tv = tv')) tvs

fun relClosed(VARty tv, tvs) = member(tv,tvs)
  | relClosed(FUNty(ty1,ty2), tvs) = relClosed(ty1,tvs) andalso relClosed(ty2,tvs)
  | relClosed(PRODty(ty1,ty2), tvs) = relClosed(ty1,tvs) andalso relClosed(ty2,tvs)
  | relClosed(SUMty(ty1,ty2), tvs) = relClosed(ty1,tvs) andalso relClosed(ty2,tvs)
  | relClosed(RECty(tv,ty), tvs) = relClosed(ty,tv::tvs)
  | relClosed(POLYty(tv,ty), tvs) = relClosed(ty,tv::tvs)
  | relClosed _ = true

(* In type-checking, we only need to check relative closure of types that are explicitly
 * part of the expression as ascriptions or type arguments of TApps. Constructed types
 * will then necessarily be relatively closed. This needs some proof, of course. *)

(* type0: expr * ty Env.env * variable list -> ty
 * the main type checker *)
fun type0 (Var x, tenv, boundtvs) =
    (* ASSERT: type of any variable in scope will be relatively closed wrt boundtvs *)
     (Env.look (x,tenv)
      handle Env.Unbound _ => typeError ("unbound variable: "^x))

  | type0 (Unit, _, _) = UNITty
  | type0 (Num _, _, _) = INTty
  | type0 (Bool _, _, _) = BOOLty

  | type0 (If(e1,e2,e3), tenv, boundtvs) =
     (case type0(e1,tenv,boundtvs)
        of BOOLty => 
	    let val thenType = type0(e2,tenv,boundtvs)
             in if eqType(thenType, type0(e3,tenv,boundtvs))
		then thenType
	        else typeError "If - branches don't match"
	    end
         | _ => typeError "If - condition not boolean")

  | type0 (Let(def,(x,body)), tenv, boundtvs) = 
      type0(body, Env.bind(x,type0(def,tenv,boundtvs),tenv), boundtvs)

  | type0 (Letrec(x,funTy,((y,argTy,defbody),letbody)), tenv, boundtvs) = 
      if relClosed(funTy,boundtvs) andalso relClosed(argTy,boundtvs)
      then (case funTy
             of FUNty(argTy',resTy) =>
		let val defbodyTy = type0(defbody,
					  Env.bind(y,argTy,Env.bind(x,funTy,tenv)),
					  boundtvs)
		 in if eqType(defbodyTy,resTy) andalso eqType(argTy,argTy')
		    then type0(letbody, Env.bind(x,funTy,tenv),boundtvs)
		    else typeError "Letrec - definiens does not agree with ascription"
		end
	      | _ => typeError "Letrec - function ascription not a function type")
      else typeError "Letrec - free type variable(s) in ascription"

  | type0 (Fun(x,xType,e), tenv, boundtvs) =
      if relClosed(xType,boundtvs)
      then FUNty(xType, type0(e, Env.bind(x,xType,tenv), boundtvs))
      else typeError "Fun - free type variable(s) in ascription"

  | type0 (App(e1,e2), tenv, boundtvs) =
      (case type0 (e1,tenv,boundtvs) 
	 of FUNty(argType,retType) => 
	     if eqType(type0(e2,tenv,boundtvs), argType)
	     then retType 
	     else typeError "App: rand does not agree with rator"
	 | _ => typeError "App: rator not of function type")

  | type0 (Pair(e1,e2), tenv, boundtvs) =
      PRODty(type0(e1,tenv,boundtvs), type0(e2,tenv,boundtvs))

  | type0 (Case (e1,(x,e2),(y,e3)), tenv, boundtvs) =
     (case type0 (e1,tenv,boundtvs)
        of SUMty(t1,t2) =>
           let val s1 = type0(e2, Env.bind(x,t1,tenv), boundtvs)
               val s2 = type0(e3, Env.bind(y,t2,tenv), boundtvs)
            in if eqType(s1,s2)
               then s1
               else typeError "Case - branches disagree"
	   end
         | _ => typeError "Case - type of scrutinee not a SUMty")
   
   | type0 (Fold (ty,e), tenv, boundtvs) = 
       if relClosed(ty, boundtvs)
       then let val ety = type0(e,tenv,boundtvs)
             in case ty
		  of RECty(tv,body) =>
		     if eqType(ety, substType(tv,ty,body))
		     then ty
		     else typeError "Fold - arg type not unwinding of rec type"
		   | _ => typeError "Fold - target type not a RECty"
            end
      else typeError "Fold - free type variable(s) in ascription"

   | type0 (Unfold e, tenv, boundtvs) = 
      (case type0(e,tenv,boundtvs)
         of ty as RECty(tv,body) => substType(tv,ty,body)
          | _ => typeError "Unfold - arg not a RECty")

   | type0 (TFun(t,e), tenv, boundtvs) = POLYty(t,type0(e,tenv,t::boundtvs))

   | type0 (TApp(e,ty), tenv, boundtvs) =
      if relClosed(ty, boundtvs)
      then (case type0(e,tenv,boundtvs)
              of POLYty(t,ty1) => substType(t,ty,ty1)
               | _ => typeError "TApp - rator not polymorphic")
      else typeError "TApp - free type variable(s) in type argument"

(* typeOf: top-level type checker with error reporting *)
fun type1 (expr, tyenv) =
    type0(expr, tyenv, [])
    handle TypeError msg => (print("Type Error: "^msg^"\n"); ERRORty)

(* typeStmt : stmt * tyenv -> ty * tyenv *)
fun typeStmt (Expr e, tyenv) =
    (type1(e, tyenv), tyenv)
  | typeStmt (Decl(ValDef(v,e)), tyenv) =
    (case type1(e, tyenv)
       of ERRORty => (ERRORty, tyenv)
        | ty => (ty, Env.bind(v,ty,tyenv)))
  | typeStmt (Decl(FunDef(f,x,argTy,resTy,def)), tyenv) =
    let val decTy = FUNty(argTy,resTy)
	val defTy = type1(def,Env.bind(f,decTy,Env.bind(x,argTy,tyenv)))
     in if eqType(resTy,defTy)
	then (decTy, Env.bind(f, decTy, tyenv))
	else ((print "Type Error: fun - definiens does not agree with ascription";
               ERRORty),
	      tyenv)
    end
  | typeStmt (Quit, tyenv) = (UNITty, tyenv)

end (* structure Type *)
