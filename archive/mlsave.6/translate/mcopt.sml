(* mcopt.sml *)

(*
signature MCopt = sig
  structure Absyn : BAREABSYN
  structure Basics : BASICS
  structure Lambda : LAMBDA
  val opt : ((Absyn.pat list * Lambda.lexp * int) list * Access.lvar list)
	   -> ((Absyn.pat list * Lambda.lexp * int) list * Access.lvar list)  
end
*)

structure MCopt (* : MCopt *) = 
struct

(*structure Absyn = Absyn*)
(*structure Basics = Basics*)
(*structure Lambda : LAMBDA = Lambda*)
open Basics Absyn Lambda
open PrintUtil PrintBasics PrintAbsyn MCprint ErrorMsg

fun combine (head::heads) (relf::relfs) =
		(head::relf)::(combine heads relfs)
  | combine (head::heads) nil = [head]::(combine heads nil)
  | combine nil nil = nil
  | combine _ _ =  impossible "3882 in match optimizer"

(* take a list of record patterns and return the list *)
(* of the tail fields of each record pattern *)
fun tailf ([_]::_) = nil
  | tailf ((hd::tl)::pats) = tl::(tailf pats)
  | tailf nil = nil
  | tailf _ = impossible "348 in match optimizer"

(* take a list of record patterns and return the list *)
(* of the first field of each record pattern *)
fun headf ((hd::tl)::pats) = hd::(headf pats)
  | headf nil = nil
  | headf _ = impossible "387 in match optimizer"


fun within (APPpat (DATACON{name=r1,...},_),plist) =
	let fun	app_inside (APPpat (DATACON{name=r2,...},_)::more) =
			if Symbol.eq(r1,r2) then true
			else app_inside more
	      | app_inside (CONpat _::more) = app_inside more
	      | app_inside (VARpat _::more) = app_inside more
	      | app_inside (WILDpat::more) = app_inside more
	      | app_inside (LAYEREDpat (_,p)::more) = app_inside (p::more)
	      | app_inside (CONSTRAINTpat (p,_)::more) = app_inside (p::more)
	      | app_inside nil = false
	      | app_inside _ = impossible "77 in match optimizer"
	in
		app_inside plist
	end

  | within (CONpat (DATACON{name=r1,...}),plist) =
	let fun con_inside (CONpat(DATACON{name=r2,...})::more) =
			if Symbol.eq(r1,r2) then true
			else con_inside more
	      | con_inside (APPpat _::more) = con_inside more
	      | con_inside (VARpat _::more) = con_inside more
	      | con_inside (WILDpat::more) = con_inside more
	      | con_inside (LAYEREDpat (_,p)::more) = con_inside (p::more)
	      | con_inside (CONSTRAINTpat (p,_)::more) = con_inside (p::more)
	      | con_inside nil = false
	      | con_inside _ =impossible "3 in match optimizer"
	in
		con_inside plist
	end

  | within (INTpat i,plist) = 
	let fun	int_inside (INTpat j::more) =
			if i=j then true
			else int_inside more
	      | int_inside (VARpat _::more) = int_inside more
	      | int_inside (WILDpat::more) = int_inside more
	      | int_inside (LAYEREDpat (_,p)::more) = int_inside (p::more)
	      | int_inside (CONSTRAINTpat (p,_)::more) = int_inside (p::more)
	      | int_inside nil = false
	      | int_inside _ =impossible "387 in match optimizer"
	in
		int_inside plist
	end

  | within (REALpat r,plist) = 
	let fun	real_inside (REALpat s::more) =
			if r=s then true
			else real_inside more
	      | real_inside (VARpat _::more) = real_inside more
	      | real_inside (WILDpat::more) = real_inside more
	      | real_inside (LAYEREDpat (_,p)::more) =
				real_inside (p::more)
	      | real_inside (CONSTRAINTpat (p,_)::more) =
				real_inside(p::more)
	      | real_inside nil = false
	      | real_inside _ =impossible "3721 in match optimizer"
	in
		real_inside plist
	end

  | within (STRINGpat s,plist) = 
	let fun	string_inside (STRINGpat t::more) =
			if s=t then true
			else string_inside more
	      | string_inside (VARpat _::more) = string_inside more
	      | string_inside (WILDpat::more) = string_inside more
	      | string_inside (LAYEREDpat(_,p)::more) =
				string_inside(p::more)
	      | string_inside (CONSTRAINTpat(p,_)::more) =
				string_inside(p::more)
	      | string_inside nil = false
	      | string_inside _ =impossible "66 in match optimizer"
	in
		string_inside plist
	end

  | within (VARpat _,_) = true
  | within (WILDpat,_) = true
  | within (LAYEREDpat (_,p), plist) = within (p,plist)
  | within (CONSTRAINTpat (p,_), plist) = within (p,plist)
  | within _ = impossible "113 in match optimizer"

infix within


fun	  unique (head::tail) =
		let val utail = unique tail
		in
			if (head within utail)
			then utail
			else head::utail
		end
	| unique nil = nil

fun branch_factor fs = length (unique (headf fs))


fun arity_factor ((head::_)::_) = 
	let fun arity (INTpat _) = 1
	      | arity (REALpat _) = 1
	      | arity (STRINGpat _) = 1
	      | arity (VARpat _) = 0
	      | arity WILDpat = 0
	      | arity (RECORDpat{pats,...}) = length (!pats)
	      | arity (APPpat (_,p)) = 1 + arity p
	      | arity (CONpat _) = 1
	      | arity (LAYEREDpat (_,p)) = arity p
	      | arity (CONSTRAINTpat (p,_)) = arity p
	 in
		arity head
	end

  | arity_factor _ = impossible "48 in match compiler"

exceptionx record

fun relevant (VARpat _) = false
  | relevant WILDpat = false
		(* any var always matches so never relevant *)
  | relevant (RECORDpat{pats=ref nil,...}) = false
		(* unit always matches so never relevant *)
  | relevant (RECORDpat _) = raisex record
		(* otherwise, immediately expand records *)
  | relevant (LAYEREDpat (_,p)) = relevant p
  | relevant (CONSTRAINTpat (p,_)) = relevant p
  | relevant (CONpat(DATACON{dcons = ref [_],...})) = false
		(* if only one data constructor, no need to test *)
  | relevant (APPpat(DATACON{dcons = ref [_],...},pat)) =
		relevant pat
  | relevant _ = true
		(* everything else is relevant *)
	



fun	  relfactor nil nil = (nil,nil,nil,nil)

	(* a record should be immediately expanded by mcand so that
	   the nested fields can be considered as well;
	   don't bother to look at the rest of the fields,
	   and leave the record at the end of relf.
	   otherwise, just check the relevance.
	*)
	| relfactor (arg as ((head::_)::_)) (x::xs) =
		let val (relf,relx,irrelf,irrelx) =
			relfactor (tailf arg) xs
		in
		(case (relevant head) of
		  true =>
			(combine (headf arg) relf, x::relx,irrelf,irrelx)
		| false =>
			(relf,relx,combine (headf arg) irrelf,x::irrelx)
		) handlex record =>
			(combine (headf arg) nil,[x],tailf arg,xs)
		end

	| relfactor a b = impossible ("relfactor: "^makestring(length(hd a))^" "^
makestring(length b))



fun	  branch (argp as [_]::_) (argx as [_]) = (argp,argx,nil,nil)
	| branch (argp as ((_::_)::_)) (x::xs) =
		let val head = headf argp
		and tail = tailf argp;
		val (bestf,bestx,otherfs,otherxs) = branch tail xs;
		val bfnew = branch_factor argp
		and bfold = branch_factor bestf
		in
			if bfnew < bfold then (combine head nil,[x],tail,xs)
			else	if bfnew > bfold
				then (bestf,bestx,combine head otherfs,x::otherxs)
			else
				(combine head bestf,x::bestx,otherfs,otherxs)
		end
	| branch nil nil = (nil,nil,nil,nil)
	| branch _ _ = impossible "84 in mcopt.branch:  #fields and #x's different."


fun	  arity (argp as [_]::_) (argx as [_]) = (argp,argx,nil,nil)
	| arity (argp as ((_::_)::_)) (x::xs) =
		let val head = headf argp
		and tail = tailf argp;
		val (bestf,bestx,otherfs,otherxs) = arity tail xs;
		val anew = arity_factor argp
		and aold = arity_factor bestf
		in
			if anew < aold then (combine head nil,[x],tail,xs)
			else	if anew > aold
				then (bestf,bestx,combine head otherfs,x::otherxs)
			else
				(combine head bestf,x::bestx,otherfs,otherxs)
		end
	| arity nil nil = (nil,nil,nil,nil)
	| arity _ _ = impossible "83 in mcopt.arity:  #fields and #x's different."




local


	fun	  combine (relf::relfs) (irrelf::irrelfs) =
			(relf@irrelf)::(combine relfs irrelfs)
		| combine nil nil = nil
		| combine nil irrel = irrel
		| combine rel nil = rel



	fun	  addtail (fields::pats) ((result,tag)::tl) =
			(fields,result,tag)::(addtail pats tl)
		| addtail nil nil = nil
		| addtail _ _ = impossible "#5 in mcopt"

	fun	  strip ((fields,result,tag)::pats) =
			let val (fl,tl) = strip pats
			in
				(fields::fl,(result,tag)::tl)
			end
		| strip nil = (nil,nil)

in

(* OPT:  rearrange the fields of a tuple into a better order to evaluate.
	 use the relevance test.  if there are no relevant fields,
	 then the first pattern will match - don't bother returning
	 the rest.  if one of the fields is a record, return it first
	 so it is expanded.  if the relevance test does not isolate
	 one field, use the branch factor test, then the arity test.
*)
fun opt (arg as (nil,_)) = arg
  | opt (pl,xl) =
      let val (pats,tl) = strip pl;
	  val (relf,relx,irrelf,irrelx) = relfactor pats xl
       in case (relf)
            of nil => ([hd pl],xl)
	     | [_]::_  => (addtail(combine relf irrelf)tl,(relx@irrelx))
             |  _  =>
		(case (map rev relf)
		  of ((RECORDpat _)::_)::_ =>
			(addtail (combine (map rev relf) irrelf) tl,
			 (rev relx) @ irrelx)
		   | _ =>
		     let val (branchf,branchx,branchfs,branchxs) =
			         branch relf relx
		      in (case branchf
		           of [_]::_  =>
			      (addtail(combine(combine branchf branchfs)irrelf)tl,
				     branchx@branchxs@irrelx)
			    | _ =>
		              let val (arityf,arityx,arityfs,arityxs) =
				   arity branchf branchx
			       in (addtail(combine(combine(combine arityf arityfs)
				                           branchfs)irrelf)tl,
				   arityx@arityxs@branchxs@irrelx)
			      end)
		      end)
      end

end (* local combine, addtail, strip *)

end (* struct MCopt *)
