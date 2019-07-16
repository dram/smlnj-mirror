(* hppaRewrite.sml -- rewrite an hppa instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor HppaRewrite(Instr:HPPAINSTR) = struct
  structure I = Instr

  fun rewriteUse(instr, rs, rt) = let
    fun replc r = if r=rs then rt else r
  in
    case instr
    of I.STORE{st, b, d, r, mem} => 
	if b=rs then I.STORE{st=st, b=rt, d=d, r=replc r, mem=mem} 
	else if r=rs then I.STORE{st=st, b=b, d=d, r=rt, mem=mem} else instr
     | I.LOAD{l, r1, r2, t, mem} =>
	if r1=rs then I.LOAD{l=l, r1=rt, r2=replc r2, t=t, mem=mem}
	else if r2=rs then I.LOAD{l=l, r1=r1, r2=rt, t=t, mem=mem} else instr
     | I.LOADI{li, r, i, t, mem=mem} => 
	if r=rs then I.LOADI{li=li, r=rt, i=i, t=t, mem=mem} else instr
     | I.ARITH{a, r1, r2, t} =>
	if r1=rs then I.ARITH{a=a, r1=rt, r2=replc r2, t=t}
	else if r2=rs then I.ARITH{a=a, r1=r1, r2=rt, t=t} else instr
     | I.ARITHI{ai, r, i, t} => 
	if r=rs then I.ARITHI{ai=ai, r=rt, i=i, t=t} else instr
     | I.COMCLR{cc, r1, r2, t} => 
	if r1=rs then I.COMCLR{cc=cc, r1=rt, r2=replc r2, t=t}
	else if r2=rs then I.COMCLR{cc=cc, r1=r1, r2=rt, t=t} else instr
     | I.SHIFTV{sv, r, len, t} => 
	if r=rs then I.SHIFTV{sv=sv, r=rt, len=len, t=t} else instr
     | I.SHIFT{s, r, p, len, t} => 
	if r=rs then I.SHIFT{s=s, r=rt, p=p, len=len, t=t} else instr
     | I.BCOND{cmp, bc, r1, r2, t, f} => 
	if r1=rs then I.BCOND{cmp=cmp, bc=bc, r1=rt, r2=replc r2, t=t, f=f}
	else if r2=rs then I.BCOND{cmp=cmp, bc=bc, r1=r1, r2=rt, t=t, f=f}
	     else instr
     | I.BCONDI{cmpi, bc, i, r2, t, f} => 
        if r2=rs then I.BCONDI{cmpi=cmpi, bc=bc, i=i, r2=rt, t=t, f=f} else instr
     | I.BV{x, b, labs} => 
	if x=rs then I.BV{x=rt, b=replc b, labs=labs} 
	else if b=rs then I.BV{x=x, b=rt, labs=labs} else instr
     | I.BLE{b, d, sr, t, defs, uses} => 
	if b=rs then I.BLE{b=rt, d=d, sr=sr, t=t, defs=defs, uses=uses} else instr
     | I.LDO{b, t, i} => if b=rs then I.LDO{b=rt, t=t, i=i} else instr
     | I.COPY(rds, rss, l) => I.COPY(rds, map replc rss, l)
     | I.MTCTL{r, t} => if r=rs then I.MTCTL{r=rt, t=t} else instr
     | I.FSTORE{fst, b, d, r, mem} =>
	if b=rs then I.FSTORE{fst=fst, b=rt, d=d, r=r, mem=mem} else instr
     | I.FSTOREX{fstx, b, x, r, mem} =>
	if b=rs then I.FSTOREX{fstx=fstx, b=rt, x=replc x, r=r, mem=mem} 
	else if x=rs then I.FSTOREX{fstx=fstx, b=b, x=rt, r=r, mem=mem} 
        else instr
     | I.FLOAD{fl, b, d, t, mem} =>
	if b=rs then I.FLOAD{fl=fl, b=rt, d=d, t=t, mem=mem} else instr
     | I.FLOADX{flx, b, x, t, mem} =>
	if b=rs then I.FLOADX{flx=flx, b=rt, x=replc x, t=t, mem=mem} 
	else if x=rs then I.FLOADX{flx=flx, b=b, x=rt, t=t, mem=mem} else instr
     | _ => instr
  end

  fun rewriteDef(instr, rs, rt) = let
    fun replc r = if r=rs then rt else r
  in
    case instr
    of I.ARITH{a, r1, r2, t} =>
        if t=rs then I.ARITH{a=a, r1=r1, r2=r2, t=rt} else instr
     | I.ARITHI{ai, i, r, t} => 
	if t=rs then I.ARITHI{ai=ai, i=i, r=r, t=rt} else instr
     | I.LOAD{l, r1, r2, t, mem} =>
        if t=rs then I.LOAD{l=l, r1=r1, r2=r2, t=rt, mem=mem} else instr
     | I.LOADI{li, i, r, t, mem} => 
	if t=rs then I.LOADI{li=li, i=i, r=r, t=rt, mem=mem} else instr
     | I.COMCLR{cc, r1, r2, t} => 
	if t=rs then I.COMCLR{cc=cc, r1=r1, r2=r2, t=rt} else instr
     | I.SHIFTV{sv, r, len, t} => 
	if t=rs then I.SHIFTV{sv=sv, r=r, len=len, t=rt} else instr
     | I.SHIFT{s, r, p, len, t} => 
	if t=rs then I.SHIFT{s=s, r=r, p=p, len=len, t=rt} else instr
     | I.BLE{d, b, sr, t, defs, uses} => 
	if t=rs then I.BLE{d=d, b=b, sr=sr, t=rt, defs=defs, uses=uses}
	else instr
     | I.LDIL{i, t} => if t=rs then I.LDIL{i=i, t=rt} else instr
     | I.LDO{i, b, t} => if t=rs then I.LDO{i=i, b=b, t=rt} else instr
     | I.COPY(rds, rss, l) => I.COPY(map replc rds, rss, l)
     | _ => instr
  end

  fun frewriteUse(instr, fs, ft) = let
    fun replc r = if r=fs then ft else r
  in
    case instr
    of I.FSTORE{fst, b, d, r, mem} =>
        if r=fs then I.FSTORE{fst=fst, b=b, d=d, r=ft, mem=mem} else instr
     | I.FSTOREX{fstx, b, x, r, mem} =>
	if r=fs then I.FSTOREX{fstx=fstx, b=b, x=x, r=ft, mem=mem} else instr
     | I.FARITH{fa, r1, r2, t}  => 
	if r1=fs then I.FARITH{fa=fa, r1=ft, r2=replc r2, t=t}
	else if r2=fs then I.FARITH{fa=fa, r1=r1, r2=ft, t=t} else instr
     | I.FUNARY{fu, f, t} => 
	if f=fs then I.FUNARY{fu=fu, f=ft, t=t} else instr
     | I.FCMP(fcc, f1, f2) => 
	if f1=fs then I.FCMP(fcc, ft, replc f2) 
	else if f2=fs then I.FCMP(fcc, f1, ft) else instr
     | I.FCOPY(fds, fss, l) => I.FCOPY(fds, map replc fss, l)
     | _ => instr
    (*esac*)
  end

  fun frewriteDef(instr, fs, ft) = let
    fun replc r = if r=fs then ft else r
  in
    case instr
    of I.FLOAD{fl, b, d, t, mem} =>
        if t=fs then I.FLOAD{fl=fl, b=b, d=d, t=ft, mem=mem} else instr
     | I.FLOADX{flx, b, x, t, mem} => 
	if t=fs then I.FLOADX{flx=flx, b=b, x=x, t=ft, mem=mem} else instr
     | I.FARITH {fa, r1, r2, t} =>
	if t=fs then I.FARITH{fa=fa, r1=r1, r2=r2, t=ft} else instr
     | I.FUNARY{fu, f, t} => 
	if t=fs then I.FUNARY{fu=fu, f=f, t=ft} else instr
     | I.FCOPY(fds, fss, l) => I.FCOPY(map replc fds, fss, l)
     | _ => instr
    (*esac*)
  end
end
