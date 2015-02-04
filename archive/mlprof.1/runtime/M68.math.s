Closure(_sin_v,1f)
1:	movl sp@(4),a0
	fmoved a0@,fp0
	fsinx fp0,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	lea a6@(12),a6
	rsb

Closure(_cos_v,1f)
1:	movl sp@(4),a0
	fcosd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	lea a6@(12),a6
	rsb

(* handle operr and dz *)
Closure(_ln_v,1f)
1:	movl sp@(4),a0
	flognd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	lea a6@(12),a6
	rsb

(* handle overflow *)
Closure(_exp_v,1f)
1:	movl sp@(4),a0
	fetoxd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	lea a6@(12),a6
	rsb

(* handle operr *)
Closure(_sqrt_v,1f)
1:	movl sp@(4),a0
	fsqrtd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	lea a6@(12),a6
	rsb

Closure(_atan_v,1f)
1:	movl sp@(4),a0
	fatand a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	lea a6@(12),a6
	rsb

