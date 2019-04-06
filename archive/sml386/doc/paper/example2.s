...
L2:
cmp   dword ptr [esp+0],edi   ; check the available memory
jns @f
call    dword ptr [esp+12]
@@:
mov   eax,dword ptr [esp+40]
mov   edx,dword ptr [eax+0]   ; edx = x
mov   eax,dword ptr [esp+40]
mov   eax,dword ptr [eax+4]   
mov   dword ptr [esp+40],eax  ; esp+40=a
cmp   edx,1                   ; if x=0
jne   L12
mov   eax,dword ptr [esp+44]  ; then a
mov   edx,dword ptr [eax+0]
jmp   edx                     
L12:                          ; else
mov   esi,edx
sub   esi,2                   ; esi=x-1
jno @f
call    dword ptr [esp+20]
@@:
mov   ebx,edx
sar   ebx,1
mov   ebp,dword ptr [esp+40]
sub   ebp,1
imul  ebp,ebx
jno @f
call    dword ptr [esp+20]
@@:
add   ebp,1                  ; ebp = x*a
mov   eax,33
stos  eax
mov   eax,esi
stos  eax
mov   eax,ebp
stos  eax                    ; make a closure with x-1 x*a
lea   eax,dword ptr [edi+-8]
mov   dword ptr [esp+40],eax
jmp   L2                     ; f(x-1,x*a)

