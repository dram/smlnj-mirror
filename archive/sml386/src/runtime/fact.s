ALIGN 2

DD 7

L7: DD MAKE_DESC((L7-base)/4+1,tag_backptr)

; 2458:

L1:

cmp   dword ptr [esp+0],edi

jns @f

call    dword ptr [esp+12]

@@:

mov   eax,17

stos  eax

lea   eax,L2

stos  eax

lea   eax,dword ptr [edi+-4]

mov   dword ptr [esp+48],eax

mov   eax,33

stos  eax

lea   eax,L8

stos  eax

mov   eax,1

stos  eax

lea   edx,dword ptr [edi+-8]

mov   eax,33

stos  eax

lea   eax,L9

stos  eax

mov   eax,edx

stos  eax

lea   edx,dword ptr [edi+-8]

mov   eax,33

stos  eax

mov   eax,dword ptr [esp+48]

stos  eax

mov   eax,edx

stos  eax

lea   eax,dword ptr [edi+-8]

mov   dword ptr [esp+40],eax

mov   eax,dword ptr [esp+44]

mov   edx,dword ptr [eax+0]

jmp   edx

ALIGN 2

L10: DD MAKE_DESC((L10-base)/4+1,tag_backptr)

DD 71

L9:

DB "Core"

ALIGN 2

ALIGN 2

L11: DD MAKE_DESC((L11-base)/4+1,tag_backptr)

DD 71

L8:

DB "Core"

ALIGN 2

ALIGN 2

DD 7

L12: DD MAKE_DESC((L12-base)/4+1,tag_backptr)

; 2453:

L2:

cmp   dword ptr [esp+0],edi

jns @f

call    dword ptr [esp+12]

@@:

mov   eax,49

stos  eax

lea   eax,L6

stos  eax

lea   eax,L5

stos  eax

mov   eax,dword ptr [esp+44]

stos  eax

lea   ebp,dword ptr [edi+-12]

lea   ebp,dword ptr [ebp+4]

mov   dword ptr [esp+40],21

jmp   L3

ALIGN 2

DD 66

L13: DD MAKE_DESC((L13-base)/4+1,tag_backptr)

; 2455:

L3:

cmp   dword ptr [esp+0],edi

jns @f

call    dword ptr [esp+12]

@@:

cmp   dword ptr [esp+40],1

jne   L14

mov   edx,dword ptr [ebp+0]

mov   dword ptr [esp+44],ebp

mov   dword ptr [esp+40],3

jmp   edx

L14:

mov   eax,49

stos  eax

lea   eax,L4

stos  eax

mov   eax,dword ptr [esp+40]

stos  eax

mov   eax,ebp

stos  eax

lea   ebp,dword ptr [edi+-12]

sub   dword ptr [esp+40],2

jno @f

call    dword ptr [esp+20]

@@:

jmp   L3

ALIGN 2

DD 6

L15: DD MAKE_DESC((L15-base)/4+1,tag_backptr)

; 2456:

L4:

cmp   dword ptr [esp+0],edi

jns @f

call    dword ptr [esp+12]

@@:

mov   eax,dword ptr [esp+44]

mov   eax,dword ptr [eax+4]

mov   dword ptr [esp+48],eax

mov   ebx,dword ptr [esp+48]

sar   ebx,1

sub   dword ptr [esp+40],1

mov   ecx,dword ptr [esp+40]

imul  ecx,ebx

mov   dword ptr [esp+40],ecx

jno @f

call    dword ptr [esp+20]

@@:

add   dword ptr [esp+40],1

mov   eax,dword ptr [esp+44]

mov   eax,dword ptr [eax+8]

mov   dword ptr [esp+44],eax

mov   eax,dword ptr [esp+44]

mov   edx,dword ptr [eax+0]

jmp   edx

ALIGN 2

DD 6

L16: DD MAKE_DESC((L16-base)/4+1,tag_backptr)

; 2454:

L5:

cmp   dword ptr [esp+0],edi

jns @f

call    dword ptr [esp+12]

@@:

mov   eax,33

stos  eax

mov   eax,dword ptr [esp+40]

stos  eax

mov   eax,dword ptr [esp+44]

lea   eax,dword ptr [eax+-4]

stos  eax

lea   esi,dword ptr [edi+-8]

mov   eax,33

stos  eax

mov   eax,esi

stos  eax

mov   eax,1

stos  eax

lea   eax,dword ptr [edi+-8]

mov   dword ptr [esp+40],eax

mov   eax,dword ptr [esp+44]

mov   eax,dword ptr [eax+4]

mov   dword ptr [esp+44],eax

mov   eax,dword ptr [esp+44]

mov   edx,dword ptr [eax+0]

jmp   edx

ALIGN 2

DD 7

L17: DD MAKE_DESC((L17-base)/4+1,tag_backptr)

; 2457:

L6:

cmp   dword ptr [esp+0],edi

jns @f

call    dword ptr [esp+12]

@@:

mov   ebp,dword ptr [esp+44]

jmp   L3
