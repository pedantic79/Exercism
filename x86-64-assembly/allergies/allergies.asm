section .text
global allergic_to
allergic_to:
    mov     ecx, edi
    mov     eax, 1
    shl     eax, cl
    and     eax, esi
    ret

global list
list:
    xor     edx, edx         ; allergy value
    xor     eax, eax         ; index/count of matching allergy
.start_loop:
    test    edi, edi         ; score != 0
    je      .end
    cmp     edx, 7           ; allergy value < 7
    jg      .end
    test    dil, 1           ; check bottom score bit
    je      .increment

            ; stores allergy into array
    mov     dword [rsi+4 + rax*4], edx
    add     eax, 1           ; increment count
.increment:
    add     edx, 1           ; increment allergy
    shr     edi, 1           ; shift score right
    jmp     .start_loop
.end:
    mov     dword [rsi], eax ; stores count into array
    ret
