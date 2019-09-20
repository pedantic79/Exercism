section .text
global leap_year
leap_year:
    xor eax, eax        ; set default return to 0 (FALSE)
    test dil, 3         ; Check bottom 2 bits to see if they are set
    jne .end            ; ZF=1 if any bit set, jump to end if divisible by 4
    mov al, 1           ; Set the return to TRUE just in-case

    ; Used https://www.hackersdelight.org/magic.htm to determine magic number
    ; and shift amount to divide by 100 using multiplication and shifting
    imul rcx, rdi, 0x51EB851F
    shr rcx, 37

    ; To calculate the modulo, we'll multiply the quotient by the divisor
    ; If it's the same as edi, that means the modulo is 0
    imul ecx, ecx, 100
    cmp edi, ecx        ; compare if they are equal
    jne .end            ; if they're note equal return TRUE

    ; Same as above, but shift by 39 to do /400
    imul rcx, rdi, 0x51EB851F
    shr rcx, 39

    imul ecx, ecx, 400  ; multiply by 400 to do the same muliplier
    cmp edi, ecx
    sete al             ; sets the A register to 0 if divisible by 400, 1 if not
.end:
    ret
