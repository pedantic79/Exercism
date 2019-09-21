section .text
global leap_year
leap_year:
    xor eax, eax        ; set default return to 0 (FALSE)
    test dil, 3         ; Check bottom 2 bits to see if it's divisible by 4
    jne .is_not_leap    ; ZF=1 if any bit set, jump to .is_not_leap

    ; Used https://www.hackersdelight.org/magic.htm to determine magic number
    ; and shift amount to divide by 100 using multiplication and shifting
    imul rcx, rdi, 0x51EB851F
    shr rcx, 37

    ; To calculate the modulo, we'll multiply the quotient by the divisor
    ; If it's the same as edi, that means the modulo is 0
    imul ecx, ecx, 100
    cmp edi, ecx        ; compare if they are equal
    jne .is_leap        ; if not divisible by 100, then it is a leap

    ; As mawis pointed out, 400 has the prime factors of 2, 2, 2, 2, 5, 5
    ; And since we already know the number is divisible by 25, since it's
    ; divisible by 100. We only need to check if it's divisible by 16
    test dil, 15
    jne .is_not_leap    ; if it is divisible by 400, then it is not a leap
.is_leap:
    mov al, 1
.is_not_leap:
    ret
