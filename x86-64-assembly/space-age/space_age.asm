section .rodata
earth: dd 31_557_600.0
planets: dd 0.240_846_7, 0.615_197_26, 1.0, 1.880_815_8, 11.862_615, 29.447_498, 84.016_846, 164.791_32

section .text
global age
age:
    ; rdi = planet
    ; rsi = time as int
    cvtsi2ss xmm0, esi
    lea rax, [rel planets]

    ; time / (earth year * planet factor)
    movss xmm1, [rel earth]
    mulss xmm1, [rax + 4 * rdi]
    divss xmm0, xmm1

    ret
