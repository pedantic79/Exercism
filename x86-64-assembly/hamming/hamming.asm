section .text
global distance
distance:
        xor     edx, edx
        xor     eax, eax
.start:
        movzx   r8d, byte [rdi + rdx]
        movzx   ecx, byte [rsi + rdx]

        ;; checks are not null
        test    r8b, r8b
        je      .end_while
        test    cl, cl
        je      .end_while

        ;; increment if characters are different
        cmp     r8b, cl
        je      .increment_loop
        add     eax, 1
.increment_loop:
        add     rdx, 1
        jmp     .start

.end_while:
        cmp     r8b, cl
        je      .equal_len
        or      eax, -1
.equal_len:
        ret
