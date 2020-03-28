section .rodata
black:  db "black",  0
brown:  db "brown",  0
red:    db "red",    0
orange: db "orange", 0
yellow: db "yellow", 0
green:  db "green",  0
blue:   db "blue",   0
violet: db "violet", 0
grey:   db "grey",   0
white:  db "white",  0
COLORS: dq black,brown,red,orange,yellow,green,blue,violet,grey,white,0

section .text
global color_code
color_code:
    lea r8, [rel COLORS]
    xor r9, r9             ; set to pos to 0

.loop:
    mov rsi, [r8 + 8 * r9] ; READ from COLORS[r9]
    test rsi, rsi          ; check if null
    je .end

    call streq             ; check if equal
    test eax, eax
    jne .end               ; jump if true

    inc r9,                ; increment pos
    jmp .loop

.end:
    mov rax, r9            ; return pos
    ret

global colors
colors:
    lea rax, [rel COLORS]
    ret

streq:
    mov eax, -1

.streq_loop:
    add eax, 1
    movzx edx, byte [rdi + rax] ; load next character into edx
    test dl, dl                 ; check if edx is '\0'
    je .streq_end

    cmp byte [rsi + rax], dl    ; compare characters
    je .streq_loop              ; loop again if equal
    xor eax, eax                ; return 0
    ret

.streq_end:
    mov eax, 1                  ; return 1
    ret
