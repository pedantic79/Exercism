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

%macro  check_color 3

    lea rsi, [rel %1]
    call streq
    test eax, eax
    je %2

    mov eax, %3
    ret

%endmacro

section .text
global color_code
color_code:
         check_color black, .brown, 0
.brown:  check_color brown, .red, 1
.red:    check_color red, .orange, 2
.orange: check_color orange, .yellow, 3
.yellow: check_color yellow, .green, 4
.green:  check_color green, .blue, 5
.blue:   check_color blue, .violet, 6
.violet: check_color violet, .grey, 7
.grey:   check_color grey, .white, 8
.white:  check_color white, .end, 9
.end:
    mov eax, 10
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
