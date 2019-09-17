section .rodata
prefix: db "One for "
prefix_len: equ $ - prefix

you: db "you"
you_len: equ $ - you

suffix: db ", one for me.", 0
suffix_len: equ $ - suffix

section .text
global two_fer
two_fer:
    mov rax, rdi          ; save name into rax
    mov rdi, rsi          ; set buffer to destination

    lea rsi, [rel prefix] ; load prefix into source
    mov rcx, prefix_len   ; set length of bytes to copy
    rep movsb             ; copy rcx number of bytes to rdi

    test rax, rax         ; check if name is null
    jne .name             ; if not jump to name

    lea rsi, [rel you]    ; copy you string into rdi
    mov rcx, you_len
    rep movsb
    jmp .end              ; skip over .name block

.name:
    mov rsi, rax          ; set name to rsi
    cmp byte [rsi], 0     ; check if byte is NULL
    je .end               ; skip if it is
.loop:
    movsb                 ; copy one byte from name to rdi
    cmp byte [rsi], 0     ; compare next byte
    jne .loop             ; repeat until NULL

.end:
    lea rsi, [rel suffix] ; copy suffix into rdi
    mov rcx, suffix_len
    rep movsb
    ret
