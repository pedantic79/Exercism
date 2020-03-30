section .text
global is_pangram
is_pangram:
    xor r8d, r8d                ; answer
    mov r9, -1                  ; pos
    mov rsi, rdi                ; save

.loop:
    add r9, 1
    movzx edi, byte [rsi + r9]  ; read next character
    test edi, edi
    jz .end                     ; skip the end if '\0'

    call is_alpha_mask          ; 0 if non-alpha, else the mask value
    test eax, eax
    jz .loop                    ; if 0 then skip

    or r8d, eax                 ; add the mask to total
    jmp .loop                   ; always loop

.end:
    xor eax, eax
    cmp r8d, 0x3ffffff          ; all bits should be set
    sete al                     ; then set to 1
    ret

is_alpha_mask:
    ; rdi
    xor eax, eax
    or rdi, 32                  ; lowercase
    sub rdi, 'a'                ; subtract 'a'
    cmp rdi, 26
    jae .end                    ; return 0 if >= 26
    bts eax, edi
.end:
    ret
