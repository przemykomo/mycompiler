bits 64
default rel

global main
section .text
main:
        push    rbp
        mov     rbp, rsp
        int3

;         cmp     rax, QWORD [RBP-15]
;         jle .L2
;         movabs     rax, -1
;         jmp .L3
; .L2:
;         movabs     rbx, -1
; .L3:
;
;         cmp     rax, rbp
        ; imul    rax, rax
        leave
        ret
