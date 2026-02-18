bits 64
default rel

section .text
square:
        push    rbp
        mov     rbp, rsp

        cmp     rax, QWORD [RBP-15]
        jle .L2
        movabs     rax, -1
        jmp .L3
.L2:
        movabs     rbx, -1
.L3:

        cmp     rax, rbp
        ; imul    rax, rax
        leave
        ret
