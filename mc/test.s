bits 64
default rel

section .text
square:
        push    rbp
        mov     rbp, rsp

        cmp     rax, QWORD [RBP-15]
        cmp     QWORD [rax-15], rbp

        cmp     rax, rbp
        ; imul    rax, rax
        leave
        ret
