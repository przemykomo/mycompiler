bits 64
default rel

section .text
square:
        push    rbp
        mov     rbp, rsp

        add     rax, QWORD [RAX-0]
        add     rax, QWORD [RAX+429496729]
        add     rax, QWORD [RAX+18446744073709551615]
        add     rdx, QWORD [RAX-0]
        add     rbx, QWORD [RAX-0]
        add     rsp, QWORD [RAX-0]
        add     rbp, QWORD [RAX-0]
        add     rsi, QWORD [RAX-0]
        add     rdi, QWORD [RAX-0]
        add     r8, QWORD [RAX-0]
        add     r9, QWORD [RAX-0]
        add     r10, QWORD [RAX-0]
        add     r11, QWORD [RAX-0]
        add     r12, QWORD [RAX-0]
        add     r13, QWORD [RAX-0]
        add     r14, QWORD [RAX-0]
        add     r15, QWORD [RAX-0]

        add     rax, QWORD [RAX-0]
        add     rax, QWORD [RCX-0]
        add     rax, QWORD [RDX-0]
        add     rax, QWORD [RBX-0]
        add     rax, QWORD [RSP-0]
        add     rax, QWORD [RBP-0]
        add     rax, QWORD [RSI-0]
        add     rax, QWORD [RDI-0]

        add     rax, QWORD [R8-0]
        add     rax, QWORD [R9-0]
        add     rax, QWORD [R10-0]
        add     rax, QWORD [R11-0]
        add     rax, QWORD [R12-0]
        add     rax, QWORD [R13-0]
        add     rax, QWORD [R14-0]
        add     rax, QWORD [R15-0]



        add     RAX, RAX
        add     RAX, RCX
        add     RAX, RDX
        add     RAX, RBX
        add     RAX, RSP
        add     RAX, RBP
        add     RAX, RSI
        add     RAX, RDI
        add     RAX, R8 
        add     RAX, R9 
        add     RAX, R10
        add     RAX, R11
        add     RAX, R12
        add     RAX, R13
        add     RAX, R14
        add     RAX, R15
        ; imul    rax, rax
        leave
        ret
