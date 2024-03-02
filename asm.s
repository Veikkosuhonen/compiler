
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern clock

.section .text  # Begins code and data


        # Function(fibonacci(n))
        .global fibonacci
        .type fibonacci, @function
        fibonacci:
        pushq %rbp
        movq %rsp, %rbp
        subq $128, %rsp
        # param backups (1)
        movq %rdi, -16(%rbp)

        # LoadIntConst(0, var_1)
        movq $0, -8(%rbp)

        # Call(<=, [n,var_1], var_0)
        xor %rax, %rax
        movq -16(%rbp), %rdx
        cmpq -8(%rbp), %rdx
        setle %al
        movq %rax, -24(%rbp)

        # CondJump(var_0, .Lfibonacci_0, .Lfibonacci_2)
        cmpq $0, -24(%rbp)
        jne .Lfibonacci_0
        jmp .Lfibonacci_2

        # Label(.Lfibonacci_0)
        .Lfibonacci_0:

        # LoadIntConst(0, var_2)
        movq $0, -32(%rbp)

        # Copy(var_2, _return0)
        movq -32(%rbp), %rax
        movq %rax, -40(%rbp)

        # Jump(.Lfibonacci_1)
        jmp .Lfibonacci_1

        # Label(.Lfibonacci_2)
        .Lfibonacci_2:

        # LoadIntConst(1, var_4)
        movq $1, -48(%rbp)

        # Call(==, [n,var_4], var_3)
        xor %rax, %rax
        movq -16(%rbp), %rdx
        cmpq -48(%rbp), %rdx
        sete %al
        movq %rax, -56(%rbp)

        # CondJump(var_3, .Lfibonacci_3, .Lfibonacci_5)
        cmpq $0, -56(%rbp)
        jne .Lfibonacci_3
        jmp .Lfibonacci_5

        # Label(.Lfibonacci_3)
        .Lfibonacci_3:

        # LoadIntConst(1, var_5)
        movq $1, -64(%rbp)

        # Copy(var_5, _return0)
        movq -64(%rbp), %rax
        movq %rax, -40(%rbp)

        # Jump(.Lfibonacci_4)
        jmp .Lfibonacci_4

        # Label(.Lfibonacci_5)
        .Lfibonacci_5:

        # LoadIntConst(1, var_9)
        movq $1, -72(%rbp)

        # Call(-, [n,var_9], var_8)
        movq -16(%rbp), %rax 
        subq -72(%rbp), %rax 
        movq %rax, -80(%rbp)

        # Call(fibonacci, [var_8], var_7)
        movq -80(%rbp), %rdi
        call fibonacci
        movq %rax, -88(%rbp)

        # LoadIntConst(2, var_12)
        movq $2, -96(%rbp)

        # Call(-, [n,var_12], var_11)
        movq -16(%rbp), %rax 
        subq -96(%rbp), %rax 
        movq %rax, -104(%rbp)

        # Call(fibonacci, [var_11], var_10)
        movq -104(%rbp), %rdi
        call fibonacci
        movq %rax, -112(%rbp)

        # Call(+, [var_7,var_10], var_6)
        movq -88(%rbp), %rax 
        addq -112(%rbp), %rax 
        movq %rax, -120(%rbp)

        # Copy(var_6, _return0)
        movq -120(%rbp), %rax
        movq %rax, -40(%rbp)

        # Label(.Lfibonacci_4)
        .Lfibonacci_4:

        # Copy(_return0, _return0)
        movq -40(%rbp), %rax
        movq %rax, -40(%rbp)

        # Label(.Lfibonacci_1)
        .Lfibonacci_1:

        # Copy(_return0, _return)
        movq -40(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lfibonacci_end)
        .Lfibonacci_end:
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(main())
        .global main
        .type main, @function
        main:
        pushq %rbp
        movq %rsp, %rbp
        subq $96, %rsp
        # param backups (0)

        # LoadIntConst(0, var_0)
        movq $0, -8(%rbp)

        # Label(.Lmain_0)
        .Lmain_0:

        # LoadIntConst(2000, var_2)
        movq $2000, -16(%rbp)

        # Call(<, [var_0,var_2], var_1)
        xor %rax, %rax
        movq -8(%rbp), %rdx
        cmpq -16(%rbp), %rdx
        setl %al
        movq %rax, -24(%rbp)

        # CondJump(var_1, .Lmain_1, .Lmain_2)
        cmpq $0, -24(%rbp)
        jne .Lmain_1
        jmp .Lmain_2

        # Label(.Lmain_1)
        .Lmain_1:

        # LoadIntConst(11, var_5)
        movq $11, -32(%rbp)

        # Call(fibonacci, [var_5], var_4)
        movq -32(%rbp), %rdi
        call fibonacci
        movq %rax, -40(%rbp)

        # Call(print_int, [var_4], var_3)
        movq -40(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # LoadIntConst(1, var_7)
        movq $1, -56(%rbp)

        # Call(+, [var_0,var_7], var_6)
        movq -8(%rbp), %rax 
        addq -56(%rbp), %rax 
        movq %rax, -64(%rbp)

        # Copy(var_6, var_0)
        movq -64(%rbp), %rax
        movq %rax, -8(%rbp)

        # Copy(U, _return0)
        movq $0, %rax
        movq %rax, -80(%rbp)

        # Jump(.Lmain_0)
        jmp .Lmain_0

        # Label(.Lmain_2)
        .Lmain_2:

        # Copy(_return0, _return)
        movq -80(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lmain_end)
        .Lmain_end:
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

# Actual end
.Lend:
        # Return from main with status code 0
        movq $0, %rax
        movq %rbp, %rsp
        popq %rbp
        ret

# String data that we pass to functions 'scanf' and 'printf'
scan_format:
.asciz "%ld"

print_format:
.asciz "%ld\n"

print_bool_format:
.asciz "%s\n"

true_str:
    .ascii "true\n"

false_str:
    .ascii "false\n"


