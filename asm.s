
# Metadata for debuggers and other tools
.extern printf
.extern scanf

.section .text  # Begins code and data


        # Function(laske_alas(x))
        .global laske_alas
        .type laske_alas, @function
        laske_alas:
        pushq %rbp
        movq %rsp, %rbp
        subq $72, %rsp
        # param backups (1)
        movq %rdi, -8(%rbp)

        # Call(print_int, [x], var_0)
        movq -8(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # LoadIntConst(0, var_2)
        movq $0, -24(%rbp)

        # Call(>, [x,var_2], var_1)
        xor %rax, %rax
        movq -8(%rbp), %rdx
        cmpq -24(%rbp), %rdx
        setg %al
        movq %rax, -32(%rbp)

        # CondJump(var_1, .Llaske_alas_0, .Llaske_alas_1)
        cmpq $0, -32(%rbp)
        jne .Llaske_alas_0
        jmp .Llaske_alas_1

        # Label(.Llaske_alas_0)
        .Llaske_alas_0:

        # LoadIntConst(1, var_5)
        movq $1, -40(%rbp)

        # Call(-, [x,var_5], var_4)
        movq -40(%rbp), %rax 
        subq -8(%rbp), %rax 
        movq %rax, -48(%rbp)

        # Call(laske_alas, [var_4], var_3)
        movq -48(%rbp), %rdi
        call laske_alas
        movq %rax, -56(%rbp)

        # Copy(U, laske_alas_return)
        movq $0, %rax
        movq %rax, -72(%rbp)

        # Label(.Llaske_alas_1)
        .Llaske_alas_1:

        # Return(laske_alas_return)
        movq -72(%rbp), %rax

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
        subq $24, %rsp
        # param backups (0)

        # Call(read_int, [], var_1)
        movq $scan_format, %rdi
        leaq -8(%rbp), %rsi
        call scanf
        cmpq $1, %rax
        jne .Lend

        # Call(laske_alas, [var_1], var_0)
        movq -8(%rbp), %rdi
        call laske_alas
        movq %rax, -16(%rbp)

        # Return(U)
        movq $0, %rax

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


