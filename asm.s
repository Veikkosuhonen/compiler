
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern clock

.section .text  # Begins code and data


        # Function(main())
        .global main
        .type main, @function
        main:
        pushq %rbp
        movq %rsp, %rbp
        subq $48, %rsp
        # param backups (0)

        # LoadIntConst(2, var_0)
        movq $2, -8(%rbp)

        # Call(&, [var_0], var_1)
        leaq -8(%rbp), %rax
        movq %rax, -16(%rbp)

        # LoadIntConst(3, var_2)
        movq $3, -24(%rbp)

        # Copy(var_2, (var_1))
        movq -16(%rbp), %rax
        movq -24(%rbp), %rdx
        movq %rdx, (%rax)

        # Call(print_int, [var_0], var_3)
        movq -8(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # Copy(U, _return)
        movq $0, %rax
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


