
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .text  # Begins code and data


        # Function(main())
        .global main
        .type main, @function
        main:
        pushq %rbp
        movq %rsp, %rbp
        subq $48, %rsp
        # param backups (0)

        # LoadIntConst(0, var_1: Int)
        movq $0, -8(%rbp)

        # LoadIntConst(1, var_4: Int)
        movq $1, -16(%rbp)

        # Call(print_int: [: Int] -> Unit, [var_4: Int], var_6: Unit)
        movq -16(%rbp), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -24(%rbp)

        # Copy(U: Unit, var_2: Unit)
        movq -32(%rbp), %rax
        movq %rax, -40(%rbp)

        # Call(print_int: [: Int] -> Unit, [var_4: Int], var_9: Unit)
        movq -16(%rbp), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -48(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -32(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lmain_end)
        .Lmain_end:

        # LoadIntConst(0, _return: Unknown)
        movq $0, %rax
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

# Error
.Lerr:
        # Return from main with status code 1
        movq $1, %rax
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


