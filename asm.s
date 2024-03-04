
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .text  # Begins code and data


        # Function(f())
        .global f
        .type f, @function
        f:
        pushq %rbp
        movq %rsp, %rbp
        subq $48, %rsp
        # param backups (0)

        # LoadIntConst(123, var_2)
        movq $123, -8(%rbp)

        # Copy(var_2, var_1)
        movq -8(%rbp), %rax
        movq %rax, -16(%rbp)

        # Call(new, [var_1], var_0)
        movq $8, %rdi
        call malloc
        test %rax, %rax
        jz .Lend
        # skip movq %rax, %rax
        movq -16(%rbp), %rdx
        movq %rdx, (%rax)
        movq %rax, -24(%rbp)

        # Copy(var_0, _return)
        movq -24(%rbp), %rax
        # skip movq %rax, %rax

        # Jump(.Lf_end)
        jmp .Lf_end

        # Copy(U, _return)
        movq $0, %rax
        # skip movq %rax, %rax

        # Label(.Lf_end)
        .Lf_end:
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
        subq $48, %rsp
        # param backups (0)

        # Call(f, [], var_0)
        call f
        movq %rax, -8(%rbp)

        # Call(*, [var_0], var_2)
        movq -8(%rbp), %rax
        movq (%rax), %rax
        movq %rax, -16(%rbp)

        # Call(print_int, [var_2], var_1)
        movq -16(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # Call(delete, [var_0], var_3)
        movq -8(%rbp), %rdi
        call free

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


