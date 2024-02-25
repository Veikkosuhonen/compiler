
# Metadata for debuggers and other tools
.extern printf
.extern scanf

.section .text  # Begins code and data


        # Function(func(x))
        .global func
        .type func, @function
        func:
        pushq %rbp
        movq %rsp, %rbp
        subq $48, %rsp
        # param backups (1)
        movq %rdi, -16(%rbp)

        # LoadBoolConst(true, var_0)
        movq $1, -8(%rbp)

        # CondJump(var_0, .Lfunc_0, .Lfunc_1)
        cmpq $0, -8(%rbp)
        jne .Lfunc_0
        jmp .Lfunc_1

        # Label(.Lfunc_0)
        .Lfunc_0:

        # Copy(x, _return)
        movq -16(%rbp), %rax
        movq %rax, %rax

        # Jump(.Lfunc_end)
        jmp .Lfunc_end

        # Copy(U, _return0)
        movq $0, %rax
        movq %rax, -32(%rbp)

        # Label(.Lfunc_1)
        .Lfunc_1:

        # LoadIntConst(1, var_1)
        movq $1, -40(%rbp)

        # Copy(var_1, _return)
        movq -40(%rbp), %rax
        movq %rax, %rax

        # Label(.Lfunc_end)
        .Lfunc_end:
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
        subq $32, %rsp
        # param backups (0)

        # LoadIntConst(69, var_1)
        movq $69, -8(%rbp)

        # Call(func, [var_1], var_0)
        movq -8(%rbp), %rdi
        call func
        movq %rax, -16(%rbp)

        # Copy(U, _return)
        movq $0, %rax
        movq %rax, %rax

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


