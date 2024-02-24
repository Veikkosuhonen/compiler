
# Metadata for debuggers and other tools
.extern printf
.extern scanf

.section .text  # Begins code and data


        # Function(recurse(x, i))
        .global recurse
        .type recurse, @function
        recurse:
        pushq %rbp
        movq %rsp, %rbp
        subq $24, %rsp
        # param backups (0)

        # Call(print_int, [x], recurse_return)
        movq -8(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # LoadIntConst(10, recurse_return)
        movq $10, -16(%rbp)

        # Call(>, [i,recurse_return], recurse_return)
        xor %rax, %rax
        movq -24(%rbp), %rdx
        cmpq -16(%rbp), %rdx
        setg %al
        movq %rax, -16(%rbp)

        # CondJump(recurse_return, .Lrecurse_0, .Lrecurse_2)
        cmpq $0, -16(%rbp)
        jne .Lrecurse_0
        jmp .Lrecurse_2

        # Label(.Lrecurse_0)
        .Lrecurse_0:

        # Copy(x, recurse_return)
        movq -8(%rbp), %rax
        movq %rax, -16(%rbp)

        # Jump(.Lrecurse_1)
        jmp .Lrecurse_1

        # Label(.Lrecurse_2)
        .Lrecurse_2:

        # LoadIntConst(2, recurse_return)
        movq $2, -16(%rbp)

        # Call(*, [recurse_return,x], recurse_return)
        movq -8(%rbp), %rax 
        imulq -16(%rbp), %rax 
        movq %rax, -16(%rbp)

        # LoadIntConst(1, recurse_return)
        movq $1, -16(%rbp)

        # Call(+, [i,recurse_return], recurse_return)
        movq -16(%rbp), %rax 
        addq -24(%rbp), %rax 
        movq %rax, -16(%rbp)

        # Call(recurse, [recurse_return,recurse_return], recurse_return)
        movq -16(%rbp), %rdi
        movq -16(%rbp), %rsi
        call recurse
        movq %rax, -16(%rbp)

        # Copy(recurse_return, recurse_return)
        movq -16(%rbp), %rax
        movq %rax, -16(%rbp)

        # Label(.Lrecurse_1)
        .Lrecurse_1:

        # Return(recurse_return)
        movq -16(%rbp), %rax

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
        subq $16, %rsp
        # param backups (0)

        # LoadIntConst(5, n)
        movq $5, -8(%rbp)

        # LoadIntConst(0, main_return)
        movq $0, -16(%rbp)

        # Call(recurse, [n,main_return], main_return)
        movq -8(%rbp), %rdi
        movq -16(%rbp), %rsi
        call recurse
        movq %rax, -16(%rbp)

        # Call(print_int, [main_return], main_return)
        movq -16(%rbp), %rsi
        movq $print_format, %rdi
        call printf

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


