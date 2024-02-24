
# Metadata for debuggers and other tools
.extern printf
.extern scanf

.section .text  # Begins code and data


        # Function(recurse)
        .global recurse
        .type recurse, @function
        recurse:
        pushq %rbp
        movq %rsp, %rbp
        subq $88, %rsp

        # Call(print_int, [p0], x0)
        movq %rdi, %rsi
        movq $print_format, %rdi
        call printf

        # LoadIntConst(10, x2)
        movq $10, -24(%rbp)

        # Call(>, [p1,x2], x3)
        xor %rax, %rax
        movq %rsi, %rdx
        cmpq -24(%rbp), %rdx
        setg %al
        movq %rax, -40(%rbp)

        # CondJump(x3, .Lrecurse_0, .Lrecurse_2)
        cmpq $0, -40(%rbp)
        jne ..Lrecurse_0
        jmp ..Lrecurse_2

        # Label(.Lrecurse_0)
        .Lrecurse_0:

        # Copy(p0, x1)
        movq %rdi, %rax
        movq %rax, -48(%rbp)

        # Jump(.Lrecurse_1)
        jmp ..Lrecurse_1

        # Label(.Lrecurse_2)
        .Lrecurse_2:

        # LoadIntConst(2, x4)
        movq $2, -56(%rbp)

        # Call(*, [x4,p0], x5)
        movq %rdi, %rax 
        imulq -56(%rbp), %rax 
        movq %rax, -64(%rbp)

        # LoadIntConst(1, x6)
        movq $1, -72(%rbp)

        # Call(+, [p1,x6], x7)
        movq -72(%rbp), %rax 
        addq %rsi, %rax 
        movq %rax, -80(%rbp)

        # Call(recurse, [x5,x7], x8)
        movq -64(%rbp), %rdi
        movq -80(%rbp), %rsi
        call recurse
        movq %rax, -88(%rbp)

        # Copy(x8, x1)
        movq -88(%rbp), %rax
        movq %rax, -48(%rbp)

        # Label(.Lrecurse_1)
        .Lrecurse_1:

        # Return(x1)
        movq -48(%rbp), %rax

        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret


        
        # Function(main)
        .global main
        .type main, @function
        main:
        pushq %rbp
        movq %rsp, %rbp
        subq $32, %rsp

        # LoadIntConst(5, x0)
        movq $5, -8(%rbp)

        # LoadIntConst(0, x1)
        movq $0, -16(%rbp)

        # Call(recurse, [x0,x1], x2)
        movq -8(%rbp), %rdi
        movq -16(%rbp), %rsi
        call recurse
        movq %rax, -24(%rbp)

        # Call(print_int, [x2], x3)
        movq -24(%rbp), %rsi
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


