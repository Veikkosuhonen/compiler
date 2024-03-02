
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
        subq $32, %rsp
        # param backups (0)

        # LoadIntConst(4, var_2)
        movq $4, -8(%rbp)

        # Call(square, [var_2], var_1)
        movq -8(%rbp), %rdi
        call square
        movq %rax, -16(%rbp)

        # Call(print_int, [var_1], var_0)
        movq -16(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # Copy(var_0, _return)
        movq -24(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lmain_end)
        .Lmain_end:
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(square(x))
        .global square
        .type square, @function
        square:
        pushq %rbp
        movq %rsp, %rbp
        subq $32, %rsp
        # param backups (1)
        movq %rdi, -8(%rbp)

        # Call(*, [x,x], var_0)
        movq -8(%rbp), %rax 
        imulq -8(%rbp), %rax 
        movq %rax, -16(%rbp)

        # Copy(var_0, _return)
        movq -16(%rbp), %rax
        # skip movq %rax, %rax

        # Jump(.Lsquare_end)
        jmp .Lsquare_end

        # Copy(var_0, _return)
        movq -16(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lsquare_end)
        .Lsquare_end:
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


