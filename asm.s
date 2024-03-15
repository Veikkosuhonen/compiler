
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .text  # Begins code and data


        # Function(quadratic(x))
        .global quadratic
        .type quadratic, @function
        quadratic:
        pushq %rbp
        movq %rsp, %rbp
        subq $16, %rsp
        # param backups (1)
        movq %rdi, -8(%rbp)

        # LoadIntConst(2, _return: Int)
        movq $2, %rax

        # Call(*: [: Pointer<T>] -> T, [_return: Int,x: Int], _return: Int)
        # skip movq %rax, %rax
        imulq -8(%rbp), %rax
        # skip movq %rax, %rax

        # Call(*: [: Pointer<T>] -> T, [_return: Int,x: Int], _return: Int)
        # skip movq %rax, %rax
        imulq -8(%rbp), %rax
        # skip movq %rax, %rax

        # LoadIntConst(2, var_2: Int)
        movq $2, -16(%rbp)

        # Call(-: [: Int] -> Int, [_return: Int,var_2: Int], _return: Int)
        # skip movq %rax, %rax
        subq -16(%rbp), %rax
        # skip movq %rax, %rax

        # Copy(_return: Int, _return: Int)
        # skip movq %rax, %rax
        # skip movq %rax, %rax

        # Label(.Lquadratic_end)
        .Lquadratic_end:
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

        # Call(&: [: T] -> Pointer<T>, [quadratic: [x: Int] -> Int], var_1: Pointer<[x: Int] -> Int>)
        leaq $quadratic, %rax
        movq %rax, -8(%rbp)

        # LoadIntConst(2, var_7: Int)
        movq $2, -16(%rbp)

        # Call(var_1: Pointer<[x: Int] -> Int>, [var_7: Int], var_6: Int)
        movq -16(%rbp), %rdi
        call var_1
        movq %rax, -24(%rbp)

        # Call(print_int: [: Int] -> Unit, [var_6: Int], var_4: Unit)
        movq -24(%rbp), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -32(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -40(%rbp), %rax
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


