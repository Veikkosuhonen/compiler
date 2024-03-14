
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
        subq $128, %rsp
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

        # Call(print_int: [: Int] -> Unit, [var_1: Int], var_4: Unit)
        movq -8(%rbp), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -16(%rbp)

        # Call(read_int: [] -> Int, [], var_8: Int)
        movq $scan_format, %rdi
        call scanf
        cmpq $1, %rax
        jne .Lerr
        movq %rsi, %rax
        movq %rax, -48(%rbp)

        # Label(.Lmain_0)
        .Lmain_0:

        # LoadIntConst(1, var_10: Int)
        movq $1, -56(%rbp)

        # Call(>: [: Int, : Int] -> Bool, [var_8: Int,var_10: Int], var_9: Bool)
        xor %rax, %rax
        movq -48(%rbp), %rdx
        cmpq -56(%rbp), %rdx
        setg %al
        movq %rax, -64(%rbp)

        # CondJump(var_9: Bool, .Lmain_1, .Lmain_2)
        cmpq $0, -64(%rbp)
        jne .Lmain_1
        jmp .Lmain_2

        # Label(.Lmain_1)
        .Lmain_1:

        # LoadIntConst(2, var_14: Int)
        movq $2, -72(%rbp)

        # Call(%: [: Int, : Int] -> Int, [var_8: Int,var_14: Int], var_13: Bool)
        movq -48(%rbp), %rax
        cqto
        idivq -72(%rbp)
        movq %rdx, %rax
        movq %rax, -80(%rbp)

        # LoadIntConst(0, var_15: Int)
        movq $0, -88(%rbp)

        # Call(==: [: T, : T] -> Bool, [var_13: Bool,var_15: Int], var_13: Bool)
        xor %rax, %rax
        movq -80(%rbp), %rdx
        cmpq -88(%rbp), %rdx
        sete %al
        movq %rax, -80(%rbp)

        # CondJump(var_13: Bool, .Lmain_3, .Lmain_5)
        cmpq $0, -80(%rbp)
        jne .Lmain_3
        jmp .Lmain_5

        # Label(.Lmain_3)
        .Lmain_3:

        # LoadIntConst(2, var_18: Int)
        movq $2, -96(%rbp)

        # Call(/: [: Int, : Int] -> Int, [var_8: Int,var_18: Int], var_17: Int)
        movq -48(%rbp), %rax
        cqto
        idivq -96(%rbp)
        movq %rax, -104(%rbp)

        # Copy(var_17: Int, var_8: Int)
        movq -104(%rbp), %rax
        movq %rax, -48(%rbp)

        # Copy(U: Unit, var_12: Unit)
        movq -32(%rbp), %rax
        movq %rax, -112(%rbp)

        # Copy(var_12: Unit, var_12: Unit)
        movq -112(%rbp), %rax
        movq %rax, -112(%rbp)

        # Jump(.Lmain_4)
        jmp .Lmain_4

        # Label(.Lmain_5)
        .Lmain_5:

        # LoadIntConst(3, var_17: Int)
        movq $3, -104(%rbp)

        # Call(*: [: Pointer<T>] -> T, [var_17: Int,var_8: Int], var_17: Int)
        movq -104(%rbp), %rax
        imulq -48(%rbp), %rax
        movq %rax, -104(%rbp)

        # LoadIntConst(1, var_19: Int)
        movq $1, -120(%rbp)

        # Call(+: [: Int, : Int] -> Int, [var_17: Int,var_19: Int], var_17: Int)
        movq -104(%rbp), %rax
        addq -120(%rbp), %rax
        movq %rax, -104(%rbp)

        # Copy(var_17: Int, var_8: Int)
        movq -104(%rbp), %rax
        movq %rax, -48(%rbp)

        # Copy(U: Unit, var_12: Unit)
        movq -32(%rbp), %rax
        movq %rax, -112(%rbp)

        # Copy(var_12: Unit, var_12: Unit)
        movq -112(%rbp), %rax
        movq %rax, -112(%rbp)

        # Label(.Lmain_4)
        .Lmain_4:

        # Call(print_int: [: Int] -> Unit, [var_8: Int], var_17: Unit)
        movq -48(%rbp), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -104(%rbp)

        # Copy(U: Unit, var_11: Unit)
        movq -32(%rbp), %rax
        movq %rax, -128(%rbp)

        # Copy(var_11: Unit, _return: Unknown)
        movq -128(%rbp), %rax
        # skip movq %rax, %rax

        # Jump(.Lmain_0)
        jmp .Lmain_0

        # Label(.Lmain_2)
        .Lmain_2:

        # Copy(_return: Unknown, _return: Unknown)
        # skip movq %rax, %rax
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


