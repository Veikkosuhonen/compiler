
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
        subq $112, %rsp
        # param backups (0)
        movq $print_int, -16(%rbp) # Save address of module function 'print_int' to var 'print_int'
        movq $XD, -88(%rbp) # Save address of module function 'XD' to var 'XD'

        # LoadIntConst(1, var_1: Int)
        movq $1, -8(%rbp)

        # Call(print_int: print_int[: Int] -> Unit, [var_1: Int], var_3: Unit)
        movq -8(%rbp), %rdi
        call print_int
        movq %rax, -24(%rbp)

        # LoadIntConst(2, var_6: Int)
        movq $2, -32(%rbp)

        # Call(print_int: print_int[: Int] -> Unit, [var_6: Int], var_8: Unit)
        movq -32(%rbp), %rdi
        call print_int
        movq %rax, -40(%rbp)

        # LoadIntConst(3, var_11: Int)
        movq $3, -48(%rbp)

        # LoadIntConst(4, var_13: Int)
        movq $4, -56(%rbp)

        # Copy(var_13: Int, var_11: Int)
        movq -56(%rbp), %rax
        movq %rax, -48(%rbp)

        # LoadIntConst(5, var_15: Int)
        movq $5, -64(%rbp)

        # Copy(var_15: Int, var_11: Int)
        movq -64(%rbp), %rax
        movq %rax, -48(%rbp)

        # Call(print_int: print_int[: Int] -> Unit, [var_11: Int], var_17: Unit)
        movq -48(%rbp), %rdi
        call print_int
        movq %rax, -72(%rbp)

        # Call(print_int: print_int[: Int] -> Unit, [var_1: Int], var_20: Unit)
        movq -8(%rbp), %rdi
        call print_int
        movq %rax, -80(%rbp)

        # Call(XD: XD[p: Int] -> Int, [var_6: Int], var_23: Int)
        movq -32(%rbp), %rdi
        call XD
        movq %rax, -96(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -104(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lmain_end)
        .Lmain_end:

        # LoadIntConst(0, _return: Unknown)
        movq $0, %rax
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(XD(p))
        .global XD
        .type XD, @function
        XD:
        pushq %rbp
        movq %rsp, %rbp
        subq $32, %rsp
        # param backups (1)
        movq %rdi, -8(%rbp)
        movq $print_int, -16(%rbp) # Save address of module function 'print_int' to var 'print_int'

        # Call(print_int: print_int[: Int] -> Unit, [p: Int], var_1: Unit)
        movq -8(%rbp), %rdi
        call print_int
        movq %rax, -24(%rbp)

        # LoadIntConst(1, var_3: Unit)
        movq $1, -32(%rbp)

        # Copy(var_3: Unit, _return: Int)
        movq -32(%rbp), %rax
        # skip movq %rax, %rax

        # Jump(.LXD_end)
        jmp .LXD_end

        # LoadIntConst(5, _return: Int)
        movq $5, %rax

        # Label(.LXD_end)
        .LXD_end:
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

print_int:
    pushq %rbp
    movq %rsp, %rbp
        movq %rdi, %rsi
        movq $print_format, %rdi
        call printf
    movq %rbp, %rsp
    popq %rbp
    ret

print_bool:
    pushq %rbp
    movq %rsp, %rbp
        movq %rdi, %rsi
        movq $print_format, %rdi
        andq $0x1, %rsi
        call printf
    movq %rbp, %rsp
    popq %rbp
    ret

read_int:
    pushq %rbp
    movq %rsp, %rbp
        movq $scan_format, %rdi
        call scanf
        cmpq $1, %rax
        jne .Lerr
        movq %rsi, %rax
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


