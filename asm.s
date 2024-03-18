
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .rodata
scan_format:
.asciz "%ld"

print_format:
.asciz "%ld\n"

print_bool_format:
.asciz "%s\n"

true_str:  .string "true\n"
false_str: .string "false\n"

.text


        # Function(main())
        .global main
        .type main, @function
        main:
        pushq %rbp
        movq %rsp, %rbp
        subq $64, %rsp
        # param backups (0)
        movq $print_bool, -16(%rbp) # Save address of module function 'print_bool' to var 'print_bool'

        # LoadBoolConst(true, var_2: Bool)
        movq $1, -8(%rbp)

        # Call(print_bool: print_bool[: Bool] -> Unit, [var_2: Bool], var_1: Unit)
        movq -8(%rbp), %rdi
        call print_bool
        movq %rax, -24(%rbp)

        # LoadBoolConst(false, var_5: Bool)
        movq $0, -32(%rbp)

        # Call(print_bool: print_bool[: Bool] -> Unit, [var_5: Bool], var_4: Unit)
        movq -32(%rbp), %rdi
        call print_bool
        movq %rax, -40(%rbp)

        # LoadBoolConst(true, var_8: Bool)
        movq $1, -48(%rbp)

        # Call(print_bool: print_bool[: Bool] -> Unit, [var_8: Bool], var_7: Unit)
        movq -48(%rbp), %rdi
        call print_bool
        movq %rax, -56(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -64(%rbp), %rax
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

.global print_int
.type print_int, @function
print_int:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rsi
    movq $print_format, %rdi
    call printf
    movq %rbp, %rsp
    popq %rbp
    ret

.global print_bool
.type print_bool, @function
print_bool:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp

    testq %rdi, %rdi
    jz .Lprint_false
.Lprint_true:
    movq $true_str, %rdi
    jmp .Lprint

.Lprint_false:
    movq $false_str, %rdi

.Lprint:
    xorl %eax, %eax
    call printf

    movq %rbp, %rsp
    popq %rbp
    ret

.global read_int
.type read_int, @function
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



