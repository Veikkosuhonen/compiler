
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .text  # Begins code and data


        # Function(call(f, x))
        .global call
        .type call, @function
        call:
        pushq %rbp
        movq %rsp, %rbp
        subq $32, %rsp
        # param backups (2)
        movq %rdi, -8(%rbp)
        movq %rsi, -16(%rbp)

        # Call(f: [: Int] -> Unit, [x: Int], var_1: Unit)
        movq -16(%rbp), %rdi
        movq -8(%rbp), %rax
        # skip movq %rax, %rax
        call *%rax
        movq %rax, -24(%rbp)

        # Copy(U: Unit, _return: Unit)
        movq -32(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lcall_end)
        .Lcall_end:
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
        subq $80, %rsp
        # param backups (0)
        movq $get_five, -8(%rbp) # Save address of module function 'get_five' to var 'get_five'
        movq $add, -32(%rbp) # Save address of module function 'add' to var 'add'
        movq $call, -48(%rbp) # Save address of module function 'call' to var 'call'
        movq $print_int, -56(%rbp) # Save address of module function 'print_int' to var 'print_int'

        # Call(get_five: get_five[] -> Int, [], var_12: Int)
        call get_five
        movq %rax, -16(%rbp)

        # Call(get_five: get_five[] -> Int, [], var_14: Int)
        call get_five
        movq %rax, -24(%rbp)

        # Call(add: add[x: Int, y: Int] -> Int, [var_12: Int,var_14: Int], var_10: Int)
        movq -16(%rbp), %rdi
        movq -24(%rbp), %rsi
        call add
        movq %rax, -40(%rbp)

        # Call(call: call[f: [: Int] -> Unit, x: Int] -> Unit, [print_int: print_int[: Int] -> Unit,var_10: Int], var_7: Unit)
        movq -56(%rbp), %rdi
        movq -40(%rbp), %rsi
        call call
        movq %rax, -64(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -72(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lmain_end)
        .Lmain_end:

        # LoadIntConst(0, _return: Unknown)
        movq $0, %rax
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(add(x, y))
        .global add
        .type add, @function
        add:
        pushq %rbp
        movq %rsp, %rbp
        subq $32, %rsp
        # param backups (2)
        movq %rdi, -8(%rbp)
        movq %rsi, -16(%rbp)

        # Call(+: [: Int, : Int] -> Int, [x: Int,y: Int], _return: Int)
        movq -8(%rbp), %rax
        addq -16(%rbp), %rax
        # skip movq %rax, %rax

        # Copy(_return: Int, _return: Int)
        # skip movq %rax, %rax
        # skip movq %rax, %rax

        # Label(.Ladd_end)
        .Ladd_end:
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(get_five())
        .global get_five
        .type get_five, @function
        get_five:
        pushq %rbp
        movq %rsp, %rbp
        subq $0, %rsp
        # param backups (0)

        # LoadIntConst(5, _return: Int)
        movq $5, %rax

        # Copy(_return: Int, _return: Int)
        # skip movq %rax, %rax
        # skip movq %rax, %rax

        # Label(.Lget_five_end)
        .Lget_five_end:
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


