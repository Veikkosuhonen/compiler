
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .text  # Begins code and data


        # Function(make_one())
        .global make_one
        .type make_one, @function
        make_one:
        pushq %rbp
        movq %rsp, %rbp
        subq $0, %rsp
        # param backups (0)

        # LoadIntConst(1, _return: Int)
        movq $1, %rax

        # Copy(_return: Int, _return: Int)
        # skip movq %rax, %rax
        # skip movq %rax, %rax

        # Label(.Lmake_one_end)
        .Lmake_one_end:
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(ret_ptr())
        .global ret_ptr
        .type ret_ptr, @function
        ret_ptr:
        pushq %rbp
        movq %rsp, %rbp
        subq $16, %rsp
        # param backups (0)
        movq $make_one, -16(%rbp) # Save address of module function 'make_one' to var 'make_one'

        # Call(&: [: T] -> Pointer<T>, [make_one: make_one[] -> Int], _return: Pointer<[] -> Int>)
        leaq -16(%rbp), %rax
        # skip movq %rax, %rax

        # Copy(_return: Pointer<[] -> Int>, _return: Pointer<[] -> Int>)
        # skip movq %rax, %rax
        # skip movq %rax, %rax

        # Label(.Lret_ptr_end)
        .Lret_ptr_end:
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
        movq $ret_ptr, -8(%rbp) # Save address of module function 'ret_ptr' to var 'ret_ptr'
        movq $print_int, -32(%rbp) # Save address of module function 'print_int' to var 'print_int'

        # Call(ret_ptr: ret_ptr[] -> Pointer<[] -> Int>, [], var_2: Pointer<[] -> Int>)
        call ret_ptr
        movq %rax, -16(%rbp)

        # Call(var_2.value: [] -> Int, [], var_8: Int)
        movq -16(%rbp), %rax # Put pointer address 'var_2' in %rax
        movq 0(%rax), %rax
        # skip movq %rax, %rax
        call *%rax
        movq %rax, -24(%rbp)

        # Call(print_int: print_int[: Int] -> Unit, [var_8: Int], var_6: Unit)
        movq -24(%rbp), %rdi
        call print_int
        movq %rax, -40(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -48(%rbp), %rax
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


