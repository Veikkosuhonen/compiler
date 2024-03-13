
# Metadata for debuggers and other tools
.extern printf
.extern scanf
.extern malloc
.extern free

.section .text  # Begins code and data


        # Function(print_dog(dog))
        .global print_dog
        .type print_dog, @function
        print_dog:
        pushq %rbp
        movq %rsp, %rbp
        subq $80, %rsp
        # param backups (1)
        movq %rdi, -8(%rbp)

        # Call(print_int: [: Int] -> Unit, [dog.size: Int], var_1: Unit)
        movq -8(%rbp), %rdi # Put struct 'dog' base pointer in %rdi
        movq 0(%rdi), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -24(%rbp)

        # Call(print_bool: [: Bool] -> Unit, [dog.isHungry: Bool], var_4: Unit)
        movq -8(%rbp), %rdi # Put struct 'dog' base pointer in %rdi
        movq 8(%rdi), %rsi
        andq $0x1, %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -40(%rbp)

        # Call(print_int: [: Int] -> Unit, [dog.tail.length: Int], var_7: Unit)
        movq -8(%rbp), %rdi # Put struct 'dog' base pointer in %rdi
        movq 16(%rdi), %rdi # Put struct 'tail' base pointer in %rdi
        movq 0(%rdi), %rsi
        movq $print_format, %rdi
        call printf
        movq %rax, -64(%rbp)

        # Copy(U: Unit, _return: Unit)
        movq -72(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lprint_dog_end)
        .Lprint_dog_end:
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
        subq $112, %rsp
        # param backups (0)

        # LoadIntConst(10, var_3: Int)
        movq $10, -8(%rbp)

        # LoadBoolConst(true, var_4: Bool)
        movq $1, -16(%rbp)

        # LoadIntConst(5, var_5: Int)
        movq $5, -24(%rbp)

        # Call(create_dog: [size: Int, isHungry: Bool, tail_length: Int] -> Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, [var_3: Int,var_4: Bool,var_5: Int], var_2: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>)
        movq -8(%rbp), %rdi
        movq -16(%rbp), %rsi
        movq -24(%rbp), %rdx
        call create_dog
        movq %rax, -32(%rbp)

        # Call(print_dog: [dog: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>] -> Unit, [var_2: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>], var_7: Unit)
        movq -32(%rbp), %rdi
        call print_dog
        movq %rax, -40(%rbp)

        # LoadIntConst(20, var_10: Int)
        movq $20, -48(%rbp)

        # Copy(var_10: Int, var_2.size: Int)
        movq -32(%rbp), %rdi # Put struct 'var_2' base pointer in %rdi
        movq -48(%rbp), %rax
        movq %rax, 0(%rdi)

        # LoadBoolConst(false, var_12: Bool)
        movq $0, -64(%rbp)

        # Copy(var_12: Bool, var_2.isHungry: Bool)
        movq -32(%rbp), %rdi # Put struct 'var_2' base pointer in %rdi
        movq -64(%rbp), %rax
        movq %rax, 8(%rdi)

        # LoadIntConst(10, var_14: Int)
        movq $10, -80(%rbp)

        # Copy(var_14: Int, var_2.tail.length: Int)
        movq -32(%rbp), %rdi # Put struct 'var_2' base pointer in %rdi
        movq 16(%rdi), %rdi # Put struct 'tail' base pointer in %rdi
        movq -80(%rbp), %rax
        movq %rax, 0(%rdi)

        # Call(print_dog: [dog: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>] -> Unit, [var_2: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>], var_16: Unit)
        movq -32(%rbp), %rdi
        call print_dog
        movq %rax, -104(%rbp)

        # Copy(U: Unit, _return: Unknown)
        movq -112(%rbp), %rax
        # skip movq %rax, %rax

        # Label(.Lmain_end)
        .Lmain_end:

        # LoadIntConst(0, _return: Unknown)
        movq $0, %rax
        # Restore stack pointer
        movq %rbp, %rsp
        popq %rbp
        ret
    

        
        # Function(create_dog(size, isHungry, tail_length))
        .global create_dog
        .type create_dog, @function
        create_dog:
        pushq %rbp
        movq %rsp, %rbp
        subq $80, %rsp
        # param backups (3)
        movq %rdi, -8(%rbp)
        movq %rsi, -16(%rbp)
        movq %rdx, -24(%rbp)

        # Declare(var_0: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>])
        

        # Copy(size: Int, var_0.size: Int)
        movq -8(%rbp), %rax
        movq %rax, -48(%rbp)

        # Copy(isHungry: Bool, var_0.isHungry: Bool)
        movq -16(%rbp), %rax
        movq %rax, -40(%rbp)

        # Declare(var_1: Tail [length: Int])
        

        # Copy(tail_length: Int, var_1.length: Int)
        movq -24(%rbp), %rax
        movq %rax, -56(%rbp)

        # Call(new: [: Constructor<T>] -> Pointer<T>, [var_1: Tail [length: Int]], var_0.tail: Pointer<Tail [length: Int]>)
        movq $8, %rdi
        call malloc
        test %rax, %rax # Check that malloc succeeded
        jz .Lerr
        movq %rax, %rdi # Store block base address at %rdi
        movq -56(%rbp), %rax
        movq %rax, 0(%rdi)
        movq %rdi, %rax # Move the pointer to variable
        movq %rax, -32(%rbp)

        # Copy(var_0.tail: Pointer<Tail [length: Int]>, var_0.tail: Pointer<Tail [length: Int]>)
        movq -32(%rbp), %rax
        movq %rax, -32(%rbp)

        # Call(new: [: Constructor<T>] -> Pointer<T>, [var_0: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]], _return: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>)
        movq $24, %rdi
        call malloc
        test %rax, %rax # Check that malloc succeeded
        jz .Lerr
        movq %rax, %rdi # Store block base address at %rdi
        movq -48(%rbp), %rax
        movq %rax, 0(%rdi)
        movq -40(%rbp), %rax
        movq %rax, 8(%rdi)
        movq -32(%rbp), %rax
        movq %rax, 16(%rdi)
        movq %rdi, %rax # Move the pointer to variable
        # skip movq %rax, %rax

        # Copy(_return: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, _return: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>)
        # skip movq %rax, %rax
        # skip movq %rax, %rax

        # Label(.Lcreate_dog_end)
        .Lcreate_dog_end:
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


