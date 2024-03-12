
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
        subq $64, %rsp
        # param backups (1)
        movq %rdi, 0(%rbp)

        # Copy(dog.size: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, var_1: Int)
        movq 0(%rbp), %rdx
        movq (%rdx), %rax
        movq %rax, -8(%rbp)

        # Call(print_int: [: Int] -> Unit, [var_1: Int], var_0: Unit)
        movq -8(%rbp), %rsi
        movq $print_format, %rdi
        call printf

        # Copy(dog.isHungry: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, var_3: Bool)
        movq 0(%rbp), %rdx
        addq $8, %rdx
        movq (%rdx), %rax
        movq %rax, -24(%rbp)

        # Call(print_bool: [: Bool] -> Unit, [var_3: Bool], var_2: Unit)
        movq -24(%rbp), %rsi
        andq $0x1, %rsi
        movq $print_format, %rdi
        call printf

        # Copy(dog.tail: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, var_6: Pointer<Tail [length: Int]>)
        movq 0(%rbp), %rdx
        addq $16, %rdx
        movq (%rdx), %rax
        movq %rax, -40(%rbp)

        # Copy(var_6.length: Pointer<Tail [length: Int]>, var_5: Int)
        movq -40(%rbp), %rdx
        movq (%rdx), %rax
        movq %rax, -48(%rbp)

        # Call(print_int: [: Int] -> Unit, [var_5: Int], var_4: Unit)
        movq -48(%rbp), %rsi
        movq $print_format, %rdi
        call printf

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
        subq $48, %rsp
        # param backups (0)

        # LoadIntConst(10, var_1: Int)
        movq $10, 0(%rbp)

        # LoadBoolConst(true, var_2: Bool)
        movq $1, -8(%rbp)

        # LoadIntConst(5, var_3: Int)
        movq $5, -16(%rbp)

        # Call(create_dog: [size: Int, isHungry: Bool, tail_length: Int] -> Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, [var_1: Int,var_2: Bool,var_3: Int], var_0: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>)
        movq 0(%rbp), %rdi
        movq -8(%rbp), %rsi
        movq -16(%rbp), %rdx
        call create_dog
        movq %rax, -24(%rbp)

        # Copy(var_0: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, var_5: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>)
        movq -24(%rbp), %rax
        movq %rax, -32(%rbp)

        # Call(print_dog: [dog: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>] -> Unit, [var_5: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>], var_4: Unit)
        movq -32(%rbp), %rdi
        call print_dog
        movq %rax, -40(%rbp)

        # Copy(var_0: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>, _return: Unknown)
        movq -24(%rbp), %rax
        # skip movq %rax, %rax

        # Call(delete: [: Pointer<T>] -> Unit, [_return: Unknown], _return: Unknown)
        movq %rax, %rdi
        call free

        # Label(.Lmain_end)
        .Lmain_end:
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
        subq $64, %rsp
        # param backups (3)
        movq %rdi, -24(%rbp)
        movq %rsi, -32(%rbp)
        movq %rdx, -48(%rbp)

        # Declare(var_0: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>])
        

        # Copy(size: Int, var_0.size: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>])
        movq -24(%rbp), %rax
        movq %rax, -16(%rbp)

        # Copy(isHungry: Bool, var_0.isHungry: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>])
        movq -32(%rbp), %rax
        movq %rax, -8(%rbp)

        # Declare(var_1: Tail [length: Int])
        

        # Copy(tail_length: Int, var_1.length: Tail [length: Int])
        movq -48(%rbp), %rax
        movq %rax, -40(%rbp)

        # Call(new: [: Constructor<T>] -> Pointer<T>, [var_1: Tail [length: Int]], var_0.tail: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>])
        movq $8, %rdi
        call malloc
        test %rax, %rax
        jz .Lend
        movq -40(%rbp), %rax
        # skip movq %rax, %rax
        movq %rax, 0(%rbp) # Move address of var_1 to var_0

        # Call(new: [: Constructor<T>] -> Pointer<T>, [var_0: Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]], _return: Pointer<Dog [size: Int, isHungry: Bool, tail: Pointer<Tail [length: Int]>]>)
        movq $24, %rdi
        call malloc
        test %rax, %rax
        jz .Lend
        movq -16(%rbp), %rax
        # skip movq %rax, %rax
        movq -8(%rbp), %rax
        # skip movq %rax, %rax
        movq 0(%rbp), %rax
        # skip movq %rax, %rax
        # skip movq %rax, %rax # Move address of var_0 to _return

        # Label(.Lcreate_dog_end)
        .Lcreate_dog_end:
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


