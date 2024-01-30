        # Metadata for debuggers and other tools
        .global main
        .type main, @function
        .extern printf
        .extern scanf

        .section .text  # Begins code and data
# Label that marks beginning of main function
main:
        # Function stack setup
        pushq %rbp
        movq %rsp, %rbp
        subq $128, %rsp  # Reserve 128 bytes of stack space

        # Read an integer into -8(%rbp)
        movq $scan_format, %rdi  # 1st param: "%ld"
        leaq -8(%rbp), %rsi      # 2nd param: (%rbp - 8)
        call scanf
        # If the input was invalid, jump to end
        cmpq $1, %rax
        jne .Lend

        # Read an integer into -8(%rbp)
        movq $scan_format, %rdi  # 1st param: "%ld"
        leaq -16(%rbp), %rsi      # 2nd param: (%rbp - 8)
        call scanf
        # If the input was invalid, jump to end
        cmpq $1, %rax
        jne .Lend

        movq -8(%rbp), %rax
        movq -16(%rbp), %rsi
        addq %rax, %rsi

        # *** REPLACE THESE TWO LINES WITH YOUR CODE ***
        # movq -8(%rbp), %rsi  # Copy input number to `%rsi`
        # imulq $2, %rsi  # Double the input

        # Call function 'printf("%ld\n", %rsi)'
        # to print the number in %rsi.
        movq $print_format, %rdi
        call printf

# Labels starting with ".L" are only visible inside this module
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
