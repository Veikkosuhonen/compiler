def fibonacci(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fibonacci(n-1) + fibonacci(n-2)


i = 0
while i < 2000:
    print(fibonacci(11))
    i += 1

