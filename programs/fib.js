
function fibonacci(n) {
    // print_int(n);
    if (n <= 0) {
        return 0
    } else {
        if (n == 1) {
            return 1
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2)
        }
    }
}

var i = 0;
while (i < 2000) {
    console.log(fibonacci(11));
    i = i + 1;
}
