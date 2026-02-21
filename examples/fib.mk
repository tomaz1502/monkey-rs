let fib = fn (n : int) -> int {
	if (n == 0) {
		return 0;
	} else {
		if (n == 1) {
			return 01;
		} else {
			return fib(n - 1) + fib(n - 2);
		}
    }
};

fib(8);
fib(9);
fib(10);
