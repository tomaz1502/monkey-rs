let fat = fn (n : int) -> int {
	if (n == 0) {
		return 1;
	} else {
		return n * fat(n - 1);
	}
};

fat(21);
