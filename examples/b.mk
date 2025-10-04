let counter = fn (x : int) -> int {
	if (x > 600) {
		return 42;
	} else {
		return counter(x + 1);
	}
};

counter(2);
