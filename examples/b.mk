let counter = fn (x : int) -> int {
	if (x < 1000) {
		return counter(x + 1);
	} else {
		return x;
	}
};

counter(2);
