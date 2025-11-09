let counter = fn (x : int) -> int {
	if (x < 630) {
		return counter(x + 1);
	} else {
		return x;
	}
};

counter(2);
