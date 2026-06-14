let counter = fn(x : int) -> bool {
	if (x > 100) {
		return true;
    } else {
		let foobar = 9999;
		counter(x + 1);
    };
};
