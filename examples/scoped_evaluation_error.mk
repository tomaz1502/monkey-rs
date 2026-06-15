{
	let f = fn(i: int) -> int -> int { return fn(j: int) -> int { return j; }; };

	let x = f(2)(3);

	if (x < 10) {
		print("lesser\n");
	} else {
		print("not lesser\n");
	};
	print("hello, world\n");
};
