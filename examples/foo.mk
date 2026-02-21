let f =
    fn (i : int) -> string {
		if (i == 0) {
			return "foo\n";
		} else {
			return "bar\n";
		}
	};

print(f(2));
