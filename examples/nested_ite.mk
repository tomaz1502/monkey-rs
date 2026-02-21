let f =
	fn (i : int) -> string {
		if (i == 0) {
			return "0";
		} else if (i == 1) {
			return "1";
		} else {
			return "2";
		}
	};

print(f(1));
print("\n");
print(f(2));
print("\n");
print(f(3));
print("\n");
print(f(4));
print("\n");
