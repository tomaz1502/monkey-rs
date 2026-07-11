load "examples/stdlib.mk";

let collatz =
    fn (x : int) -> int {
        printlnInt(x);
        if (x == 1) {
            return 1;
        } else if (x % 2 == 0) {
            return collatz(x / 2);
        } else {
            return collatz(3 * x + 1);
        };
    };

let i = intOfStr(read());
printlnInt(collatz(i));
