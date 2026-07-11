let charOfDigit =
    fn (i : int) -> char {
        if (i == 0) {
            return '0';
        } else if (i == 1) {
            return '1';
        } else if (i == 2) {
            return '2';
        } else if (i == 3) {
            return '3';
        } else if (i == 4) {
            return '4';
        } else if (i == 5) {
            return '5';
        } else if (i == 6) {
            return '6';
        } else if (i == 7) {
            return '7';
        } else if (i == 8) {
            return '8';
        } else if (i == 9) {
            return '9';
        } else {
            return 'x';
        };
    };

let digitOfChar =
    fn (s : char) -> int {
        if (s == '0') {
            return 0;
        } else if (s == '1') {
            return 1;
        } else if (s == '2') {
            return 2;
        } else if (s == '3') {
            return 3;
        } else if (s == '4') {
            return 4;
        } else if (s == '5') {
            return 5;
        } else if (s == '6') {
            return 6;
        } else if (s == '7') {
            return 7;
        } else if (s == '8') {
            return 8;
        } else if (s == '9') {
            return 9;
        } else {
            return 42;
        };
    };

let strOfInt =
    fn (i : int) -> string {
        if (i < 0) {
            return concat("-", strOfInt(-i));
        } else if (i < 10) {
            return strOfChar(charOfDigit(i));
        } else {
            let r = strOfInt(i / 10);
            let c = charOfDigit(i % 10);
            return concat(r, strOfChar(c));
        };
    };

let strOfBool =
    fn (b : bool) -> string {
        if (b) {
            return "true";
        } else {
            return "false";
        };
    };

let intOfStr =
    fn (s : string) -> int {
        let r = fn (s : string, idx : int) -> int {
            if (idx < 0) {
                return 0;
            } else {
                let d = digitOfChar(getElem(s, idx));
                return d + 10 * r(s, idx - 1);
            };
        };
        return r(s, len(s) - 1);
    };

let println =
    fn (s : string) -> unit {
        print(s);
        print("\n");
    };

let printlnBool =
    fn (b : bool) -> unit {
        println(strOfBool(b));
    };

let printlnInt =
    fn (i : int) -> unit {
        println(strOfInt(i));
    };

let isPrime =
    fn (n : int) -> bool {
        let check = fn(d : int) -> bool {
            if (d * d > n) {
                return true;
            } else if (n % d == 0) {
                return false;
            } else {
                return check(d + 1);
            };
        };
        return check(2);
    };
