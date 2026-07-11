load "examples/stdlib.mk";

let x = 10;

let f = fn() -> int { return x; };
let g = fn(x : int) -> int { return f(); };

let i = g(99);
printlnInt(i);
