pub static RESERVED_WORDS : [&str; 17] =
  [ "len"
  , "print"
  , "read"
  , "let"
  , "fn"
  , "false"
  , "true"
  , "return"
  , "if"
  , "else"
  , "int"
  , "bool"
  , "char"
  , "string"
  , "unit"
  , "uu"
  , "getSlice"
  ];

pub fn unescape(input: &str) -> String {
    let mut out = String::new();
    let mut chars = input.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }

    out
}
