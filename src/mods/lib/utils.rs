use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltinSymbol {
    Print,
    Read,
    Len,
    StrOfChar,
    Concat,
    GetElem,
    GetSlice,
}

impl fmt::Display for BuiltinSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_> ) -> fmt::Result {
        let s = match self {
            BuiltinSymbol::Print     => "print",
            BuiltinSymbol::Read      => "read",
            BuiltinSymbol::Len       => "len",
            BuiltinSymbol::StrOfChar => "strOfChar",
            BuiltinSymbol::Concat    => "concat",
            BuiltinSymbol::GetElem   => "getElem",
            BuiltinSymbol::GetSlice  => "getSlice",
        };
        write!(f, "{}", s)
    }
}

pub static RESERVED_WORDS : [&str; 18] =
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
  , "strOfChar"
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
