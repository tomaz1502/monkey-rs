use std::fmt;
use std::fs;
use std::path::Path;

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

pub fn get_text(file_path: &str) -> Result<String, String> {
    if Path::new(file_path).exists() {
        fs::read_to_string(file_path).map_err(|e| e.to_string())
    } else {
        let err_msg = std::format!("File does not exist: {}", file_path);
        Err(err_msg)
    }
}

