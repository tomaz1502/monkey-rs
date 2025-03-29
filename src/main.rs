use std::fs;
use std::path::Path;
use std::env;
use std::io;
use std::process;

use mods::lexer::lexer;

mod mods;

fn die(msg: &str) -> ()
{
    println!("{}", msg);
    process::exit(1);
}

fn get_text(file_path: &str) -> io::Result<String>
{
    if Path::new(file_path).exists() {
        fs::read_to_string(file_path)
    } else {
        let err_msg = std::format!("File does not exist: {}", file_path);
        Err(io::Error::other(err_msg))
    }
}

fn main() -> io::Result<()>
{
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        die("Usage: monkey <program path>");
    }
    let text = get_text(&args[1])?;
    let mut tokenizer = lexer::Tokenizer::new(text);
    let _tkn = tokenizer.get_next();

    Ok(())
}
