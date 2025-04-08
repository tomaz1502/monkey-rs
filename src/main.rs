use std::env;
use std::fs;
use std::io;
use std::path::Path;
use std::process;
use io::{Write, BufRead};

use mods::lib::lexer;
use mods::lib::parser;

mod mods;

fn die(msg: &str) -> ()
{
    println!("{}", msg);
    process::exit(1)
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

fn repl() -> io::Result<()>
{
    let stdin = io::stdin();
    loop {
        print!(">> ");
        io::stdout().flush()?;
        let line = stdin.lock().lines().next().unwrap().unwrap();
        match parser::Parser::parse(line) {
            Ok(prog) => println!("{:?}", prog),
            Err(_) => todo!(),
        }
    }
}

fn main() -> io::Result<()>
{
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        die("Usage: monkey <program path> or just monkey");
    }
    else if args.len() == 2 {
        let text = get_text(&args[1])?;
        let mut tokenizer = lexer::Lexer::new(text);
        let _tkn = tokenizer.get_next_token();
    } else {
        repl()?;
    }

    Ok(())
}
