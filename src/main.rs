use std::fs;
use std::io::Write;
use std::path::Path;
use std::env;
use std::io;
use std::io::BufRead;
use std::process;

use mods::parser::lexer;

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
        let mut tokenizer = lexer::Tokenizer::new(line);
        loop {
            let tkn = tokenizer.get_next();
            match tkn {
                Ok(tkn) => println!("{:?}", tkn),
                Err(lexer::TknError::Eof) => break,
                Err(e) => panic!("unexpected error: {:?}", e)
            }
        }
    }
}

fn main() -> io::Result<()>
{
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        die("Usage: monkey <program path> or just monkey");
    }
    if args.len() == 2 {
        let text = get_text(&args[1])?;
        let mut tokenizer = lexer::Tokenizer::new(text);
        let _tkn = tokenizer.get_next();
    } else {
        repl()?;
    }

    Ok(())
}
