use std::env;
use std::fs;
use std::io;
use std::path::Path;
use std::process;
use io::{Write, BufRead};

use mods::lib::parser;
use mods::lib::type_checker;
use mods::lib::type_checker::TypeCheck;
use mods::lib::evaluator;
use mods::lib::evaluator::Evaluate;

mod mods;

fn die(msg: &str) {
    println!("{}", msg);
    process::exit(1)
}

fn get_text(file_path: &str) -> io::Result<String> {
    if Path::new(file_path).exists() {
        fs::read_to_string(file_path)
    } else {
        let err_msg = std::format!("File does not exist: {}", file_path);
        Err(io::Error::other(err_msg))
    }
}

fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    loop {
        print!(">> ");
        io::stdout().flush()?;
        let line = stdin.lock().lines().next().unwrap().unwrap();
        let mut tc_ctx = type_checker::Context::new();
        let mut ev_ctx = evaluator::Context::new();
        match parser::Parser::parse(line) {
            Ok(prog) => {
                match tc_ctx.tc(&prog) {
                    Some(_) => {
                        match ev_ctx.eval(&prog) {
                            Some(_) => {}
                            _ => println!("Evaluation failed."),
                        }
                    }
                    _ => println!("Type checking failed."),
                }
            }
            Err(err) => println!("Error while parsing! {:?}", err)
        }
    }
}

fn interpret_file(file_path: &str) -> io::Result<()> {
    let source_code = get_text(file_path)?;
    let mut tc_ctx = type_checker::Context::new();
    let mut ev_ctx = evaluator::Context::new();
    match parser::Parser::parse(source_code) {
        Ok(prog) => {
            match tc_ctx.tc(&prog) {
                Some(_) => {
                    match ev_ctx.eval(&prog) {
                        Some(_) => {}
                        _ => println!("Evaluatin failed."),
                    }
                }
                _ => println!("Type checking failed."),
            }
        }
        Err(err) => println!("Error while parsing! {:?}", err)
    }
    Ok(())
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        die("Usage: monkey <program path> or just monkey");
    } else if args.len() == 2 {
        interpret_file(&args[1])?;
    } else {
        repl()?;
    }

    Ok(())
}
