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

fn process(text: String, tc_ctx: &mut type_checker::Context, ev_ctx: &mut evaluator::Context) -> io::Result<()> {
    let prog = parser::Parser::parse(text)?;
    for stmt in prog.stmts {
        tc_ctx.tc(&stmt).ok_or(std::io::Error::other("Typing error."))?;
        ev_ctx.eval(&stmt).ok_or(std::io::Error::other("Evaluation error."))?;
    }
    Ok(())
}

fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    let mut tc_ctx = type_checker::Context::new();
    let mut ev_ctx = evaluator::Context::new();
    loop {
        print!(">> ");
        io::stdout().flush()?;
        let line = stdin.lock().lines().next().unwrap().unwrap();
        process(line, &mut tc_ctx, &mut ev_ctx)?;
    }
}

fn interpret_file(file_path: &str) -> io::Result<()> {
    let source_code = get_text(file_path)?;
    let mut tc_ctx = type_checker::Context::new();
    let mut ev_ctx = evaluator::Context::new();
    process(source_code, &mut tc_ctx, &mut ev_ctx)?;
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
