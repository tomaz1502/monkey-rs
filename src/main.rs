use std::env;
use std::fs;
use std::io;
use std::path::Path;
use io::{Write, BufRead};

use mods::lib::parser;
use mods::lib::type_checker;
use mods::lib::type_checker::TypeCheck;
use mods::lib::evaluator;
use mods::lib::evaluator::Evaluate;

mod mods;

fn get_text(file_path: &str) -> Result<String, String> {
    if Path::new(file_path).exists() {
        fs::read_to_string(file_path).map_err(|e| e.to_string())
    } else {
        let err_msg = std::format!("File does not exist: {}", file_path);
        Err(err_msg)
    }
}

fn process(text: String,
           tc_ctx: &mut type_checker::Context,
           ev_ctx: &mut evaluator::Context)
           -> Result<(), String> {
    let prog = parser::Parser::parse(text)?;
    println!("prog = {prog:?}");
    for stmt in prog.stmts {
        tc_ctx.tc(&stmt).ok_or("Typing error.".to_string())?;
        ev_ctx.eval(&stmt).ok_or("Evaluation error.".to_string())?;
    }
    Ok(())
}

fn repl() -> Result<(), String> {
    let stdin = io::stdin();
    let mut tc_ctx = type_checker::Context::new();
    let mut ev_ctx = evaluator::Context::new();
    loop {
        print!(">> ");
        io::stdout().flush().map_err(|e| e.to_string())?;
        let line = stdin.lock().lines().next().unwrap().unwrap();
        process(line, &mut tc_ctx, &mut ev_ctx)?;
    }
}

fn interpret_file(file_path: &str) -> Result<(), String> {
    let source_code = get_text(file_path)?;
    let mut tc_ctx = type_checker::Context::new();
    let mut ev_ctx = evaluator::Context::new();
    process(source_code, &mut tc_ctx, &mut ev_ctx)?;
    Ok(())
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        return Err("Usage: monkey <program_path>?".to_string());
    } else if args.len() == 2 {
        interpret_file(&args[1])?;
    } else {
        repl()?;
    }

    Ok(())
}
