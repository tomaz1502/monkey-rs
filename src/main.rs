use std::env;
use std::fs;
use std::io;
use std::path::Path;
use std::process;
use io::{Write, BufRead};

use mods::lib::parser;
use mods::lib::type_checker;
use mods::lib::type_checker::TypeCheck;

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
        match parser::Parser::parse(line) {
            Ok(prog) => println!("{:?}", prog),
            Err(err) => println!("Error while parsing! {:?}", err)
        }
    }
}

fn interpret_file(file_path: &str) -> io::Result<()> {
    let source_code = get_text(file_path)?;
    let mut ctx = type_checker::Context::new();
    match parser::Parser::parse(source_code) {
        Ok(prog) => {
            let opt_typ = ctx.scope_tc(vec![], &prog);
            match opt_typ {
                None => println!("Typing error."),
                Some(_typ) => {
                    println!("Typing OK.");
                    // println!("\n--------------------------------------------------------\n");
                    // println!("AST: {:?}", prog);
                    // println!("\n--------------------------------------------------------\n");
                    // println!("TYPE: {:?}", typ);
                    // println!("\n--------------------------------------------------------\n");
                    // let res = prog.eval();
                    // println!("RESULT: {:?}", res)
                }
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
