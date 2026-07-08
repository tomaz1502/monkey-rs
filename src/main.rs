use std::env;
use std::io;
use io::{Write, BufRead};

use monkey::utils::*;
use monkey::type_checker;
use monkey::type_checker::TypeCheck;
use monkey::evaluator;
use monkey::evaluator::Evaluate;
use monkey::parser;

fn process(text: String,
           tc_ctx: &mut type_checker::Context,
           ev_ctx: &mut evaluator::Context)
           -> Result<(), String> {
    let prog = parser::Parser::parse(text)?;
    for stmt in prog.stmts {
        tc_ctx.tc(&stmt).ok_or("Typing error.".to_string())?;
        ev_ctx.eval(&stmt).map_err(|_| "Evaluation error.".to_string())?;
    }
    Ok(())
}

fn repl(tc_ctx: &mut type_checker::Context,
        ev_ctx: &mut evaluator::Context) -> Result<(), String> {
    let stdin = io::stdin();
    loop {
        print!(">> ");
        io::stdout().flush().map_err(|e| e.to_string())?;
        let line = stdin.lock().lines().next().unwrap().unwrap();
        process(line, tc_ctx, ev_ctx)?;
    }
}

fn interpret_file(file_path: &str,
                  tc_ctx: &mut type_checker::Context,
                  ev_ctx: &mut evaluator::Context) -> Result<(), String> {
    let source_code = get_text(file_path)?;
    process(source_code, tc_ctx, ev_ctx)?;
    Ok(())
}

struct Config {
    loads: Vec<String>,
    main: Option<String>,
}

const USAGE: &'static str = "Usage: `monkey <file/path>? [ (-l | --load=)<path/to/lib.mk> ]*";

impl Config  {
    fn build(args: Vec<String>) -> Result<Self, &'static str> {
        let mut cfg = Config { loads: vec![], main: None };
        let mut i = 1;
        while i < args.len() - 1 {
            if args[i] == "-l" {
                cfg.loads.push(args[i + 1].clone());
                i += 2;
            } else if args[i].starts_with("--load=") {
                let p = &args[i][6..];
                cfg.loads.push(p.to_string());
                i += 1;
            } else {
                if cfg.main.is_some() {
                    return Err(USAGE);
                } else {
                    cfg.main = Some(args[i].clone());
                    i += 1;
                }
            }
        }
        Ok(cfg)
    }
}

fn run(cfg: Config) -> Result<(), String> {
    let mut tc_ctx = type_checker::Context::new();
    let mut ev_ctx = evaluator::Context::new();
    for load in cfg.loads {
        let source_code = get_text(&load)?;
        process(source_code, &mut tc_ctx, &mut ev_ctx)?;
    }
    if let Some(f) = cfg.main {
        interpret_file(&f, &mut tc_ctx, &mut ev_ctx)?;
    } else {
        repl(&mut tc_ctx, &mut ev_ctx)?;
    }
    Ok(())
}

fn main() -> Result<(), String> {
    let cfg = Config::build(env::args().collect())?;
    run(cfg)?;
    Ok(())
}
