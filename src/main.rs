use std::{fs, process::ExitCode};

use goulash::{
    iir::{self, visitor::IIRStmtVisitor},
    interpreter::{Interpreter, IntpControlFlow},
    parser::parse,
    value::Value,
};

fn main() -> ExitCode {
    let mut args = std::env::args();
    let program_name = args
        .next()
        .expect("first argument isn't program name; this is a bug in your system");

    let src;
    if let Some(arg) = args.next() {
        src = match fs::read_to_string(arg) {
            Ok(src) => src,
            Err(e) => {
                eprintln!("{e}");
                return ExitCode::FAILURE;
            }
        };
    } else {
        eprintln!("Usage: {program_name} <filename>");
        return ExitCode::FAILURE;
    }

    let stmts = match parse(&src) {
        Ok(stmts) => stmts,
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::FAILURE;
        }
    };

    let mut interpreter = Interpreter::new(&src);
    match iir::build(&src, stmts) {
        Ok(iir) => {
            for iir_stmt in &iir {
                match interpreter.visit_stmt(iir_stmt) {
                    IntpControlFlow::Ret(Value::None) => {
                        eprintln!("Internal error");
                        return ExitCode::FAILURE;
                    }
                    _ => (),
                }
            }
            ExitCode::SUCCESS
        }
        Err(e) => {
            for error in e {
                eprintln!("{error}");
            }
            return ExitCode::FAILURE;
        }
    }
}
