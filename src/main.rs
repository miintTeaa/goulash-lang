use goulash::{
    int_ir::{builder::build, visitor::IIRStmtVisitor},
    interpreter::{Interpreter, IntpControlFlow},
    value::Value,
};

fn main() {
    let src = String::from(r#"let x = 2; print (!false);"#);
    let src = &src;
    let (errors, stmts) = goulash::parse(src);

    if !errors.is_empty() {
        for error in errors {
            println!("{error}");
        }
        return;
    }

    let mut interpreter = Interpreter::new(src);
    match build(src, stmts) {
        Ok(iir) => {
            for iir_stmt in &iir {
                match interpreter.visit_stmt(iir_stmt) {
                    IntpControlFlow::Ret(Value::None) => {
                        eprintln!("Internal error");
                        break;
                    }
                    _ => (),
                }
            }
        }
        Err(_) => todo!(),
    }
}
