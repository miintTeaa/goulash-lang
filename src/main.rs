use goulash::{
    iir::{self, visitor::IIRStmtVisitor},
    interpreter::{Interpreter, IntpControlFlow},
    value::Value,
};

fn main() {
    let src = String::from(
        r#"
    print("Expected output: Test, 10, 1, 2, 3, 20");
    let x = 2;
    print ({
        x = "Test";
        print (x_str);
        10
    });
    print (
        fn a, b, c {
            print (a);
            print (b);
            print (c);
            20
        } (1, 2, 3)
    );
    "#,
    );
    let src = &src;

    let (errors, stmts) = goulash::parse(src);

    if !errors.is_empty() {
        for error in errors {
            println!("{error}");
        }
        return;
    }

    let mut interpreter = Interpreter::new(src);
    match iir::build(src, stmts) {
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
