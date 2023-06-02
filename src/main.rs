use goulash::{
    iir::{self, visitor::IIRStmtVisitor},
    interpreter::{Interpreter, IntpControlFlow},
    parser::parse,
    value::Value,
};

fn main() {
    let src = String::from(
        r#"
    let println = fn x {
        print(x);
        print("\n");
    };
    let test = fn x { print("" + x + " "); x };
    println("1 2 3 4 11");
    let x = test(1) + test(2) * test(3) + test(4);
    print(x);
    println("");
    let prev = obj Test1 {
        shout2 = fn a, b {
            print("shouting " + a);
            println(" and " + b);
        };
    };
    let test2 = obj Test2 {
        shout3 = fn a, b, c {
            print("shouting " + a);
            print(" and " + b);
            println(" and also " + c)
        };
        a = 2;
    } extends prev;
    test2.shout3("hello " + "world", false, true);
    test2.shout2("hi", None);
    println("you should see two foos and no bars");
    false or print("foo ");
    true or print("bar ");
    true and print("foo ");
    false and print("bar ");
    println("");
    "#,
    );
    let src = &src;

    let stmts = match parse(src) {
        Ok(stmts) => stmts,
        Err(e) => {
            println!("{e}");
            return;
        }
    };

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
