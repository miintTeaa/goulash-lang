use goulash::{
    iir::{self, visitor::IIRStmtVisitor},
    interpreter::{Interpreter, IntpControlFlow},
    value::Value,
};

fn main() {
    let src = String::from(
        r#"
    let x = 2;
    print ({
        x = "Test";
        print (x_str);
        10
    });
    x_str = fn a, b, c {
        print (a);
        print (b);
        print (c);
        20
    };
    print (
        x_str_fn(1, 2, 3)
    );
    x_str_fn = obj TestObj {
        to_int = fn self {
            print("transforming into int");
            self.foo
        }
        op_eq = fn self, other {
            print("checking if eq");
            self.foo == other.foo
        }
        foo = 42
    };
    print(x_str_fn_TestObj);
    print(x_str_fn_TestObj.foo);
    let y = obj TestObjTwo extends x_str_fn_TestObj {
        bar = 20
        baz = 30
        op_eq = fn self, other {
            print("checking if eq in testobjtwo");
            self.foo == other.foo
        }
    };
    print(y);
    print(y.bar);
    print(y.foo);
    y.foo = 10;
    print(x_str_fn_TestObj.foo);
    print(y.foo);
    print(x_str_fn_TestObj >= y);
    y.foo = y.foo;
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
