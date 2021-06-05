use sierra::builtins::builtins_init;
use sierra::function::ScmFunction;
use sierra::gc::GC;
use sierra::vm;
use sierra::vm::scm_apply;
use sierra::{function::Compiler, parse, scm_init, Lex, Value};

#[global_allocator]
static ALLOC: GC = GC;
const SRC: &'static str = r#"
    (car (list 1 2 3 4))
   
"#;

fn main() {
    scm_init();
    builtins_init();
    let lex = Lex::new(SRC);
    let source = parse(lex).unwrap();
    let mut compiler = Compiler::new();
    vm().disasm = true;
    let code = ScmFunction::new(compiler.compile_sexprs(source).unwrap());

    let result = scm_apply(Value::make_pointer(code), &[]);
    match result {
        Ok(val) => println!("{:?}", val),
        Err(e) => eprintln!("Error: {:?}", e),
    }
}
