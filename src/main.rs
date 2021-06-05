use logos::*;
use sierra::{
    gc::{Gc, GcObject},
    parse, scm_init,
    synpass::SynPass,
    Lex, ScmHashtable, ScmString, Token, Value,
};
use std::{collections::HashSet, iter::Peekable, rc::Rc};

const SRC: &'static str = r#"
    (defun add (x y) (+ x y 42))
"#;

fn main() {
    scm_init();
    let lex = Lex::new(SRC);
    let source = parse(lex).unwrap();
    let transformed = SynPass::new(source).transform_all().unwrap();
    println!("{:?}", Value::make_pointer(transformed));
}
