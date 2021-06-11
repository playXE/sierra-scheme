use crate::gc::{GcPointer, Trace, Tracer};

use super::{cons::Cons, environment::Environment, instruction::CodeBlock};

pub struct Closure {
    pub code: GcPointer<CodeBlock>,
    pub args: GcPointer<Cons>,
    pub env: GcPointer<Environment>,
}

unsafe impl Trace for Closure {
    fn trace(&self, visitor: &mut Tracer) {
        self.code.trace(visitor);
        self.args.trace(visitor);
        self.env.trace(visitor);
    }
}
