use crate::gc::{GcCell, Trace, Tracer};

use super::Value;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    Label,
    PushConst(u32),
    LocalGet(u32, u32),
    LocalSet(u32, u32),
    GlobalGet(u32),
    GlobalSet(u32),
    StackPop,
    Duplicate,
    JmpIfTrue(i32),
    JmpIfFalse(i32),
    Jmp(i32),

    TailApply(u32),
    Return,
    Apply(u32),
    MakeEnv(u32),
    MakeEnvDot(u32),
    MakeClosure(u32),
    CallPrimop(u32),
}

#[derive(Clone)]
pub enum Trampoline {
    Apply(Value, Vec<Value>),
    Value(Value),
}

pub struct CodeBlock {
    pub code: Vec<Opcode>,
    pub literals: Vec<Value>,
}

impl GcCell for CodeBlock {}
unsafe impl Trace for CodeBlock {
    fn trace(&self, visitor: &mut Tracer) {
        self.literals.trace(visitor);
    }
}
