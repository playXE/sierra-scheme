use std::{
    cell::UnsafeCell,
    panic::{catch_unwind, resume_unwind, AssertUnwindSafe},
};

use crate::{function::*, gc::*, *};

pub struct SchemeThread {
    stack: Gc<ScmVector>,

    pub(crate) last_error: Value,
}

thread_local! {
    static THREAD: UnsafeCell<SchemeThread> = UnsafeCell::new(SchemeThread {
        stack: ScmVector::new_nil(4096),

        last_error: Value::make_nil(),
    });
}

pub fn scm_thread() -> &'static mut SchemeThread {
    unsafe { THREAD.with(|th| &mut *th.get()) }
}
struct SchemeFrame {
    func: Gc<ScmFunction>,
    env: Gc<ScmVarEnv>,
    ip: usize,
    acc: Value,
    rest: Value,
    stack: Gc<ScmVector>,
}

#[allow(dead_code)]
impl SchemeFrame {
    #[inline(always)]
    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }
    #[inline(always)]
    pub fn pop(&mut self) -> Value {
        self.stack.pop_back()
    }
}

unsafe fn interpret(frame: *mut SchemeFrame) -> Value {
    let mut frame = &mut *frame;
    let code = frame.func.code_block;

    loop {
        let op = *code.unlinked.get_unchecked(frame.ip);
        frame.ip += 1;

        match op {
            Op::True => frame.acc = Value::make_true(),
            Op::False => frame.acc = Value::make_false(),
            Op::Nil => frame.acc = Value::make_nil(),
            Op::Fixnum(x) => frame.acc = Value::make_fixnum(x),
            Op::Real(x) => frame.acc = Value::make_real(x),
            Op::GetEnv(mut depth) => {
                let mut env = frame.env;
                while depth != 0 {
                    env = env.upper.unwrap();
                    depth -= 1;
                }
                frame.acc = Value::make_pointer(env);
            }
            Op::GetVar0(x) => {
                frame.acc = frame.env.vars[x as usize];
            }
            Op::GetVar1(x) => {
                frame.acc = frame.env.upper.unwrap().vars[x as usize];
            }
            Op::GetVar(x) => {
                let env = frame.acc.ptr::<ScmVarEnv>();
                frame.acc = env.vars[x as usize];
            }
            Op::SetVar(x) => {
                let mut env = frame.acc.ptr::<ScmVarEnv>();
                env.vars[x as usize] = frame.stack.pop_back();
            }
            Op::SetVar0(x) => {
                frame.env.vars[x as usize] = frame.acc;
            }
            Op::SetVar1(x) => {
                frame.env.upper.unwrap().vars[x as usize] = frame.acc;
            }
            Op::SetGlobal(x) => {
                let sym = code.symbols[x as usize];
                vm_global_set(sym, frame.acc);
            }
            Op::GetGlobal(x) => {
                let sym = code.symbols[x as usize];
                frame.acc = vm_global_lookup(sym);
            }
            Op::Jump(x) => {
                frame.ip = (frame.ip as isize + x as isize) as usize;
            }
            Op::JumpFalse(x) => {
                if !frame.acc.to_bool() {
                    frame.ip = (frame.ip as isize + x as isize) as usize;
                }
            }
            Op::JumpTrue(x) => {
                if !frame.acc.to_bool() {
                    frame.ip = (frame.ip as isize + x as isize) as usize;
                }
            }
            Op::MakeClosure(x) => {
                let codeb = code.codes[x as usize].ptr::<CodeBlock>();
                let env = Gc::new(ScmVarEnv {
                    upper: Some(frame.env),
                    vars: ScmVector::new(codeb.var_count),
                });
                let func = Gc::new(ScmFunction {
                    code_block: codeb,
                    env,
                    hdr: Header::new(ScmType::Closure),
                });
                frame.acc = Value::make_pointer(func);
            }

            Op::Return => {
                return frame.acc;
            }
            Op::PopAcc => {
                frame.acc = frame.stack.pop_back();
            }
            Op::PushAcc => {
                frame.stack.push(frame.acc);
            }
            Op::Rest => {
                frame.acc = frame.rest;
            }
            Op::Apply(argc) => {
                let args = &frame.stack.slice_from(frame.stack.size - argc as usize);
                match scm_apply(frame.acc, args) {
                    Ok(val) => frame.acc = val,
                    Err(e) => vm_raise(e),
                }
            }
            Op::GetLiteral(x) => {
                frame.acc = code.literals[x as usize];
            }
            Op::GetSym(x) => {
                frame.acc = code.symbols[x as usize];
            }
            _ => todo!(),
        }
    }
}

pub fn scm_apply(value: Value, args: &[Value]) -> Result<Value, Value> {
    let error = catch_unwind(AssertUnwindSafe(|| unsafe {
        if value.closurep() {
            return __scm_apply_clos(value.closure(), args);
        }
        if value.nativep() {
            let native = value.native();
            return (native.clos)(args);
        }
        vm_raise(Value::make_pointer(ScmRuntimeError::new(format!(
            "Cannot apply '{:?}'",
            value
        ))))
    }));
    match error {
        Ok(val) => return Ok(val),
        Err(val) => {
            if val.is::<SchemeError>() {
                return Err(scm_thread().last_error);
            }
            resume_unwind(val);
        }
    }
}

unsafe fn __scm_apply_clos(clos: Gc<ScmFunction>, args: &[Value]) -> Value {
    let mut frame = SchemeFrame {
        env: Gc::new(ScmVarEnv {
            upper: Some(clos.env),
            vars: ScmVector::new_nil(clos.code_block.var_count),
        }),
        rest: Value::make_nil(),
        func: clos,
        stack: scm_thread().stack,
        ip: 0,
        acc: Value::make_nil(),
    };
    if args.len() < clos.code_block.argc_on_stack {
        vm_raise(Value::make_pointer(ScmRuntimeError::new(format!(
            "Expected at least {} argument(s) but found {}",
            clos.code_block.argc_on_stack,
            args.len()
        ))));
    }

    for arg in args.iter().take(clos.code_block.argc_on_stack).rev() {
        frame.push(*arg);
    }
    if args.len() > clos.code_block.argc_on_stack {
        let mut vec = ScmVector::new(args.len() - clos.code_block.argc_on_stack);
        for i in &args[(args.len() - clos.code_block.argc_on_stack)..] {
            vec.push(*i);
        }
        frame.rest = Value::make_pointer(vec);
    }
    interpret(&mut frame)
}
