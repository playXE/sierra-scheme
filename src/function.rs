use crate::*;
use std::fmt::Write;
use std::mem::replace;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Op {
    JumpTrue(i32),
    JumpFalse(i32),
    Jump(i32),
    PushAcc,
    PopAcc,
    GetLiteral(u32),
    GetSym(u32),

    GetEnv(i16),
    GetVar0(u16),
    SetVar0(u16),
    GetVar1(u16),
    SetVar1(u16),
    GetVar(u16),
    SetVar(u16),
    GetGlobal(u32),
    SetGlobal(u32),
    MakeClosure(u32),
    Fixnum(isize),
    Real(f64),
    TailApply(u32),
    TailApplyRest(u32),
    Apply(u32),
    /// N arguments on stack plus one argument is vector or cons list of params.
    ApplyRest(u32),
    /// Get access to variadic arguments.
    Rest,

    Return,

    True,
    False,
    Nil,
}

pub struct CodeBlock {
    pub argc_on_stack: usize,
    pub var_count: usize,
    pub code: Gc<ScmBVector>,
    pub codes: Gc<ScmVector>,
    pub symbols: Gc<ScmVector>,
    pub literals: Gc<ScmVector>,
    pub unlinked: Vec<Op>,
}
impl CodeBlock {
    pub fn new() -> Gc<Self> {
        Gc::new(Self {
            argc_on_stack: 0,
            var_count: 0,
            unlinked: vec![],
            code: ScmBVector::new(16),
            codes: ScmVector::new(0),
            symbols: ScmVector::new(4),
            literals: ScmVector::new(8),
        })
    }

    pub fn disassemble(&mut self) -> Result<String, std::fmt::Error> {
        let mut buf = String::new();
        for (ix, op) in self.unlinked.iter().enumerate() {
            write!(buf, "{:04}: ", ix)?;
            match op {
                Op::Apply(x) | Op::TailApply(x) => write!(buf, "acc = apply {}", x)?,
                Op::ApplyRest(x) | Op::TailApplyRest(x) => write!(buf, "acc = apply-rest {}", x)?,
                Op::False => write!(buf, "acc = #f")?,
                Op::True => write!(buf, "acc = #t")?,
                Op::Nil => write!(buf, "acc = #nil")?,
                Op::Fixnum(x) => write!(buf, "acc = {}", x)?,
                Op::Real(x) => write!(buf, "acc = {}", x)?,
                Op::GetLiteral(x) => {
                    write!(buf, "acc = {:?} at {}", self.literals[(*x) as usize], x)?
                }
                Op::GetSym(x) => write!(
                    buf,
                    "acc = <symbol:{:?}> at {}",
                    self.symbols[(*x) as usize],
                    x
                )?,
                Op::GetVar(x) => write!(buf, "acc = get-var {}", x)?,
                Op::GetVar0(x) => write!(buf, "acc = get-var-0 {}", x)?,
                Op::GetVar1(x) => write!(buf, "acc = get-var-1 {}", x)?,
                Op::SetVar(x) => write!(buf, "acc = get-var {}", x)?,
                Op::SetVar0(x) => write!(buf, "set-var-0 {} <- acc", x)?,
                Op::SetVar1(x) => write!(buf, "set-var-1 {} <- acc", x)?,
                Op::GetEnv(x) => write!(buf, "acc = get-env {}", x)?,
                Op::Jump(x) => write!(buf, "jump {}", x)?,
                Op::JumpFalse(x) => write!(buf, "jump-false {}", x)?,
                Op::JumpTrue(x) => write!(buf, "jump-true {}", x)?,
                Op::PushAcc => write!(buf, "*stack++ = acc")?,
                Op::PopAcc => write!(buf, "acc = *stack--")?,
                Op::GetGlobal(x) => {
                    write!(buf, "acc = get-global '{:?}'", self.symbols[(*x) as usize])?
                }
                Op::SetGlobal(x) => {
                    write!(buf, "set-global '{:?}' <- acc", self.symbols[(*x) as usize])?
                }
                Op::MakeClosure(x) => write!(buf, "acc = make_closure {}", x)?,
                Op::Rest => write!(buf, "rest")?,
                Op::Return => write!(buf, "return <- acc")?,
            }
            write!(buf, "\n")?;
        }
        Ok(buf)
    }
}

#[repr(C)]
pub struct ScmFunction {
    pub hdr: Header,
    pub code_block: Gc<CodeBlock>,
    pub env: Gc<ScmVarEnv>,
}
impl ScmFunction {
    pub fn new(code: Gc<CodeBlock>) -> Gc<Self> {
        Gc::new(Self {
            env: Gc::new(ScmVarEnv {
                upper: None,
                vars: ScmVector::new(0),
            }),
            hdr: Header::new(ScmType::Closure),
            code_block: code,
        })
    }
}
impl Value {
    pub fn nativep(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::NativeClosure
    }
    pub fn native(self) -> Gc<ScmNative> {
        assert!(self.nativep());
        self.ptr()
    }
    pub fn closurep(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::Closure
    }

    pub fn closure(self) -> Gc<ScmFunction> {
        assert!(self.closurep());
        self.ptr()
    }
}
pub struct ScmVarEnv {
    pub upper: Option<Gc<Self>>,
    pub vars: Gc<ScmVector>,
}

impl GcObject for ScmVarEnv {}
impl GcObject for ScmFunction {}
impl GcObject for CodeBlock {
    const NEEDS_FINALIZATION: bool = true;
}

pub struct Compiler {
    pub code: Gc<CodeBlock>,
    pub val_map: Gc<ScmHashtable>,
    pub name_map: Gc<ScmHashtable>,
    pub raw_code: Vec<Op>,
    pub scopes: Value,
}

pub fn peephole(code: &mut Vec<Op>) {
    let mut prev = None;
    code.retain(|x| {
        if *x == Op::PopAcc && prev == Some(Op::PushAcc) {
            false
        } else {
            prev = Some(*x);
            true
        }
    });
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: CodeBlock::new(),
            val_map: ScmHashtable::new(4, None, None),
            name_map: ScmHashtable::new(4, None, None),
            raw_code: vec![],
            scopes: Value::make_cons(
                Value::make_pointer(ScmHashtable::new(4, None, None)),
                Value::make_nil(),
            ),
        }
    }
    pub fn save_state(
        &mut self,
    ) -> (
        Vec<Op>,
        Gc<CodeBlock>,
        Gc<ScmHashtable>,
        Gc<ScmHashtable>,
        Value,
    ) {
        (
            replace(&mut self.raw_code, vec![]),
            replace(&mut self.code, CodeBlock::new()),
            replace(&mut self.val_map, ScmHashtable::new(4, None, None)),
            replace(&mut self.name_map, ScmHashtable::new(4, None, None)),
            self.scopes,
        )
    }
    pub fn push_scope(&mut self) {
        let table = ScmHashtable::new(4, None, None);
        let cons = Value::make_cons(Value::make_pointer(table), self.scopes);
        self.scopes = cons;
    }

    pub fn compile_sexprs(&mut self, sexprs: Gc<ScmVector>) -> Result<Gc<CodeBlock>, Value> {
        for i in 0..sexprs.size() {
            let sexp = sexprs[i];

            self.compile_sexp(sexp)?;
        }
        self.emit(Op::Return);
        self.code.unlinked = replace(&mut self.raw_code, vec![]);
        if vm().disasm {
            println!(
                "Global code at {:p}\n{}",
                self.code.raw(),
                self.code.disassemble().unwrap()
            );
        }
        Ok(self.code)
    }
    pub fn compile_lambda(
        &mut self,
        mut formals: Value,
        body: Value,
    ) -> Result<Gc<CodeBlock>, Value> {
        let (pbc, pcode, pval_map, pname_map, scopes) = self.save_state();
        self.push_scope();
        while formals.consp() {
            let sym = formals.car();
            if !sym.symbolp() {
                return Err(Value::make_pointer(ScmCompileError::new(format!(
                    "Expected symbol but found '{:?}'",
                    sym
                ))));
            }
            let ix = self.define_variable(sym);
            self.emit(Op::PopAcc);
            self.emit(Op::SetVar0(ix));
            // VM needs to know how much arguments push to stack.
            self.code.argc_on_stack += 1;

            formals = formals.cdr();
        }
        // variadic parameters
        if formals != Value::make_nil() {
            if !formals.symbolp() {
                return Err(Value::make_pointer(ScmCompileError::new(format!(
                    "Expected symbol but found '{:?}'",
                    formals
                ))));
            }
            let ix = self.define_variable(formals);
            self.emit(Op::Rest);
            self.emit(Op::SetVar0(ix));
        }

        self.compile_sexp(body)?;

        self.emit(Op::Return);
        let mut code = self.code;
        code.unlinked = std::mem::replace(&mut self.raw_code, pbc);
        if vm().disasm {
            println!(
                "lambda code at {:p}: \n{}",
                code.raw(),
                code.disassemble().unwrap()
            );
        }
        self.code = pcode;

        self.val_map = pval_map;
        self.name_map = pname_map;
        self.scopes = scopes;
        Ok(code)
    }
    pub fn compile_sexp(&mut self, val: Value) -> Result<(), Value> {
        if val.consp() {
            let first = val.car();
            if first == mk_symbol("lambda") {
                let data = val.cdr();
                if !data.consp() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "expected (lambda <formals> <body>) but found '{:?}'",
                        first
                    ))));
                }
                let data = data.cons();
                if !data.cdr.consp() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "expected (lambda <formals> <body>) but found '{:?}'",
                        first
                    ))));
                }

                if !data.cdr.consp() || data.cdr.cdr() != Value::make_nil() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "expected (lambda <formals> <body>) but found '{:?}'",
                        first
                    ))));
                }
                let lambda = self.compile_lambda(data.car, data.cdr.car())?;
                self.code.codes.push(Value::make_pointer(lambda));
                let ix = self.code.codes.size - 1;
                self.emit(Op::MakeClosure(ix as _));
            } else if first == mk_symbol("define") {
                let data = val.cdr();
                if !data.consp() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "Expected <variable> but found '{:?}'",
                        data
                    ))));
                }

                let data = data.cons();
                if !data.car.symbolp() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "Expected <variable> but found '{:?}'",
                        data.car
                    ))));
                }
                if !data.cdr.consp() || data.cdr.cons().cdr != Value::make_nil() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "Expected <variable> but found '{:?}'",
                        val
                    ))));
                }
                let sym = data.car;
                let ix = self.define_variable(sym);
                self.compile_sexp(data.cdr.car())?;

                self.emit(Op::SetVar0(ix));
            } else if first == mk_symbol("set!") {
                let data = val.cdr();
                if !data.consp() || !data.cdr().consp() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "Expected (set! <variable> <value>) but found '{:?}'",
                        data
                    ))));
                }
                self.compile_sexp(data.cdr().car())?;

                self.var_store(data.car());
            } else if first == mk_symbol("if") {
                let args = val.cdr();
                if !args.consp() || !args.cdr().consp() {
                    return Err(Value::make_pointer(ScmCompileError::new(format!(
                        "Expected (if! <cond> <then> <else>) but found '{:?}'",
                        val
                    ))));
                }

                self.compile_sexp(args.car())?;
                let if_false = self.cjmp(false);
                self.compile_sexp(args.cdr().car())?;
                let end = self.jmp();
                if_false(self);
                if args.cdr().cdr().consp() {
                    self.compile_sexp(args.cdr().cdr().car())?;
                } else {
                    self.compile_sexp(args.cdr().cdr())?;
                }
                end(self);
            } else {
                let mut args = val.cdr();
                let mut argc = 0;
                while args.consp() {
                    argc += 1;
                    let arg = args.car();
                    self.compile_sexp(arg)?;
                    self.emit(Op::PushAcc);
                    args = args.cdr();
                }
                if !args.nilp() {
                    self.compile_sexp(args)?;
                    self.emit(Op::PushAcc);
                    self.compile_sexp(first)?;
                    self.emit(Op::ApplyRest(argc));
                } else {
                    self.compile_sexp(first)?;
                    self.emit(Op::Apply(argc));
                }
            }
        } else if val.fixnump() {
            self.emit(Op::Fixnum(val.fixnum()));
        } else if val.realp() {
            self.emit(Op::Real(val.real()));
        } else if val.symbolp() {
            self.var_load(val);
        } else if val.boolp() {
            if val == Value::make_true() {
                self.emit(Op::True);
            } else {
                self.emit(Op::False)
            }
        } else if val.nilp() {
            self.emit(Op::Nil);
        }

        Ok(())
    }
    pub fn cjmp(&mut self, cond: bool) -> impl FnOnce(&mut Self) {
        let p = self.raw_code.len();
        self.emit(Op::Jump(0));
        move |this: &mut Self| {
            let to = this.raw_code.len() - p;
            let ins = if cond {
                Op::JumpTrue(to as _)
            } else {
                Op::JumpFalse(to as _)
            };
            this.raw_code[p] = ins;
        }
    }
    pub fn jmp(&mut self) -> impl FnOnce(&mut Self) {
        let p = self.raw_code.len();
        self.emit(Op::Jump(0));
        move |this: &mut Self| {
            let to = this.raw_code.len() - p;
            let ins = Op::Jump(to as _);
            this.raw_code[p] = ins;
        }
    }
    pub fn pop_scope(&mut self) -> Value {
        if self.scopes.cdr() == Value::make_nil() {
            unreachable!("No scopes left");
        }
        let prev = self.scopes;
        self.scopes = prev.cdr();
        prev
    }
    pub fn current_scope(&self) -> Gc<ScmHashtable> {
        self.scopes.car().hashtable()
    }
    pub fn define_variable(&mut self, name: Value) -> u16 {
        debug_assert!(name.symbolp());
        let mut vars = self.current_scope();
        let (found, ix) = vars.lookup(name);
        if found {
            return ix.fixnum() as _;
        }
        let ix = self.code.var_count;
        self.code.var_count += 1;
        vars.insert(name, Value::make_fixnum(ix as _));
        ix as _
    }

    pub fn get_variable(&self, name: Value) -> Option<u16> {
        let (found, ix) = self.current_scope().lookup(name);
        if found {
            Some(ix.fixnum() as _)
        } else {
            None
        }
    }
    fn lookup_scope(&self, var: Value) -> Option<(u16, i32, Gc<ScmHashtable>)> {
        if let Some(var) = self.get_variable(var) {
            return Some((var, 0, self.current_scope()));
        }

        let mut scope = self.scopes.cdr();
        let mut depth = 0;
        while scope.consp() {
            depth += 1;
            let table = scope.car().hashtable();
            scope = scope.cdr();
            let (found, ix) = table.lookup(var);
            if found {
                return Some((ix.fixnum() as u16, depth, table));
            }
        }
        assert_eq!(scope, Value::make_nil());
        None
    }
    pub fn emit(&mut self, op: Op) {
        self.raw_code.push(op);
    }
    pub fn get_sym(&mut self, name: Gc<ScmSymbol>) -> u32 {
        let (found, ix) = self.name_map.lookup(Value::make_pointer(name));
        if found {
            return ix.fixnum() as _;
        }
        let ix = self.code.symbols.size;
        self.code.symbols.push(Value::make_pointer(name));
        self.name_map
            .insert(Value::make_pointer(name), Value::make_fixnum(ix as _));
        ix as _
    }
    pub fn var_load(&mut self, name: Value) {
        if let Some((index, depth, _scope)) = self.lookup_scope(name) {
            if depth == 0 {
                self.emit(Op::GetVar0(index));
            } else if depth == 1 {
                self.emit(Op::GetVar1(index));
            } else {
                self.emit(Op::GetEnv(depth as _));
                self.emit(Op::GetVar(index));
            }
        } else {
            let sym = self.get_sym(name.symbol());
            self.emit(Op::GetGlobal(sym));
        }
    }
    /// Value to store should be on stack.
    pub fn var_store(&mut self, name: Value) {
        // this code sometimes emit PopAcc which is later removed by peephope opt.
        if let Some((index, depth, _scope)) = self.lookup_scope(name) {
            if depth == 0 {
                self.emit(Op::SetVar0(index));
            } else if depth == 1 {
                self.emit(Op::SetVar1(index));
            } else {
                self.emit(Op::PushAcc);
                self.emit(Op::GetEnv(depth as _));
                self.emit(Op::SetVar(index));
            }
        } else {
            let sym = self.get_sym(name.symbol());
            self.emit(Op::SetGlobal(sym));
        }
    }

    pub fn get_val(&mut self, val: Value) -> u32 {
        let (found, ix) = self.val_map.lookup(val);
        if found {
            return ix.fixnump() as u32;
        }

        let ix = self.code.literals.size;
        self.code.literals.push(val);
        self.val_map.insert(val, Value::make_fixnum(ix as _));
        ix as _
    }

    pub fn get_val2(&mut self, val: Value) -> u32 {
        let ix = self.code.literals.size;
        self.code.literals.push(val);
        ix as _
    }
}
