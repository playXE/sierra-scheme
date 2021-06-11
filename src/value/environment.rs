use crate::gc::{GarbageCollector, GcCell, GcPointer, Trace, Tracer};

use super::{cons::Cons, symbol::Symbol, SchemeError, Value};

pub struct Environment {
    parent: Option<GcPointer<Self>>,
    symbols: Box<[Option<GcPointer<Symbol>>]>,
    values: Box<[Value]>,
}

impl GcCell for Environment {}

unsafe impl Trace for Environment {
    fn trace(&self, visitor: &mut Tracer) {
        self.parent.trace(visitor);
        for sym in self.symbols.iter() {
            sym.trace(visitor);
        }
        for val in self.values.iter() {
            val.trace(visitor);
        }
    }
}

impl Environment {
    pub fn new(
        gc: &mut GarbageCollector,
        count: usize,
        parent: Option<GcPointer<Self>>,
    ) -> GcPointer<Self> {
        gc.allocate(Self {
            values: vec![Value::encode_null_value(); count].into_boxed_slice(),
            symbols: vec![None; count].into_boxed_slice(),
            parent,
        })
    }

    pub fn make(
        gc: &mut GarbageCollector,
        mut args: GcPointer<Cons>,
        parent: Option<GcPointer<Self>>,
    ) -> Result<GcPointer<Self>, SchemeError> {
        let count = args.length();
        let mut env = Self::new(gc, count, parent);
        for i in 0..count {
            env.symbols[i] = Some(args.first.as_symbol()?);
            env.values[i] = Value::encode_null_value();
            args = args.rest.as_cons_or_null().unwrap();
        }
        Ok(env)
    }

    pub fn get_symbol(&self, ix: usize) -> GcPointer<Symbol> {
        self.symbols[ix].unwrap()
    }
    pub fn set_symbol(&mut self, ix: usize, sym: GcPointer<Symbol>) {
        self.symbols[ix] = Some(sym);
    }
    pub fn get_value(&self, ix: usize) -> Value {
        self.values[ix]
    }

    pub fn index_of_symbol(&self, sym: GcPointer<Symbol>) -> Option<usize> {
        for i in 0..self.symbols.len() {
            if let Some(x) = self.symbols[i] {
                if GcPointer::ptr_eq(&x, &sym) {
                    return Some(i);
                }
            }
        }
        None
    }

    pub fn set_value(&mut self, ix: usize, value: Value) {
        self.values[ix] = value;
    }

    pub fn get_variable(&mut self, sym: GcPointer<Symbol>, frame: GcPointer<Self>) -> (u32, u32) {
        let mut frame = Some(frame);
        let mut frame_ix = 0;
        while let Some(f) = frame {
            let ix = f.index_of_symbol(sym);
            if let Some(ix) = ix {
                return (frame_ix, ix as _);
            } else {
                frame = f.parent;
                frame_ix += 1;
            }
        }
        (u32::MAX, u32::MAX)
    }
    pub fn get_symbol_at(varref: VarPos, frame: GcPointer<Self>) -> Option<GcPointer<Symbol>> {
        Self::get_frame(varref.0 as _, frame).map(|x| x.get_symbol(varref.1 as _))
    }
    pub fn set_symbol_at(varref: VarPos, sym: GcPointer<Symbol>, frame: GcPointer<Self>) {
        Self::get_frame(varref.0 as _, frame)
            .into_iter()
            .for_each(|mut x| x.set_symbol(varref.1 as _, sym));
    }
    pub fn get_value_at(varref: VarPos, frame: GcPointer<Self>) -> Option<Value> {
        Self::get_frame(varref.0 as _, frame).map(|x| x.get_value(varref.1 as _))
    }
    pub fn set_value_at(varref: VarPos, val: Value, frame: GcPointer<Self>) {
        Self::get_frame(varref.0 as _, frame)
            .into_iter()
            .for_each(|mut x| x.set_value(varref.1 as _, val));
    }
    pub fn get_frame(ix: usize, mut frame: GcPointer<Self>) -> Option<GcPointer<Self>> {
        for _ in 0..ix {
            frame = frame.parent?;
        }
        Some(frame)
    }
}

pub type VarPos = (u32, u32);

impl std::fmt::Debug for Environment {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
