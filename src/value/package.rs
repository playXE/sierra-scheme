use std::collections::HashMap;

use crate::gc::{GarbageCollector, GcCell, GcPointer, Trace, Tracer};

use super::{macro_::Macro, symbol::Symbol, SchemeError, Value};

/// Package is a storage for symbols. When the parser reads out symbols from the stream,
/// it retrieves the appropriate symbol from the package, or if one hasn't been seen before,
/// it interns a new one.
pub struct Package {
    pub name: Option<Box<str>>,
    pub symbols: HashMap<String, GcPointer<Symbol>>,
    pub bindings: HashMap<GcPointer<Symbol>, Value>,
    pub macros: HashMap<GcPointer<Symbol>, GcPointer<Macro>>,
    pub imports: Vec<GcPointer<Package>>,
}
impl Package {
    pub fn new(gc: &mut GarbageCollector, name: Option<String>) -> GcPointer<Self> {
        gc.allocate(Self {
            name: name.map(|x| x.into_boxed_str()),
            symbols: HashMap::new(),
            bindings: HashMap::new(),
            macros: HashMap::new(),
            imports: vec![],
        })
    }

    pub fn find(&self, name: &str, deep: bool) -> Option<GcPointer<Symbol>> {
        if let Some(result) = self.symbols.get(name) {
            return Some(*result);
        }
        if deep {
            for pkg in self.imports.iter() {
                if let Some(result) = pkg.find(name, deep) {
                    return Some(result);
                }
            }
        }
        None
    }
}

impl GcPointer<Package> {
    /// Interns the given name. If a symbol with this name already exists, it is returned.
    /// Otherwise a new symbol is created, added to internal storage, and returned.
    pub fn intern(&mut self, gc: &mut GarbageCollector, name: &str) -> GcPointer<Symbol> {
        if let Some(sym) = self.symbols.get(name) {
            return *sym;
        }
        let sym = Symbol::new(gc, name, Some(*self));
        self.symbols.insert(name.to_owned(), sym);
        sym
    }
    /// Uninterns the given symbol. If a symbol existed with this name, it will be removed,
    /// and the function returns true; otherwise returns false.
    pub fn unintern(&mut self, name: &str) -> bool {
        self.symbols.remove(name).is_some()
    }

    /// Retrieves the value binding for the given symbol, also traversing the import list.
    pub fn get_value(&self, sym: GcPointer<Symbol>) -> Result<Value, SchemeError> {
        if sym.pkg.is_none() {
            return Err(SchemeError::new(format!("No package in symbol")));
        }
        let pkg = sym.pkg.unwrap();
        if !GcPointer::ptr_eq(self, &pkg) {
            return Err(SchemeError::new(format!(
                "Unexpected package in getBinding: {:?}",
                pkg.name
            )));
        }
        if let Some(val) = self.bindings.get(&sym) {
            return Ok(*val);
        }

        for pkg in self.imports.iter() {
            let local = pkg.find(&sym.name, false);
            if let Some(local) = local {
                if local.exported {
                    if let Some(val) = pkg.bindings.get(&local) {
                        return Ok(*val);
                    }
                }
            }
        }
        Ok(Value::encode_null_value())
    }

    pub fn set_value(&mut self, sym: GcPointer<Symbol>, val: Value) -> Result<(), SchemeError> {
        if sym.pkg.is_none() {
            return Err(SchemeError::new(format!("No package in symbol")));
        }
        let pkg = sym.pkg.unwrap();
        if !GcPointer::ptr_eq(self, &pkg) {
            return Err(SchemeError::new(format!(
                "Unexpected package in getBinding: {:?}",
                pkg.name
            )));
        }
        if val.is_null() {
            self.bindings.remove(&sym);
        } else {
            self.bindings.insert(sym, val);
        }
        Ok(())
    }
}

unsafe impl Trace for Package {
    fn trace(&self, visitor: &mut Tracer) {
        self.symbols.trace(visitor);
        self.bindings.trace(visitor);
        self.macros.trace(visitor);
        self.imports.trace(visitor);
    }
}

impl GcCell for Package {}
