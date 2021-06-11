use std::hash::{Hash, Hasher};

use crate::gc::{GarbageCollector, GcCell, GcPointer, Trace, Tracer};

use super::package::Package;

pub struct Symbol {
    pub name: Box<str>,
    pub full_name: Box<str>,
    pub exported: bool,
    pub pkg: Option<GcPointer<Package>>,
}
impl Symbol {
    pub fn new(
        gc: &mut GarbageCollector,
        name: &str,
        pkg: Option<GcPointer<Package>>,
    ) -> GcPointer<Self> {
        gc.allocate(Self {
            name: name.to_owned().into_boxed_str(),
            pkg,
            full_name: if let Some(pkg) = pkg {
                if let Some(ref name) = pkg.name {
                    format!("{}:{}", name, name).into_boxed_str()
                } else {
                    name.to_owned().into_boxed_str()
                }
            } else {
                name.to_owned().into_boxed_str()
            },
            exported: false,
        })
    }
}
impl GcCell for Symbol {}
unsafe impl Trace for Symbol {
    fn trace(&self, visitor: &mut Tracer) {
        let _ = visitor;
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const Self as usize);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self as *const Self == other as *const Self
    }
}

impl Eq for Symbol {}
