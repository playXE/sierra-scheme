use super::*;
use closure::Closure;
pub struct Macro {
    pub clos: Closure,
}

impl GcCell for Macro {}
unsafe impl Trace for Macro {
    fn trace(&self, visitor: &mut Tracer) {
        self.clos.trace(visitor);
    }
}
