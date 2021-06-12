use std::ops::{Deref, DerefMut};

use super::*;
/// The Vector type a sequence of Values that can be accessed in constant time
/// (although insertions and deletions are linear time).
pub struct Vector {
    elements: Vec<Value>,
}
impl Vector {
    pub fn with_capacity(gc: &mut GarbageCollector, capacity: usize) -> GcPointer<Self> {
        gc.allocate(Self {
            elements: Vec::with_capacity(capacity),
        })
    }
    pub fn from_cons(
        gc: &mut GarbageCollector,
        val: Value,
    ) -> Result<GcPointer<Self>, ConsToNativeError> {
        Ok(gc.allocate(Self {
            elements: Cons::to_vec(val)?,
        }))
    }

    pub fn from_iter(
        gc: &mut GarbageCollector,
        iter: impl Iterator<Item = Value>,
    ) -> GcPointer<Self> {
        let mut this = Self::with_capacity(gc, 0);
        for item in iter {
            this.push(item);
        }
        this
    }

    pub fn to_cons(&self, gc: &mut GarbageCollector) -> Value {
        let mut result = Value::encode_null_value();
        for i in (0..self.len()).rev() {
            result = Value::encode_object_value(Cons::new(gc, self.elements[i], result));
        }
        result
    }
}

unsafe impl Trace for Vector {
    fn trace(&self, visitor: &mut Tracer) {
        self.elements.trace(visitor);
    }
}

impl GcCell for Vector {}

impl Deref for Vector {
    type Target = Vec<Value>;
    fn deref(&self) -> &Self::Target {
        &self.elements
    }
}

impl DerefMut for Vector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.elements
    }
}
