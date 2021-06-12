use super::*;

#[derive(Clone, Copy)]
pub struct Cons {
    pub first: Value,
    pub rest: Value,
}

unsafe impl Trace for Cons {
    fn trace(&self, visitor: &mut Tracer) {
        self.first.trace(visitor);
        self.rest.trace(visitor);
    }
}

impl GcCell for Cons {}

impl Cons {
    pub fn new(gc: &mut GarbageCollector, first: Value, rest: Value) -> GcPointer<Self> {
        gc.allocate(Self { first, rest })
    }
    pub fn make_list(gc: &mut GarbageCollector, values: impl Iterator<Item = Value>) -> Value {
        let mut first = None;
        let mut last: Option<GcPointer<Cons>> = None;
        for value in values {
            let newcell = Cons::new(gc, value, Value::encode_null_value());
            if first.is_none() {
                first = Some(newcell);
            } else {
                last.unwrap().rest = Value::encode_object_value(newcell);
            }
            last = Some(newcell);
        }

        if first.is_none() {
            Value::encode_null_value()
        } else {
            Value::encode_object_value(first.unwrap())
        }
    }

    pub fn to_vec(mut element: Value) -> Result<Vec<Value>, ConsToNativeError> {
        let mut results = vec![];
        while !element.is_null() {
            if !element.is_cons() {
                return Err(ConsToNativeError);
            }
            let cons = element.as_cons_or_null().unwrap();
            results.push(cons.first);
            element = cons.rest;
        }
        Ok(results)
    }
}

impl GcPointer<Cons> {
    /// Returns the number of cons cells in the list, starting at value. O(n) operation.
    pub fn length(&self) -> usize {
        let mut result = 0;
        let mut cons = Some(*self);
        while let Some(c) = cons {
            result += 1;
            cons = c.rest.as_cons_or_null();
        }
        result
    }
    pub fn get_nth_cons(&self, mut n: usize) -> Option<GcPointer<Cons>> {
        let mut cons = *self;
        while n != 0 {
            n -= 1;
            match cons.rest.as_cons_or_null() {
                Some(c) => cons = c,
                None => return None,
            }
        }
        Some(cons)
    }

    pub fn get_nth(&self, n: usize) -> Option<Value> {
        self.get_nth_cons(n).map(|x| x.first)
    }
    pub fn get_nth_tail(&self, n: usize) -> Option<Value> {
        self.get_nth_cons(n).map(|x| x.rest)
    }

    pub fn second(&self) -> Option<Value> {
        self.get_nth_cons(1).map(|x| x.first)
    }

    pub fn third(&self) -> Option<Value> {
        self.get_nth_cons(2).map(|x| x.first)
    }

    pub fn fourth(&self) -> Option<Value> {
        self.get_nth_cons(3).map(|x| x.first)
    }

    pub fn after_first(&self) -> Value {
        self.rest
    }

    pub fn after_second(&self) -> Option<Value> {
        self.get_nth_cons(1).map(|x| x.rest)
    }

    pub fn after_third(&self) -> Option<Value> {
        self.get_nth_cons(2).map(|x| x.rest)
    }
}

/// Thrown when trying to convert non null-terminated cons list to native array.
pub struct ConsToNativeError;

impl std::fmt::Debug for ConsToNativeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Only null-terminated lists of cons cells can be converted to arrays"
        )
    }
}
