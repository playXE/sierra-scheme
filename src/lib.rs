use gc::ShadowStack;
use std::mem::transmute;

pub mod gc;
pub mod value;

pub struct State {
    stack: ShadowStack,
}

impl State {
    pub fn shadowstack(&self) -> &'static ShadowStack {
        unsafe { transmute(&self.stack) }
    }
}
