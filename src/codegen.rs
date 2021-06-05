use std::collections::HashMap;

use crate::Value as ScmVal;
use crate::{gc::Gc, ScmHashtable, ScmObj, ScmType};
use cranelift::{
    codegen::*,
    frontend::{FunctionBuilder, FunctionBuilderContext, Variable},
    prelude::{types, InstBuilder, IntCC, MemFlags, Value},
};
use cranelift_jit::*;
use cranelift_module::*;

pub struct JIT {
    builder_context: FunctionBuilderContext,
    ctx: Context,
    data_ctx: DataContext,
    module: JITModule,
}
impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

pub struct Translator<'a> {
    builder: FunctionBuilder<'a>,
    variables: Gc<ScmHashtable>,
    module: &'a mut JITModule,
}

impl<'a> Translator<'a> {
    pub fn is_fixnum(&mut self, value: Value) -> Value {
        let and = self.builder.ins().band_imm(value, 1);
        self.builder.ins().icmp_imm(IntCC::NotEqual, and, 1)
    }

    pub fn is(&mut self, value: Value, ty: ScmType) -> Value {
        let is_fixnum = self.is_fixnum(value);

        let if_not = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::B1);
        let c = self.builder.ins().bconst(types::B1, false);
        self.builder.ins().brnz(is_fixnum, merge_block, &[c]);
        self.builder.ins().jump(if_not, &[]);
        self.builder.switch_to_block(if_not);
        let load = self.builder.ins().load(
            types::I8,
            MemFlags::new(),
            value,
            offsetof!(ScmObj.hdr.ty) as i32,
        );
        let cmp = self
            .builder
            .ins()
            .icmp_imm(IntCC::Equal, load, ty as u8 as i64);
        self.builder.ins().fallthrough(merge_block, &[cmp]);

        self.builder.switch_to_block(merge_block);
        self.builder.block_params(merge_block)[0]
    }
}
