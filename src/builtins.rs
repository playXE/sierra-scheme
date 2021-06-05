use crate::{mk_symbol, vm_global_insert, vm_raise, ScmNative, ScmRuntimeError, Value};

pub fn builtins_init() {
    vm_global_insert(
        mk_symbol("cons"),
        Value::make_pointer(ScmNative::new(|args| {
            if args.is_empty() {
                return Value::make_nil();
            }
            let first = args[0];
            let second = args.get(1).copied().unwrap_or_else(|| Value::make_nil());
            Value::make_cons(first, second)
        })),
    );
    vm_global_insert(
        mk_symbol("car"),
        Value::make_pointer(ScmNative::new(|args| {
            if args.is_empty() || !args[0].consp() {
                vm_raise(Value::make_pointer(ScmRuntimeError::new(format!(
                    "Type error: cons cell expected"
                ))));
            }
            args[0].car()
        })),
    );

    vm_global_insert(
        mk_symbol("cdr"),
        Value::make_pointer(ScmNative::new(|args| {
            if args.is_empty() || !args[0].consp() {
                vm_raise(Value::make_pointer(ScmRuntimeError::new(format!(
                    "Type error: cons cell expected"
                ))));
            }
            args[0].cdr()
        })),
    );

    vm_global_insert(
        mk_symbol("list"),
        Value::make_pointer(ScmNative::new(|args| {
            args.iter()
                .rev()
                .fold(Value::make_nil(), |a, x| Value::make_cons(*x, a))
        })),
    );
}
