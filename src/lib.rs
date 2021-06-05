use parking_lot::{lock_api::RawMutex, RawMutex as Lock};
use std::{
    collections::hash_map::DefaultHasher,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    mem::{size_of, transmute, MaybeUninit},
    ops::{Deref, Index, IndexMut},
    ptr::null_mut,
};
use vm::scm_thread;

use dashmap::DashMap;
use gc::{GC_malloc, GC_malloc_atomic, GC_realloc, Gc, GcObject};
#[macro_use]
pub mod macros;
pub mod builtins;
pub mod codegen;
pub mod function;
pub mod gc;
pub mod macro_expander;
pub mod synpass;
pub mod vm;
/// Scheme value encoding.
///
/// This struct simply allows us to put fixnum value on stack instead of allocating it on the heap.
/// To identify fixnum value we set first bit to 1 and to get fixnum value we just clear it. We do not
/// set bit in GC object pointer since BDWGC cannot conservatively mark tagged pointers.
///
///
#[derive(Clone, Copy)]
#[repr(C)]
pub struct Value {
    rep: ValueRep,
}

impl Value {
    pub fn to_bool(self) -> bool {
        if self.nilp() {
            false
        } else if self == Value::make_false() {
            false
        } else if self.fixnump() && self.fixnum() == 0 {
            false
        } else if self.realp() && self.real() == 0.0 {
            false
        } else {
            true
        }
    }
    pub fn nilp(self) -> bool {
        self == Value::make_nil()
    }
    pub fn car(self) -> Self {
        self.cons().car
    }

    pub fn cdr(self) -> Self {
        self.cons().cdr
    }
    #[inline(always)]
    pub fn raw(self) -> usize {
        unsafe { self.rep.raw }
    }
    #[inline(always)]
    pub fn pointerp(self) -> bool {
        !self.fixnump()
    }
    #[inline(always)]
    pub fn fixnump(self) -> bool {
        (self.raw() & 1) != 0
    }
    pub fn make_real(value: f64) -> Value {
        Self::make_pointer(ScmReal::new(value))
    }
    #[inline(always)]
    pub const fn make_fixnum(value: isize) -> Value {
        let mut value = value as usize;
        value = (value << 1) | 1;
        Self {
            rep: ValueRep { raw: value },
        }
    }
    #[inline(always)]
    pub fn make_nil() -> Self {
        unsafe { _NIL }
    }
    #[inline(always)]
    pub fn make_true() -> Self {
        unsafe { _TRUE }
    }
    #[inline(always)]
    pub fn make_bool(x: bool) -> Self {
        if x {
            Self::make_true()
        } else {
            Self::make_false()
        }
    }
    #[inline(always)]
    pub fn make_false() -> Self {
        unsafe { _FALSE }
    }
    #[inline(always)]
    pub fn make_cons(car: Value, cdr: Value) -> Self {
        let cons = ScmCons::new(car, cdr);
        Self::make_pointer(cons)
    }
    #[inline(always)]
    pub fn stringp(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::Str
    }

    #[inline(always)]
    pub fn realp(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::Real
    }
    #[inline(always)]
    pub fn vectorp(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::Vector
    }
    #[inline(always)]
    pub fn vector(self) -> Gc<ScmVector> {
        assert!(self.vectorp());
        self.ptr()
    }
    #[inline(always)]
    pub fn boolp(self) -> bool {
        self == Self::make_true() || self == Self::make_false()
    }
    #[inline(always)]
    pub fn consp(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::Cons
    }
    #[inline(always)]
    pub fn cons(self) -> Gc<ScmCons> {
        assert!(self.consp());
        self.ptr()
    }
    #[inline(always)]
    pub fn make_pointer<T: GcObject + Send>(value: Gc<T>) -> Value {
        Self {
            rep: ValueRep {
                gc: unsafe { transmute(value) },
            },
        }
    }
    #[inline(always)]
    pub fn fixnum(self) -> isize {
        let value = self.raw() >> 1;

        value as u32 as isize
    }
    #[inline(always)]
    pub fn pointer(self) -> Gc<ScmObj> {
        assert!(self.pointerp());
        unsafe { self.rep.gc }
    }

    #[inline(always)]
    pub fn real(self) -> f64 {
        assert!(self.realp());
        self.ptr::<ScmReal>().value
    }
    #[inline(always)]
    pub fn string(self) -> Gc<ScmString> {
        assert!(self.stringp());
        self.ptr::<ScmString>()
    }

    fn ptr<T: GcObject + Send>(self) -> Gc<T> {
        unsafe { transmute(self.pointer()) }
    }

    pub fn numberp(self) -> bool {
        self.fixnump() || self.realp()
    }

    pub fn number(self) -> f64 {
        assert!(self.numberp());
        if self.fixnump() {
            self.fixnum() as _
        } else {
            self.real() as _
        }
    }

    pub fn symbolp(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::Sym
    }

    pub fn symbol(self) -> Gc<ScmSymbol> {
        assert!(self.pointerp());
        self.ptr()
    }

    pub fn hashnodep(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::HashNode
    }
    pub fn hashtablep(self) -> bool {
        self.pointerp() && self.pointer().hdr.ty == ScmType::HashTable
    }

    pub fn hashnode(self) -> Gc<ScmHashtableNode> {
        assert!(self.hashnodep());
        self.ptr()
    }

    pub fn hashtable(self) -> Gc<ScmHashtable> {
        assert!(self.hashtablep());
        self.ptr()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.raw() == other.raw()
    }
}
impl Eq for Value {}

pub fn add(x: Value, y: Value) -> Value {
    if !x.fixnump() || !y.fixnump() {
        return Value::make_fixnum(-1);
    }

    Value::make_fixnum(x.fixnum() + y.fixnum())
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union ValueRep {
    gc: Gc<ScmObj>,
    raw: usize,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u8)]
pub enum ScmType {
    Real,
    Str,
    Cons,
    Sym,
    Port,
    Vector,
    Nil,
    True,
    False,
    Macro,
    HashNode,
    HashTable,
    BVector,
    Closure,
    NativeClosure,
    /// Macro written in native Rust code.
    NativeMacro,
    CompileError,
    RuntimeError,
}
pub struct Header {
    pub ty: ScmType,
}
impl Header {
    pub fn new(ty: ScmType) -> Self {
        Self { ty }
    }
}

#[repr(C)]
pub struct ScmObj {
    pub hdr: Header,
}
impl GcObject for ScmHashtableNode {}
#[repr(C)]
pub struct ScmHashtable {
    pub hdr: Header,
    pub nodes: Gc<ScmVector>,
    pub count: usize,
    pub mutex: Lock,

    pub eqv: fn(Value, Value) -> bool,
    pub hash: fn(Value, &mut dyn Hasher),
}
impl GcObject for ScmHashtable {}
impl ScmHashtable {
    pub fn new(
        size: usize,
        compute: Option<fn(Value, &mut dyn Hasher)>,
        eqv: Option<fn(Value, Value) -> bool>,
    ) -> Gc<Self> {
        let size = if size < 5 { 5 } else { size };
        let compute = compute.unwrap_or(default_hash);
        let eqv = eqv.unwrap_or(default_eqv);
        Gc::new(Self {
            hdr: Header::new(ScmType::HashTable),
            eqv,
            count: 0,
            mutex: Lock::INIT,
            nodes: ScmVector::new_nil(size),
            hash: compute,
        })
    }

    pub fn lock(&self) {
        self.mutex.lock();
    }

    pub fn unlock(&self) {
        unsafe {
            self.mutex.unlock();
        }
    }

    pub fn resize(&mut self) {
        let size = self.nodes.size * 2;
        let mut newtbl = Self {
            nodes: ScmVector::new_nil(size),
            eqv: self.eqv,
            count: 0,
            hash: self.hash,
            mutex: Lock::INIT,
            hdr: Header::new(ScmType::HashTable),
        };
        let mut node;
        let mut next;
        for n in 0..self.nodes.size {
            node = self.nodes[n];
            while node != Value::make_nil() {
                let node_ = node.hashnode();

                next = node_.next;

                newtbl.insert(node_.key, node_.value);
                self.remove(node_.key);
                node = next;
            }
        }

        self.nodes = newtbl.nodes;
    }
    pub fn remove(&mut self, key: Value) -> bool {
        let mut hasher = DefaultHasher::default();
        (self.hash)(key, &mut hasher);
        let hash = hasher.finish();
        let position = (hash % self.nodes.size as u64) as usize;
        let mut node = self.nodes[position];
        let mut prevnode = Value::make_nil();
        while node.hashnodep() {
            let n = node.hashnode();
            if n.hash == hash && (self.eqv)(n.key, key) {
                if prevnode.hashnodep() {
                    prevnode.hashnode().next = n.next;
                } else {
                    self.nodes[position] = n.next;
                }
                self.count -= 1;
                return true;
            }
            prevnode = node;
            node = n.next;
        }
        false
    }
    pub fn insert(&mut self, key: Value, val: Value) -> (bool, Value) {
        let mut hasher = DefaultHasher::default();
        (self.hash)(key, &mut hasher);
        let hash = hasher.finish();
        let mut position = (hash % self.nodes.size as u64) as usize;
        let mut node = self.nodes[position];

        while node != Value::make_nil() {
            if node.hashnode().hash == hash && (self.eqv)(key, node.hashnode().key) {
                let prev = node.hashnode().value;
                node.hashnode().value = val;
                return (true, prev);
            }
            node = node.hashnode().next;
        }
        if self.count >= (self.nodes.size as f64 * 0.75) as usize {
            self.resize();
            position = (hash % self.nodes.size as u64) as usize;
        }
        let mut node = ScmHashtableNode::new(hash, key, val);
        node.next = self.nodes[position];
        self.nodes[position] = Value::make_pointer(node);
        self.count += 1;
        (false, Value::make_nil())
    }

    pub fn lookup(&self, key: Value) -> (bool, Value) {
        let mut hasher = DefaultHasher::default();
        (self.hash)(key, &mut hasher);
        let hash = hasher.finish();
        let position = (hash % self.nodes.size as u64) as usize;
        let mut node = self.nodes[position];
        while node != Value::make_nil() {
            if node.hashnode().hash == hash && (self.eqv)(node.hashnode().key, key) {
                return (true, node.hashnode().value);
            }
            node = node.hashnode().next;
        }
        (false, Value::make_nil())
    }
}

fn default_eqv(x: Value, y: Value) -> bool {
    if x.fixnump() && y.fixnump() {
        return x == y;
    } else if x.realp() && y.realp() {
        return x.real() == y.real();
    } else if x.stringp() && y.stringp() {
        return x.string().as_bytes() == y.string().as_bytes();
    } else {
        return x == y;
    }
}
fn default_hash(x: Value, state: &mut dyn Hasher) {
    if x.fixnump() {
        state.write_isize(x.fixnum());
        //x.fixnum().hash(state);
    } else if x.realp() {
        state.write_u64(x.real().to_bits()); // x.real().to_bits().hash(state);
    } else if x.stringp() {
        state.write(x.string().as_bytes());
        //x.string().hash(state);
    } else {
        state.write_usize(x.raw());
        // x.raw().hash(state);
    }
}
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        default_hash(*self, state)
    }
}
#[repr(C)]
pub struct ScmHashtableNode {
    hdr: Header,
    hash: u64,
    key: Value,
    value: Value,
    next: Value,
}

impl ScmHashtableNode {
    pub fn new(hash: u64, key: Value, value: Value) -> Gc<Self> {
        Gc::new(Self {
            hdr: Header::new(ScmType::HashNode),
            key,
            value,
            hash,
            next: Value::make_nil(),
        })
    }
}
#[repr(C)]
pub struct ScmReal {
    pub hdr: Header,
    pub value: f64,
}
impl GcObject for ScmReal {}
impl ScmReal {
    pub fn new(val: f64) -> Gc<Self> {
        Gc::new_atomic(Self {
            hdr: Header::new(ScmType::Real),
            value: val,
        })
    }
}

#[repr(C)]
pub struct ScmCons {
    pub hdr: Header,
    pub car: Value,
    pub cdr: Value,
}
impl ScmCons {
    pub fn new(car: Value, cdr: Value) -> Gc<Self> {
        Gc::new(Self {
            hdr: Header::new(ScmType::Cons),
            car,
            cdr,
        })
    }
}

#[repr(C)]
pub struct ScmString {
    pub hdr: Header,
    pub(crate) size: usize,
    pub(crate) chars: *mut u8,
}

impl GcObject for ScmString {
    const NEEDS_FINALIZATION: bool = false;
}
unsafe impl Send for ScmString {}
impl ScmString {
    pub fn new(value: impl AsRef<str>) -> Gc<Self> {
        unsafe {
            let str = value.as_ref();
            let memory = GC_malloc_atomic(str.len() + 1);
            std::ptr::copy_nonoverlapping(str.as_bytes().as_ptr(), memory, str.len());
            memory.add(str.len()).write(0);
            Gc::new(Self {
                hdr: Header::new(ScmType::Str),
                size: str.len() + 1,
                chars: memory,
            })
        }
    }

    pub fn as_str(&self) -> &str {
        &*self
    }
}
pub fn equal(x: Value, y: Value) -> bool {
    if x.fixnump() && y.fixnump() {
        return x == y;
    }

    if x.numberp() && y.numberp() {
        let (x, y) = (x.number(), y.number());
        return x == y;
    }

    if x.stringp() && y.stringp() {
        return **x.string() == **y.string();
    }

    if x.vectorp() && y.vectorp() {
        let x = x.vector();
        let y = y.vector();

        if x.size != y.size {
            return false;
        }
        for i in 0..x.size {
            if !equal(x[i], y[i]) {
                return false;
            }
        }
        return true;
    }

    if x.consp() && y.consp() {
        let x = x.cons();
        let y = y.cons();
        return equal(x.car, y.car) && equal(x.cdr, y.cdr);
    }
    // just compare by pointer.
    x.raw() == y.raw()
}
impl Deref for ScmString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.chars, self.size)) }
    }
}
#[repr(C)]
pub struct ScmVector {
    pub hdr: Header,
    pub(crate) size: usize,
    pub(crate) capacity: usize,
    pub(crate) elts: *mut Value,
}

impl Index<usize> for ScmVector {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
        unsafe { &*self.elts.add(index) }
    }
}

impl IndexMut<usize> for ScmVector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        unsafe { &mut *self.elts.add(index) }
    }
}
impl ScmVector {
    pub fn slice_from(&self, from: usize) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.elts.add(from), self.size - from) }
    }
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }
    pub fn new(count: usize) -> Gc<Self> {
        unsafe {
            let pointer = GC_malloc(count * size_of::<Value>()).cast::<Value>();
            Gc::new(Self {
                hdr: Header::new(ScmType::Vector),
                capacity: count,
                size: 0,
                elts: pointer,
            })
        }
    }
    pub fn new_nil(count: usize) -> Gc<Self> {
        unsafe {
            let pointer = GC_malloc(count * size_of::<Value>()).cast::<Value>();
            let mut this = Gc::new(Self {
                hdr: Header::new(ScmType::Vector),
                capacity: count,
                size: 0,
                elts: pointer,
            });
            for _ in 0..count {
                this.push(Value::make_nil());
            }
            this
        }
    }
    #[inline]
    pub fn push(&mut self, value: Value) {
        if self.size < self.capacity {
            unsafe {
                self.elts.add(self.size).write(value);
                self.size += 1;
            }
        } else {
            unsafe {
                self.push_slow(value);
            }
        }
    }
    #[cold]
    #[inline(never)]
    unsafe fn push_slow(&mut self, value: Value) {
        let pointer =
            GC_realloc(self.elts.cast(), (self.capacity + 1) * size_of::<Value>()).cast::<Value>();
        self.elts = pointer;
        self.capacity += 1;
        self.push(value);
    }

    pub fn pop_back(&mut self) -> Value {
        let sz = self.size;
        if sz == 0 {
            return Value::make_nil();
        }
        unsafe {
            let val = self.elts.add(sz - 1).read();
            self.size -= 1;
            val
        }
    }
}
impl GcObject for ScmVector {}
unsafe impl Send for ScmVector {}
impl GcObject for ScmCons {}
#[repr(C)]
pub struct ScmSymbol {
    hdr: Header,
    size: usize,
    chars: *const u8,
}
unsafe impl Send for ScmSymbol {}
impl GcObject for ScmSymbol {}
impl ScmSymbol {
    /// Generates new unique symbol. It is not interned!
    pub fn gensym(value: impl AsRef<str>) -> Gc<Self> {
        unsafe {
            let str = value.as_ref();
            let memory = GC_malloc_atomic(str.len() + 1);
            std::ptr::copy_nonoverlapping(str.as_bytes().as_ptr(), memory, str.len());
            memory.add(str.len()).write(0);
            Gc::new(Self {
                hdr: Header::new(ScmType::Sym),
                size: str.len() + 1,
                chars: memory,
            })
        }
    }
    pub fn str(&self) -> &'static str {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.chars, self.size - 1))
        }
    }
    pub fn new(value: impl AsRef<str>) -> Gc<Self> {
        let value = value.as_ref();
        unsafe {
            let symtab = &*_SYMTAB.as_ptr();
            symtab.intern(value)
        }
    }
}

// Check for equality with string. This is needed for lookups in hash tables.
impl PartialEq<str> for ScmSymbol {
    fn eq(&self, other: &str) -> bool {
        &**self == other
    }
}
// Check for pointer equality.
impl PartialEq for ScmSymbol {
    fn eq(&self, other: &Self) -> bool {
        self.chars == other.chars
    }
}

impl Hash for ScmSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.chars.hash(state);
    }
}
impl Eq for ScmSymbol {}
impl Deref for ScmSymbol {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.chars, self.size)) }
    }
}
static mut _NIL: Value = Value::make_fixnum(0);
static mut _TRUE: Value = Value::make_fixnum(0);
static mut _FALSE: Value = Value::make_fixnum(0);
static mut _SYMTAB: MaybeUninit<SymbolTable> = MaybeUninit::uninit();
pub fn scm_init() {
    unsafe {
        extern "C" {
            fn GC_init();
        }

        GC_init();
        _SYMTAB = MaybeUninit::new(SymbolTable::new());
        _NIL = Value::make_pointer(Gc::new(ScmObj {
            hdr: Header::new(ScmType::Nil),
        }));

        _TRUE = Value::make_pointer(Gc::new(ScmObj {
            hdr: Header::new(ScmType::True),
        }));
        _FALSE = Value::make_pointer(Gc::new(ScmObj {
            hdr: Header::new(ScmType::False),
        }));
        _VM = Box::into_raw(Box::new(VM::new()));
    }
}

// we can't allocate ScmObj but we can transmute Gc<ScmObj> to other Gc-ed objects
// so it does not need finalization.
impl GcObject for ScmObj {
    const NEEDS_FINALIZATION: bool = false;
}

use logos::*;
use std::{collections::HashSet, iter::Peekable};

fn parse_int(input: &str, radix: u32, span: Span, raw: &str) -> Result<isize, String> {
    let input = input.replace("_", "");
    if input.len() == 0 {
        return Ok(0);
    }
    match isize::from_str_radix(input.as_str(), radix) {
        Ok(num) => Ok(num),
        Err(err) => Err(format!(
            "Parse int failed: {}\nNear {:?}: {}",
            err, span, raw
        )),
    }
}

fn bin_int(lex: &mut Lexer<Token>) -> Result<isize, String> {
    let slice = lex.slice();
    parse_int(&slice[2..], 2, lex.span(), slice)
}

fn oct_int(lex: &mut Lexer<Token>) -> Result<isize, String> {
    let slice = lex.slice();
    parse_int(&slice[2..], 8, lex.span(), slice)
}

fn dec_int(lex: &mut Lexer<Token>) -> Result<isize, String> {
    let slice = lex.slice();
    parse_int(slice, 10, lex.span(), slice)
}

fn hex_int(lex: &mut Lexer<Token>) -> Result<isize, String> {
    let slice = lex.slice();
    parse_int(&slice[2..], 16, lex.span(), slice)
}
fn parse_string_literal(i: &mut Lexer<Token>) -> Option<String> {
    unescape::unescape(&i.slice()[1..i.slice().len() - 1])
    // Some(i.slice().into())
}
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    EOF,

    #[regex(r"[.,#!$%\^&\*<>+;:{}=\-_`~a-zA-Z_][.,#!$@%\^&\*<>+;:{}=\-_`~a-zA-Z0-9]*",priority=2, callback = |lex| lex.slice().to_string())]
    Symbol(String),
    #[regex(r"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?", priority=1,callback = |lex| lex.slice().parse())]
    Float(f64),
    #[regex(r"[0-9][0-9_]*",priority = 3, callback =  dec_int)]
    #[regex(r"0b[0-1_]*", priority = 3, callback =  bin_int)]
    #[regex(r"0o[0-7_]*", priority = 3, callback =   oct_int)]
    #[regex(r"0x[0-9a-fA-F_]*", priority = 3, callback =   hex_int)]
    Fixnum(isize),
    #[token("'", priority = 5)]
    Quote,
    #[token("`", priority = 5)]
    QuasiQuote,
    #[token(",", priority = 5)]
    Unquote,
    #[token(",@", priority = 5)]
    UnquoteSplice,
    #[token("#t", priority = 5)]
    True,
    #[token("#f", priority = 5)]
    False,
    #[token("(", priority = 4)]
    LParen,
    #[token(")", priority = 4)]
    RParen,
    /*  #[token(r#"\n"#)]
    #[token(r#"\f"#)]
    Newline,
    #[token(r#"[ ]+"#)]
    Space,
    #[token(r#"[\t]+"#)]
    Tab,*/
    #[regex(r#""([^\\"]|\\([rnt\\/"']))*""#, parse_string_literal)]
    String(String),
}

pub struct Lex<'a> {
    lexer: Peekable<Lexer<'a, Token>>,
}
impl<'a> Lex<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
        }
    }

    pub fn get(&mut self) -> Option<Token> {
        self.lexer.next()
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    pub fn expect(&mut self, tok: Token) -> Result<(), String> {
        let ntok = self.next();
        if ntok.as_ref() != Some(&tok) {
            return Err(format!("Expected '{:?}' found '{:?}'", tok, ntok));
        }
        Ok(())
    }
}
impl Iterator for Lex<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next()
    }
}

pub fn parse(mut lex: Lex<'_>) -> Result<Gc<ScmVector>, String> {
    let mut ast = ScmVector::new(4);

    while lex.peek().is_some() {
        ast.push(parse_atom(&mut lex)?);
    }
    Ok(ast)
}

pub fn mk_symbol(x: impl AsRef<str>) -> Value {
    Value::make_pointer(ScmSymbol::new(x))
}

fn parse_pair(lex: &mut Lex<'_>) -> Result<Value, String> {
    match lex.peek() {
        Some(Token::RParen) => {
            lex.next().unwrap();
            return Ok(Value::make_nil());
        }
        _ => (),
    }
    let car = parse_atom(lex)?;

    let cdr = match lex.peek() {
        Some(Token::Symbol(x)) if x == "." => {
            lex.next().unwrap();
            let value = parse_atom(lex)?;
            lex.expect(Token::RParen)?;
            value
        }
        Some(_) => parse_pair(lex)?,

        None => return Err("Unexpected EOF, expected . or atom".to_string()),
    };
    Ok(Value::make_cons(car, cdr))
}
fn parse_atom(lex: &mut Lex<'_>) -> Result<Value, String> {
    match lex.next() {
        None => Err("unexpected EOF while reading".to_owned()),
        Some(tok) => match tok {
            Token::LParen => parse_pair(lex),

            Token::Symbol(x) => Ok(Value::make_pointer(ScmSymbol::new(x))),
            Token::String(x) => Ok(Value::make_pointer(ScmString::new(x))),
            Token::Fixnum(x) => Ok(Value::make_fixnum(x)),
            Token::Float(x) => Ok(Value::make_real(x)),
            Token::Quote => Ok(mk_symbol("quote")),
            Token::QuasiQuote => Ok(mk_symbol("quasiquote")),
            Token::Unquote => Ok(mk_symbol("unquote")),
            Token::UnquoteSplice => Ok(mk_symbol("unquote-splicing")),
            Token::True => Ok(Value::make_true()),
            Token::False => Ok(Value::make_false()),
            _ => Err(format!("Unexpected token '{:?}'", tok)),
        },
    }
}
fn print(f: &mut Formatter, val: Value, seen: &mut HashSet<usize>) -> fmt::Result {
    if val.fixnump() {
        write!(f, "{}", val.fixnum())
    } else if val.boolp() {
        if val == Value::make_true() {
            write!(f, "#t")
        } else {
            write!(f, "#f")
        }
    } else if val == Value::make_nil() {
        write!(f, "#nil")
    } else if val.realp() {
        write!(f, "{}", val.real())
    } else if val.vectorp() {
        let vec = val.vector();
        seen.insert(vec.raw() as _);
        write!(f, "#(")?;
        for i in 0..vec.size {
            if vec[i].pointerp() && seen.contains(&(vec[i].pointer().raw() as usize)) {
                write!(f, "<cyclic-value>")?;
            } else {
                print(f, vec[i], seen)?;
            }
            if i != vec.size - 1 {
                write!(f, " ")?;
            }
        }
        write!(f, ")")
    } else if val.consp() {
        write!(f, "(")?;
        print_pair(f, val.cons(), seen)?;
        write!(f, ")")
    } else if val.stringp() {
        write!(f, "{}", &**val.string())
    } else if val.symbolp() {
        write!(f, "{}", val.symbol().str())
    } else if val.pointerp() && val.pointer().hdr.ty == ScmType::CompileError {
        write!(
            f,
            "<compile-error: '{}'>",
            val.ptr::<ScmCompileError>().msg.as_str()
        )
    } else if val.pointerp() && val.pointer().hdr.ty == ScmType::RuntimeError {
        write!(
            f,
            "<runtime-error: '{}'>",
            val.ptr::<ScmRuntimeError>().msg.as_str()
        )
    } else if val.closurep() {
        write!(f, "#closure<{:x}>", val.raw())
    } else {
        write!(f, "<todo> {:x}", val.raw())
    }
}

fn print_pair(f: &mut Formatter, cons: Gc<ScmCons>, seen: &mut HashSet<usize>) -> fmt::Result {
    if seen.contains(&(cons.raw() as usize)) {
        return write!(f, "<cyclic-value>");
    }
    seen.insert(cons.raw() as usize);

    print(f, cons.car, seen)?;
    if cons.cdr.consp() {
        if seen.contains(&(cons.cdr.pointer().raw() as usize)) {
            return write!(f, "<cyclic-value>");
        }
    }

    if cons.cdr == Value::make_nil() {
        return Ok(());
    } else if cons.cdr.consp() {
        write!(f, " ")?;
        print_pair(f, cons.cdr.cons(), seen)
    } else {
        write!(f, " . ")?;
        print(f, cons.cdr, seen)
    }
}
impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut seen = HashSet::new();
        print(f, *self, &mut seen)
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            last_error: Value::make_nil(),
            disasm: false,
            environment: ScmHashtable::new(64, None, None),
        }
    }
}
pub struct SymbolTable {
    pub(crate) symbols: DashMap<&'static str, Gc<ScmSymbol>>,
    pub(crate) ids: DashMap<usize, &'static str>,
}
impl Drop for SymbolTable {
    fn drop(&mut self) {
        for entry in self.ids.iter_mut() {
            let key = entry.value();
            unsafe {
                let _ = Box::from_raw((*key) as *const _ as *mut str);
            }
        }
        self.symbols.clear();
        self.ids.clear();
    }
}
impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: DashMap::with_capacity(0),
            ids: DashMap::with_capacity(0),
        }
    }

    pub fn description(&self, symbol: Gc<ScmSymbol>) -> &'static str {
        *self.ids.get(&(symbol.raw() as usize)).unwrap()
    }
    pub fn intern(&self, val: &str) -> Gc<ScmSymbol> {
        let string = val;

        if let Some(key) = self.symbols.get(string) {
            return *key.value();
        }

        let sym = ScmSymbol::gensym(string);
        let _key = self.symbols.insert(sym.str(), sym);
        self.ids.insert(sym.raw() as usize, sym.str());
        sym
    }
}

static mut _VM: *mut VM = null_mut();
pub struct SchemeError;

#[no_mangle]
pub extern "C" fn vm() -> &'static mut VM {
    unsafe { &mut *_VM }
}
#[no_mangle]
pub extern "C" fn vm_raise(error: Value) -> ! {
    vm().last_error = error;
    scm_thread().last_error = error;
    std::panic::resume_unwind(Box::new(SchemeError));
}

#[no_mangle]
pub extern "C" fn vm_global_lookup(name: Value) -> Value {
    vm().environment.lock();
    let (found, value) = vm().environment.lookup(name);
    vm().environment.unlock();
    if !found {
        vm_raise(Value::make_pointer(ScmString::new(format!(
            "Variable '{:?}' not found",
            name
        ))))
    }
    value
}

#[no_mangle]
pub extern "C" fn vm_global_set(name: Value, value: Value) -> Value {
    let vm = vm();
    vm.environment.lock();
    let (found, value) = vm.environment.insert(name, value);
    vm.environment.unlock();
    if !found {
        vm_raise(Value::make_pointer(ScmString::new(format!(
            "Variable '{:?}' not found",
            name
        ))))
    }
    value
}

#[no_mangle]
pub extern "C" fn vm_global_insert(name: Value, value: Value) -> Value {
    let vm = vm();
    vm.environment.lock();
    let (_found, value) = vm.environment.insert(name, value);
    vm.environment.unlock();

    value
}
#[repr(C)]
pub struct ScmBVector {
    pub hdr: Header,
    pub size: usize,
    pub capacity: usize,
    pub elts: *mut u8,
}
unsafe impl Send for ScmBVector {}
impl GcObject for ScmBVector {}
impl ScmBVector {
    pub fn new(count: usize) -> Gc<Self> {
        unsafe {
            let pointer = GC_malloc_atomic(count);
            Gc::new(Self {
                hdr: Header::new(ScmType::BVector),
                capacity: count,
                size: 0,
                elts: pointer,
            })
        }
    }
    pub fn new_zeroed(count: usize) -> Gc<Self> {
        unsafe {
            let pointer = GC_malloc_atomic(count);
            let mut this = Gc::new(Self {
                hdr: Header::new(ScmType::Vector),
                capacity: count,
                size: 0,
                elts: pointer,
            });
            std::ptr::write_bytes(this.elts, 0, count);
            this.size = count;
            this
        }
    }
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn resize(&mut self, new_size: usize) {
        if new_size > self.size {
            for _ in self.size..new_size {
                self.push(0);
            }
        } else {
            for _ in (new_size..self.size).rev() {
                self.pop_back();
            }
        }
    }
    #[inline]
    pub fn push(&mut self, value: u8) {
        if self.size < self.capacity {
            unsafe {
                self.elts.add(self.size).write(value);
                self.size += 1;
            }
        } else {
            unsafe {
                self.push_slow(value);
            }
        }
    }
    #[inline(never)]
    unsafe fn push_slow(&mut self, value: u8) {
        let pointer = GC_realloc(
            self.elts.cast(),
            (self.capacity as f64 * 1.25).round() as usize,
        )
        .cast::<u8>();
        self.elts = pointer;
        self.capacity += 1;
        self.push(value);
    }

    pub fn pop_back(&mut self) -> Option<u8> {
        let sz = self.size;
        if sz == 0 {
            return None;
        }
        unsafe {
            let val = self.elts.add(sz - 1).read();
            self.size -= 1;
            Some(val)
        }
    }
}

#[repr(C)]
pub struct ScmCompileError {
    pub hdr: Header,
    pub msg: Gc<ScmString>,
}
impl GcObject for ScmCompileError {}
impl ScmCompileError {
    pub fn new(msg: impl AsRef<str>) -> Gc<Self> {
        Gc::new(Self {
            hdr: Header::new(ScmType::CompileError),
            msg: ScmString::new(msg),
        })
    }
}

impl Index<usize> for ScmBVector {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        unsafe { &*self.elts.add(index) }
    }
}

impl IndexMut<usize> for ScmBVector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        unsafe { &mut *self.elts.add(index) }
    }
}
#[repr(C)]
pub struct ScmRuntimeError {
    pub hdr: Header,
    pub msg: Gc<ScmString>,
}
impl GcObject for ScmRuntimeError {}
impl ScmRuntimeError {
    pub fn new(msg: impl AsRef<str>) -> Gc<Self> {
        Gc::new(Self {
            hdr: Header::new(ScmType::RuntimeError),
            msg: ScmString::new(msg),
        })
    }
}

pub struct VM {
    pub environment: Gc<ScmHashtable>,
    pub last_error: Value,
    pub disasm: bool,
}

#[repr(C)]
pub struct ScmNative {
    pub hdr: Header,
    pub clos: fn(&[Value]) -> Value,
}
impl ScmNative {
    pub fn new(clos: fn(&[Value]) -> Value) -> Gc<Self> {
        Gc::new(Self {
            hdr: Header::new(ScmType::NativeClosure),
            clos,
        })
    }
}
impl GcObject for ScmNative {}
