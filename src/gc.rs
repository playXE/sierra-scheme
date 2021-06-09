use std::any::TypeId;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::Deref;
use std::ops::DerefMut;
use std::ptr::NonNull;
use tagged_box::*;

use crate::value::Value;
pub struct Handle<T: GcCell> {
    pointer: Value,
    marker: PhantomData<T>,
}

impl<T: GcCell> Deref for Handle<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.pointer.get_pointer().cast::<T>() }
    }
}

impl<T: GcCell> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.pointer.get_pointer().cast::<T>() }
    }
}

pub struct Tracer {}

/// Indicates that a type can be traced by a garbage collector.
///
/// This doesn't necessarily mean that the type is safe to allocate in a garbage collector ([GcCell]).
///
/// ## Safety
/// See the documentation of the `trace` method for more info.
/// Essentially, this object must faithfully trace anything that
/// could contain garbage collected pointers or other `Trace` items.
pub unsafe trait Trace {
    /// Visit each field in this type
    ///
    ///
    /// Structures should trace each of their fields,
    /// and collections should trace each of their elements.
    ///
    /// ### Safety
    /// Some types (like `GcPointer`) need special actions taken when they're traced,
    /// but those are somewhat rare and are usually already provided by the garbage collector.
    ///
    /// ## Always Permitted
    /// - Reading your own memory (includes iteration)
    ///   - Interior mutation is undefined behavior, even if you use `RefCell`
    /// - Panicking
    ///   - This should be reserved for cases where you are seriously screwed up,
    ///       and can't fulfill your contract to trace your interior properly.
    ///   - This rule may change in future versions, depending on how we deal with multi-threading.
    /// ## Never Permitted Behavior
    /// - Forgetting a element of a collection, or field of a structure
    ///   - If you forget an element undefined behavior will result
    ///   - This is why we always prefer automatically derived implementations where possible.
    ///     - You will never trigger undefined behavior with an automatic implementation,
    ///       and it'll always be completely sufficient for safe code (aside from destructors).
    ///     - With an automatically derived implementation you will never miss a field
    /// - Invoking this function directly.
    fn trace(&mut self, visitor: &mut Tracer) {
        let _ = visitor;
    }
}

/// `GcCell` is a type that can be allocated in GC gc and passed to JavaScript environment.
///
///
/// All cells that is not part of `src/vm` treatened as dummy objects and property accesses
/// is no-op on them.
///
pub trait GcCell: mopa::Any + Trace {
    /// Used when object has dynamic size i.e arrays
    fn compute_size(&self) -> usize {
        std::mem::size_of_val(self)
    }

    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

#[repr(C)]
pub struct GcPointerBase {
    pub vtable: TaggedPointer,
    pub type_id: TypeId,
}

pub const POSSIBLY_BLACK: u8 = 0;
pub const POSSIBLY_GREY: u8 = 2;
pub const DEFINETELY_WHITE: u8 = 1;

impl GcPointerBase {
    pub fn allocation_size(&self) -> usize {
        self.get_dyn().compute_size() + size_of::<Self>()
    }
    pub fn new(vtable: usize, type_id: TypeId) -> Self {
        Self {
            vtable: unsafe { TaggedPointer::new_unchecked(vtable, DEFINETELY_WHITE) },
            type_id,
        }
    }

    pub fn state(&self) -> u8 {
        self.vtable.discriminant()
        //self.cell_state.load(Ordering::Acquire)
    }

    pub fn set_state(&mut self, from: u8, to: u8) -> bool {
        /*self.cell_state
        .compare_exchange_weak(from, to, Ordering::AcqRel, Ordering::Relaxed)
        == Ok(from)*/
        unsafe {
            if self.state() != from {
                return false;
            }
            self.vtable = TaggedPointer::new_unchecked(self.vtable.as_ptr::<()>() as usize, to);
            true
        }
    }
    pub fn force_set_state(&mut self, to: u8) {
        unsafe {
            self.vtable = TaggedPointer::new_unchecked(self.vtable.as_ptr::<()>() as usize, to);
        }
        //self.cell_state.store(to, Ordering::AcqRel);
    }
    pub fn data<T>(&self) -> *mut T {
        unsafe {
            (self as *const Self as *mut u8)
                .add(size_of::<Self>())
                .cast()
        }
    }
    pub fn raw(&self) -> usize {
        self.vtable.as_raw_usize()
    }

    pub fn get_dyn(&self) -> &mut dyn GcCell {
        unsafe {
            std::mem::transmute(mopa::TraitObject {
                vtable: self.vtable() as _,
                data: self.data::<u8>() as _,
            })
        }
    }

    pub fn vtable(&self) -> usize {
        self.vtable.as_ptr::<()>() as _
        // (self.vtable & !(1 << 0)) as usize
    }
}
pub fn vtable_of<T: GcCell>(x: *const T) -> usize {
    unsafe { core::mem::transmute::<_, mopa::TraitObject>(x as *const dyn GcCell).vtable as _ }
}

pub fn vtable_of_type<T: GcCell + Sized>() -> usize {
    vtable_of(core::ptr::null::<T>())
}

/// A garbage collected pointer to a value.
///
/// This is the equivalent of a garbage collected smart-pointer.
///
///
/// The smart pointer is simply a guarantee to the garbage collector
/// that this points to a garbage collected object with the correct header,
/// and not some arbitrary bits that you've decided to gc allocate.]
#[repr(transparent)]
pub struct GcPointer<T: ?Sized> {
    pub(crate) base: NonNull<GcPointerBase>,
    pub(crate) marker: PhantomData<T>,
}

impl<T: GcCell + ?Sized> GcPointer<T> {
    pub fn ptr_eq<U: GcCell + ?Sized>(this: &Self, other: &GcPointer<U>) -> bool {
        this.base == other.base
    }
    #[inline]
    pub fn as_dyn(self) -> GcPointer<dyn GcCell> {
        GcPointer {
            base: self.base,
            marker: PhantomData,
        }
    }
}

impl<T: GcCell + ?Sized> GcPointer<T> {
    #[inline]
    pub fn is<U: GcCell>(self) -> bool {
        unsafe { (*self.base.as_ptr()).type_id == TypeId::of::<U>() }
    }

    #[inline]
    pub fn get_dyn(&self) -> &dyn GcCell {
        unsafe { (*self.base.as_ptr()).get_dyn() }
    }

    #[inline]
    pub fn get_dyn_mut(&mut self) -> &mut dyn GcCell {
        unsafe { (*self.base.as_ptr()).get_dyn() }
    }

    #[inline]
    pub unsafe fn downcast_unchecked<U: GcCell>(self) -> GcPointer<U> {
        GcPointer {
            base: self.base,
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn downcast<U: GcCell>(self) -> Option<GcPointer<U>> {
        if !self.is::<U>() {
            None
        } else {
            Some(unsafe { self.downcast_unchecked() })
        }
    }
}

impl<T: GcCell + ?Sized> Copy for GcPointer<T> {}
impl<T: GcCell + ?Sized> Clone for GcPointer<T> {
    fn clone(&self) -> Self {
        *self
    }
}
macro_rules! impl_prim {
    ($($t: ty)*) => {
        $(
            unsafe impl Trace for $t {}
            impl GcCell for $t {


            }
        )*
    };
}

impl_prim!(String bool f32 f64 u8 i8 u16 i16 u32 i32 u64 i64 );

/// Shadow stack type. This is a simple singly-linked list used for rooting in starlight.
pub struct ShadowStack {
    #[doc(hidden)]
    pub head: Cell<*mut RawShadowStackEntry>,
}

#[repr(C)]
pub struct RawShadowStackEntry {
    /// Shadowstack itself
    stack: *mut ShadowStack,
    /// Previous rooted entry
    prev: *mut RawShadowStackEntry,
    /// Pointer to vtable that is a `Trace` of rooted variable
    vtable: usize,
    /// Value is located right after vtable pointer, to access it we can construct trait object.
    data_start: [u8; 0],
}
impl RawShadowStackEntry {
    pub unsafe fn get_dyn(&self) -> &mut dyn Trace {
        std::mem::transmute(mopa::TraitObject {
            vtable: self.vtable as _,
            data: self.data_start.as_ptr() as *mut (),
        })
    }
}
impl ShadowStack {
    pub fn new() -> Self {
        Self {
            head: Cell::new(null_mut()),
        }
    }
}

unsafe impl Trace for ShadowStack {
    fn trace(&mut self, visitor: &mut dyn Tracer) {
        unsafe {
            let mut head = *self.head.as_ptr();
            while !head.is_null() {
                let next = (*head).prev;
                (*head).get_dyn().trace(visitor);
                head = next;
            }
        }
    }
}

impl<T: Trace> Drop for RootedInternal<'_, T> {
    fn drop(&mut self) {
        (*self.stack).head.set(self.prev);
    }
}

#[repr(C)]
pub struct RootedInternal<'a, T: Trace> {
    pub stack: &'a ShadowStack,
    pub prev: *mut RawShadowStackEntry,
    pub vtable: usize,
    pub value: T,
}

impl<'a, T: Trace> RootedInternal<'a, T> {
    #[inline]
    pub unsafe fn construct(
        stack: &'a ShadowStack,
        prev: *mut RawShadowStackEntry,
        vtable: usize,
        value: T,
    ) -> Self {
        Self {
            stack,
            prev,
            vtable,
            value,
        }
    }
}

/// Rooted value on stack. This is non-copyable type that is used to hold GC thing on stack.
///
/// # Usage
///
/// This type must be used for any type that is GC pointer or contains pointers to GC objects.
/// To construct rooted value [root!](root) macro should be used with provided shadowstack.
///
pub struct Rooted<'a, 'b, T: Trace> {
    #[doc(hidden)]
    pinned: std::pin::Pin<&'a mut RootedInternal<'b, T>>,
}
pub fn identity_clos<T, R>(x: T, clos: impl FnOnce(T) -> R) -> R {
    clos(x)
}
/// Create [Rooted<T>](Rooted) instance and push it to provided shadowstack instance.
///
///
/// ***NOTE***: This macro does not heap allocate internally. It uses some unsafe tricks to
/// allocate value on stack and push stack reference to shadowstack. Returned `Rooted<T>` internally
/// is `Pin<&mut T>`.
///
#[macro_export]
macro_rules! letroot {
    ($name: ident: $t: ty  = $stack: expr,$value: expr) => {
        let stack: &ShadowStack = &$stack;
        let value = $value;
        let mut $name = unsafe {
            $crate::gc::shadowstack::RootedInternal::<$t>::construct(
                stack as *mut _,
                stack.head,
                std::mem::transmute::<_, mopa::TraitObject>(&value as &dyn $crate::gc::cell::Trace)
                    .vtable as usize,
                value,
            )
        };

        stack.head.set(unsafe { std::mem::transmute(&mut $name) });

        let mut $name =
            unsafe { $crate::gc::shadowstack::Rooted::construct(std::pin::Pin::new(&mut $name)) };
    };

    ($name : ident = $stack: expr,$value: expr) => {
        let stack: &$crate::gc::shadowstack::ShadowStack = &$stack;
        let value = $value;
        let mut $name = unsafe {
            $crate::gc::shadowstack::RootedInternal::<_>::construct(
                stack,
                stack.head.get(),
                std::mem::transmute::<_, mopa::TraitObject>(&value as &dyn $crate::gc::cell::Trace)
                    .vtable as usize,
                value,
            )
        };

        stack.head.set(unsafe { std::mem::transmute(&mut $name) });

        let mut $name =
            unsafe { $crate::gc::shadowstack::Rooted::construct(std::pin::Pin::new(&mut $name)) };
    };
}

impl<'a, 'b, T: Trace> Rooted<'a, 'b, T> {
    /// Create `Rooted<T>` instance from pinned reference. Note that this should be used only
    /// inside `root!` macro and users of Starlight API should not use this function.
    pub unsafe fn construct(pin: Pin<&'a mut RootedInternal<'b, T>>) -> Self {
        Self { pinned: pin }
    }
    pub unsafe fn get_internal(&self) -> &RootedInternal<T> {
        std::mem::transmute_copy::<_, &RootedInternal<T>>(&self.pinned)
    }
    pub unsafe fn get_internal_mut(&mut self) -> &mut RootedInternal<T> {
        std::mem::transmute_copy::<_, &mut RootedInternal<T>>(&mut self.pinned)
    }
}

impl<'a, T: Trace> Deref for Rooted<'a, '_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.pinned.value
    }
}

impl<'a, T: Trace> DerefMut for Rooted<'a, '_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            &mut std::mem::transmute_copy::<_, &mut RootedInternal<T>>(&mut self.pinned).value
        }
    }
}
