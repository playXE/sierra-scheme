use core::f64;
use std::{
    alloc::Layout,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::{forget, ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut},
    ptr::NonNull,
    u128,
};

extern "C" {
    pub fn GC_malloc(size: usize) -> *mut u8;
    pub fn GC_malloc_atomic(size: usize) -> *mut u8;
    pub fn GC_malloc_uncollectable(size: usize) -> *mut u8;
    pub fn GC_realloc(old: *mut u8, new_size: usize) -> *mut u8;
    pub fn GC_free(obj: *mut u8);
    pub fn GC_init();
    pub fn GC_gcollect();
    pub fn GC_enable_incremental();
    pub fn GC_register_finalizer(
        obj: *mut u8,
        finalizer: Option<GCFinalizationProc>,
        client_data: *mut u8,
        old_finalizer: *mut GCFinalizationProc,
        old_client_data: *mut *mut u8,
    );

    pub fn GC_register_finalizer_no_order(
        obj: *mut u8,
        finalizer: Option<GCFinalizationProc>,
        client_data: *mut u8,
        old_finalizer: *mut GCFinalizationProc,
        old_client_data: *mut *mut u8,
    );

}

pub type GCFinalizationProc = unsafe extern "C" fn(obj: *mut u8, client_data: *mut u8);

/// A garbage collected pointer.
///
/// The type `Gc<T>` provides shared ownership of a value of type `T`,
/// allocted in the heap. `Gc` pointers are `Copyable`, so new pointers to
/// the same value in the heap can be produced trivially. The lifetime of
/// `T` is tracked automatically: it is freed when the application
/// determines that no references to `T` are in scope. This does not happen
/// deterministically, and no guarantees are given about when a value
/// managed by `Gc` is freed.
///
/// Shared references in Rust disallow mutation by default, and `Gc` is no
/// exception: you cannot generally obtain a mutable reference to something
/// inside an `Gc`. If you need mutability, put a `Cell` or `RefCell` inside
/// the `Gc`.
///
/// Unlike `Rc<T>`, cycles between `Gc` pointers are allowed and can be
/// deallocated without issue.
///
/// `Gc<T>` automatically dereferences to `T` (via the `Deref` trait), so
/// you can call `T`'s methods on a value of type `Gc<T>`.
///
/// `Gc<T>` will implement `Sync` as long as `T` implements `Sync`. `Gc<T>`
/// will always implement `Send` because it requires `T` to implement `Send`.
/// This is because if `T` has a finalizer, it will be run on a seperate thread.
#[repr(C)]
pub struct Gc<T: ?Sized + Send + GcObject> {
    ptr: GcPointer<T>,
    _phantom: PhantomData<T>,
}
impl<T: GcObject + Send + PartialEq> PartialEq<T> for Gc<T> {
    fn eq(&self, other: &T) -> bool {
        **self == *other
    }
}
impl<T: GcObject + Send + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}
impl<T: GcObject + Send + Eq> Eq for Gc<T> {}
impl GcObject for str {}
/// This zero-sized wrapper struct is needed to allow `Gc<T>` to have the same
/// `Send` + `Sync` semantics as `T`. Without it, the inner `NonNull` type would
/// mean that a `Gc` never implements `Send` or `Sync`.
#[derive(PartialEq, Eq)]
#[repr(C)]
struct GcPointer<T: ?Sized + GcObject>(NonNull<GcBox<T>>);

unsafe impl<T: GcObject> Send for GcPointer<T> {}
unsafe impl<T: GcObject> Sync for GcPointer<T> {}

impl<T: Send + GcObject> Gc<T> {
    pub fn raw(self) -> *mut T {
        self.ptr.0.as_ptr().cast()
    }

    /// Constructs a new `Gc<T>`.
    pub fn new(v: T) -> Self {
        Gc {
            ptr: unsafe { GcPointer(NonNull::new_unchecked(GcBox::new(v))) },
            _phantom: PhantomData,
        }
    }
    /// Constructs a new `Gc<T>`.
    pub fn new_atomic(v: T) -> Self {
        Gc {
            ptr: unsafe { GcPointer(NonNull::new_unchecked(GcBox::new_atomic(v))) },
            _phantom: PhantomData,
        }
    }

    /// Constructs a new `Gc<T>`.
    pub fn new_uncollectable(v: T) -> Self {
        Gc {
            ptr: unsafe { GcPointer(NonNull::new_unchecked(GcBox::new_uncollectable(v))) },
            _phantom: PhantomData,
        }
    }

    /// Constructs a new `Gc<MaybeUninit<T>>` which is capable of storing data
    /// up-to the size permissible by `layout`.
    ///
    /// This can be useful if you want to store a value with a custom layout,
    /// but have the collector treat the value as if it were T.
    ///
    /// # Panics
    ///
    /// If `layout` is smaller than that required by `T` and/or has an alignment
    /// which is smaller than that required by `T`.
    pub fn new_from_layout(layout: Layout) -> Gc<MaybeUninit<T>> {
        let tl = Layout::new::<T>();
        if layout.size() < tl.size() || layout.align() < tl.align() {
            panic!(
                "Requested layout {:?} is either smaller than size {} and/or not aligned to {}",
                layout,
                tl.size(),
                tl.align()
            );
        }
        unsafe { Gc::new_from_layout_unchecked(layout) }
    }

    /// Constructs a new `Gc<MaybeUninit<T>>` which is capable of storing data
    /// up-to the size permissible by `layout`.
    ///
    /// This can be useful if you want to store a value with a custom layout,
    /// but have the collector treat the value as if it were T.
    ///
    /// # Safety
    ///
    /// The caller is responsible for ensuring that both `layout`'s size and
    /// alignment must match or exceed that required to store `T`.
    pub unsafe fn new_from_layout_unchecked(layout: Layout) -> Gc<MaybeUninit<T>> {
        Gc::from_inner(GcBox::new_from_layout(layout))
    }

    pub fn unregister_finalizer(&mut self) {
        let ptr = self.ptr.0.as_ptr() as *mut GcBox<T>;
        unsafe {
            GcBox::unregister_finalizer(&mut *ptr);
        }
    }
}

pub trait GcObject {
    const NEEDS_FINALIZATION: bool = false;
}

impl<T: ?Sized + Send + GcObject> Gc<T> {
    /// Get a raw pointer to the underlying value `T`.
    pub fn into_raw(this: Self) -> *const T {
        this.ptr.0.as_ptr() as *const T
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.ptr.0.as_ptr() == other.ptr.0.as_ptr()
    }

    /// Get a `Gc<T>` from a raw pointer.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that `raw` was allocated with `Gc::new()` or
    /// u8 `Gc::new_from_layout()`.
    ///
    /// It is legal for `raw` to be an interior pointer if `T` is valid for the
    /// size and alignment of the originally allocated block.
    pub fn from_raw(raw: *const T) -> Gc<T> {
        Gc {
            ptr: unsafe { GcPointer(NonNull::new_unchecked(raw as *mut GcBox<T>)) },
            _phantom: PhantomData,
        }
    }

    fn from_inner(ptr: NonNull<GcBox<T>>) -> Self {
        Self {
            ptr: GcPointer(ptr),
            _phantom: PhantomData,
        }
    }
}

impl<T: Send + GcObject> Gc<MaybeUninit<T>> {
    /// As with `MaybeUninit::assume_init`, it is up to the caller to guarantee
    /// that the inner value really is in an initialized state. Calling this
    /// when the content is not yet fully initialized causes immediate undefined
    /// behaviour.
    pub unsafe fn assume_init(self) -> Gc<T> {
        let ptr = self.ptr.0.as_ptr() as *mut GcBox<MaybeUninit<T>>;
        Gc::from_inner((&mut *ptr).assume_init())
    }
}

impl<T: ?Sized + fmt::Display + Send + GcObject> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Debug + Send + GcObject> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + Send + GcObject> fmt::Pointer for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&(&**self as *const T), f)
    }
}

impl<T: GcObject> GcObject for MaybeUninit<T> {
    const NEEDS_FINALIZATION: bool = T::NEEDS_FINALIZATION;
}

/// A `GcBox` is a 0-cost wrapper which allows a single `Drop` implementation
/// while also permitting multiple, copyable `Gc` references. The `drop` method
/// on `GcBox` acts as a guard, preventing the destructors on its contents from
/// running unless the object is really dead.
#[repr(C)]
struct GcBox<T: ?Sized + GcObject>(ManuallyDrop<T>);

impl<T: GcObject> GcBox<T> {
    fn new(value: T) -> *mut GcBox<T> {
        let layout = Layout::new::<T>();
        let ptr = unsafe { GC_malloc(layout.size()) as *mut GcBox<T> }; //ALLOCATOR.allocate(layout).unwrap().as_ptr() as *mut GcBox<T>;
        let gcbox = GcBox(ManuallyDrop::new(value));

        unsafe {
            ptr.copy_from_nonoverlapping(&gcbox, 1);
            GcBox::register_finalizer(&mut *ptr);
        }

        forget(gcbox);
        ptr
    }
    fn new_atomic(value: T) -> *mut GcBox<T> {
        let layout = Layout::new::<T>();
        let ptr = unsafe { GC_malloc_atomic(layout.size()) as *mut GcBox<T> }; //ALLOCATOR.allocate(layout).unwrap().as_ptr() as *mut GcBox<T>;
        let gcbox = GcBox(ManuallyDrop::new(value));

        unsafe {
            ptr.copy_from_nonoverlapping(&gcbox, 1);
            GcBox::register_finalizer(&mut *ptr);
        }

        forget(gcbox);
        ptr
    }
    fn new_uncollectable(value: T) -> *mut GcBox<T> {
        let layout = Layout::new::<T>();
        let ptr = unsafe { GC_malloc_uncollectable(layout.size()) as *mut GcBox<T> }; //ALLOCATOR.allocate(layout).unwrap().as_ptr() as *mut GcBox<T>;
        let gcbox = GcBox(ManuallyDrop::new(value));

        unsafe {
            ptr.copy_from_nonoverlapping(&gcbox, 1);
            //GcBox::register_finalizer(&mut *ptr);
        }

        forget(gcbox);
        ptr
    }
    fn new_from_layout(layout: Layout) -> NonNull<GcBox<MaybeUninit<T>>> {
        unsafe {
            let base_ptr = GC_malloc(layout.size()) as *mut usize; //ALLOCATOR.allocate(layout).unwrap().as_ptr() as *mut usize;
            NonNull::new_unchecked(base_ptr as *mut GcBox<MaybeUninit<T>>)
        }
    }
    #[allow(dead_code)]
    fn new_from_layout_atomic(layout: Layout) -> NonNull<GcBox<u8>> {
        unsafe {
            let base_ptr = GC_malloc_atomic(layout.size()) as *mut usize; //ALLOCATOR.allocate(layout).unwrap().as_ptr() as *mut usize;
            NonNull::new_unchecked(base_ptr as *mut GcBox<u8>)
        }
    }

    fn register_finalizer(&mut self) {
        #[cfg(feature = "gc_stats")]
        crate::stats::NUM_REGISTERED_FINALIZERS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        if !T::NEEDS_FINALIZATION {
            return;
        }

        unsafe extern "C" fn fshim<T>(obj: *mut u8, _meta: *mut u8) {
            ManuallyDrop::drop(&mut *(obj as *mut ManuallyDrop<T>));
        }

        unsafe {
            GC_register_finalizer(
                self as *mut _ as *mut u8,
                Some(fshim::<T>),
                ::std::ptr::null_mut(),
                ::std::ptr::null_mut(),
                ::std::ptr::null_mut(),
            )
        }
    }

    fn unregister_finalizer(&mut self) {
        unsafe {
            GC_register_finalizer(
                self as *mut _ as *mut u8,
                None,
                ::std::ptr::null_mut(),
                ::std::ptr::null_mut(),
                ::std::ptr::null_mut(),
            )
        }
        //ALLOCATOR.unregister_finalizer(self as *mut _ as *mut u8);
    }
}

impl<T: GcObject> GcBox<MaybeUninit<T>> {
    unsafe fn assume_init(&mut self) -> NonNull<GcBox<T>> {
        // Now that T is initialized, we must make sure that it's dropped when
        // `GcBox<T>` is freed.
        let init = self as *mut _ as *mut GcBox<T>;
        GcBox::register_finalizer(&mut *init);
        NonNull::new_unchecked(init)
    }
}

impl<T: ?Sized + Send + GcObject> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.ptr.0.as_ptr() as *mut T) }
    }
}

impl<T: ?Sized + Send + GcObject> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.ptr.0.as_ptr() as *const T) }
    }
}

/// `Copy` and `Clone` are implemented manually because a reference to `Gc<T>`
/// should be copyable regardless of `T`. It differs subtly from `#[derive(Copy,
/// Clone)]` in that the latter only makes `Gc<T>` copyable if `T` is.
impl<T: ?Sized + Send + GcObject> Copy for Gc<T> {}

impl<T: ?Sized + Send + GcObject> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized + GcObject> Copy for GcPointer<T> {}

impl<T: ?Sized + GcObject> Clone for GcPointer<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ?Sized + Hash + Send + GcObject> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: GcObject, const N: usize> GcObject for [T; N] {
    const NEEDS_FINALIZATION: bool = T::NEEDS_FINALIZATION;
}

macro_rules! impl_simple {
    ($($t: ty)*) => {
        $(
            impl GcObject for $t {}
        )*
    };
}

impl_simple!(
    u8 u16 u32 u64 u128
    i8 i16 i32 i64 i128
    f32 f64
    bool
);

macro_rules! impl_drop {
    ($($t: ty)*) => {
        $(
            impl GcObject for $t {
                const NEEDS_FINALIZATION: bool = true;
            }
        )*
    };
}
impl_drop!(String std::fs::File);
#[cfg(test)]
mod test {
    use super::*;
    use std::mem::size_of;

    #[test]
    #[should_panic]
    fn test_too_small() {
        Gc::<[u8; 256]>::new_from_layout(Layout::from_size_align(1, 1).unwrap());
    }

    #[test]
    #[should_panic]
    fn test_unaligned() {
        #[repr(align(1024))]
        struct S {
            _x: usize,
        }
        impl GcObject for S {}
        Gc::<S>::new_from_layout(Layout::from_size_align(size_of::<S>(), 1).unwrap());
    }
}

pub fn collect_garbage() {
    unsafe {
        GC_gcollect();
    }
}

use std::alloc::GlobalAlloc;

pub struct GC;

unsafe impl GlobalAlloc for GC {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        GC_malloc(layout.size())
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        GC_free(ptr);
    }
    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        GC_realloc(ptr, new_size)
    }
}
