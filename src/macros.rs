/// Compile time assertion
#[macro_export]
macro_rules! const_assert {
    ($x:expr $(,)?) => {
        #[allow(unknown_lints, clippy::eq_op)]
        const _: [(); 0 - !{
            const ASSERT: bool = $x;
            ASSERT
        } as usize] = [];
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset__let_base_ptr {
    ($name:ident, $type:ty) => {
        // No UB here, and the pointer does not dangle, either.
        // But we have to make sure that `uninit` lives long enough,
        // so it has to be in the same scope as `$name`. That's why
        // `let_base_ptr` declares a variable (several, actually)
        // instead of returning one.
        let uninit = ::core::mem::MaybeUninit::<$type>::uninit();
        let $name: *const $type = uninit.as_ptr();
    };
}

/// Macro to compute the distance between two pointers.
#[cfg(feature = "unstable_const")]
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset_offset_from {
    ($field:expr, $base:expr) => {
        // Compute offset, with unstable `offset_from` for const-compatibility.
        // (Requires the pointers to not dangle, but we already need that for `raw_field!` anyway.)
        unsafe { ($field as *const u8).offset_from($base as *const u8) as usize }
    };
}
#[cfg(feature = "unstable_raw")]
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset__raw_const {
    ($path:expr) => {{
        $crate::ptr::raw_const!($path)
    }};
}
#[cfg(not(feature = "unstable_raw"))]
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset__raw_const {
    ($path:expr) => {{
        // This is UB because we create an intermediate reference to uninitialized memory.
        // Nothing we can do about that without `raw_const!` though.
        &$path as *const _
    }};
}

/// Deref-coercion protection macro.
#[cfg(allow_clippy)]
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset__field_check {
    ($type:path, $field:tt) => {
        // Make sure the field actually exists. This line ensures that a
        // compile-time error is generated if $field is accessed through a
        // Deref impl.
        #[allow(clippy::unneeded_field_pattern)]
        let $type { $field: _, .. };
    };
}
#[cfg(not(allow_clippy))]
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset__field_check {
    ($type:path, $field:tt) => {
        // Make sure the field actually exists. This line ensures that a
        // compile-time error is generated if $field is accessed through a
        // Deref impl.
        let $type { $field: _, .. };
    };
}

/// Deref-coercion protection macro.
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset__field_check_tuple {
    ($type:ty, $field:tt) => {
        // Make sure the type argument is a tuple
        let (_, ..): $type;
    };
}

/// Computes a const raw pointer to the given field of the given base pointer
/// to the given parent type.
///
/// The `base` pointer *must not* be dangling, but it *may* point to
/// uninitialized memory.
#[macro_export(local_inner_macros)]
macro_rules! raw_field {
    ($base:expr, $parent:path, $field:tt) => {{
        _memoffset__field_check!($parent, $field);

        // Get the field address.
        // Crucially, we know that this will not trigger a deref coercion because
        // of the field check we did above.
        #[allow(unused_unsafe)] // for when the macro is used in an unsafe block
        unsafe {
            _memoffset__raw_const!((*($base as *const $parent)).$field)
        }
    }};
}

/// Computes a const raw pointer to the given field of the given base pointer
/// to the given parent tuple typle.
///
/// The `base` pointer *must not* be dangling, but it *may* point to
/// uninitialized memory.

#[macro_export(local_inner_macros)]
macro_rules! raw_field_tuple {
    ($base:expr, $parent:ty, $field:tt) => {{
        _memoffset__field_check_tuple!($parent, $field);

        // Get the field address.
        // Crucially, we know that this will not trigger a deref coercion because
        // of the field check we did above.
        #[allow(unused_unsafe)] // for when the macro is used in an unsafe block
        unsafe {
            _memoffset__raw_const!((*($base as *const $parent)).$field)
        }
    }};
}

#[cfg(not(feature = "unstable_const"))]
#[macro_export]
#[doc(hidden)]
macro_rules! _memoffset_offset_from {
    ($field:expr, $base:expr) => {
        // Compute offset.
        ($field as usize) - ($base as usize)
    };
}

/// Calculates the offset of the specified field from the start of the named struct.
///
/// ## Examples
/// ```
/// #[macro_use]
/// extern crate wtf_rs;
///
/// #[repr(C, packed)]
/// struct Foo {
///     a: u32,
///     b: u64,
///     c: [u8; 5]
/// }
///
/// fn main() {
///     assert_eq!(object_offsetof!(Foo, a), 0);
///     assert_eq!(object_offsetof!(Foo, b), 4);
/// }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! object_offsetof {
    ($parent:path, $field:tt) => {{
        // Get a base pointer (non-dangling if rustc supports `MaybeUninit`).
        _memoffset__let_base_ptr!(base_ptr, $parent);
        // Get field pointer.
        let field_ptr = raw_field!(base_ptr, $parent, $field);
        // Compute offset.
        _memoffset_offset_from!(field_ptr, base_ptr)
    }};
}

/// Calculates the offset of the specified field from the start of the tuple.
///
/// ## Examples
/// ```
/// #[macro_use]
/// extern crate wtf_rs;
///
/// fn main() {
///     assert!(offset_of_tuple!((u8, u32), 1) >= 0, "Tuples do not have a defined layout");
/// }
/// ```

#[macro_export(local_inner_macros)]
macro_rules! offset_of_tuple {
    ($parent:ty, $field:tt) => {{
        // Get a base pointer (non-dangling if rustc supports `MaybeUninit`).
        _memoffset__let_base_ptr!(base_ptr, $parent);
        // Get field pointer.
        let field_ptr = raw_field_tuple!(base_ptr, $parent, $field);
        // Compute offset.
        _memoffset_offset_from!(field_ptr, base_ptr)
    }};
}

#[macro_export]
macro_rules! offsetof {
    ($obj: ident . $($field:ident).*) => {
        unsafe {
            let ptr = 0x4000 as *mut $obj;
            &((*ptr).$($field).*) as *const _ as usize - 0x4000
        }
    }
}
