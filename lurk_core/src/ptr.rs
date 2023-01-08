use std::{
  fmt,
  marker::PhantomData,
};

use lurk_ff::{
  LurkField,
  Tag,
};

// If .0 is negative, RawPtr is opaque. This lets us retain the efficiency and
// structure of the current implementation. It cuts the local store's address
// space in half, which is likely not an issue. This representation does not
// affect external data, so if we want to change it in the future, we can do so
// without a change of defined behavior.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct RawPtr<F: LurkField>((usize, bool), PhantomData<F>);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ptr<F: LurkField>(Tag, RawPtr<F>);
