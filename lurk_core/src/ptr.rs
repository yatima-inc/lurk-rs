use std::{
  hash::Hash,
  io,
  marker::PhantomData,
};

use lurk_ff::{
  tag::ExprTag,
  LurkField,
  Tag,
};

use crate::{
  store::Store,
  writer::Write,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RawPtr {
  Null,
  Opaque(usize),
  Index(usize),
}

impl RawPtr {
  pub fn new(p: usize) -> Self { RawPtr::Index(p) }

  pub fn is_opaque(&self) -> bool { matches!(self, Self::Opaque(_)) }

  pub fn is_null(&self) -> bool { *self == Self::Null }

  pub fn idx(&self) -> Option<usize> {
    match self {
      Self::Index(x) => Some(*x),
      _ => None,
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ptr<F: LurkField> {
  pub tag: Tag,
  pub raw: RawPtr,
  pub _f: PhantomData<F>,
}

#[allow(clippy::derive_hash_xor_eq)]
impl<F: LurkField> Hash for Ptr<F> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.tag.hash(state);
    self.raw.hash(state);
  }
}

impl<F: LurkField> Ptr<F> {
  pub fn index(expr_tag: ExprTag, idx: usize) -> Self {
    Ptr {
      tag: F::expr_tag(expr_tag),
      raw: RawPtr::Index(idx),
      _f: Default::default(),
    }
  }

  pub fn opaque(expr_tag: ExprTag, idx: usize) -> Self {
    Ptr {
      tag: F::expr_tag(expr_tag),
      raw: RawPtr::Index(idx),
      _f: Default::default(),
    }
  }

  pub fn null(expr_tag: ExprTag) -> Self {
    Ptr {
      tag: F::expr_tag(expr_tag),
      raw: RawPtr::Null,
      _f: Default::default(),
    }
  }

  pub fn is_null(&self) -> bool { self.raw == RawPtr::Null }

  pub fn is_opaque(&self) -> bool { self.raw.is_opaque() }
}

impl<F: LurkField> From<char> for Ptr<F> {
  fn from(c: char) -> Self {
    Self {
      tag: F::expr_tag(ExprTag::Char),
      raw: RawPtr::Index(u32::from(c) as usize),
      _f: Default::default(),
    }
  }
}

// impl<F: LurkField> Write<F> for Ptr<F> {
//  fn fmt<W: io::Write>(&self, store: &Store<F>, w: &mut W) -> io::Result<()> {
//    if self.is_opaque() {
//      // This should never fail.
//      write!(w, "<Opaque ")?;
//      write!(w, "{:?}", self.tag())?;
//
//      if let Some(x) = store.get_expr_hash(self) {
//        write!(w, " ")?;
//        Expr::Num(crate::num::Num::Scalar(*x.value())).fmt(store, w)?;
//      }
//      write!(w, ">")
//    }
//    else if let Some(expr) = store.fetch(self) {
//      expr.fmt(store, w)
//    }
//    else {
//      Ok(())
//    }
//  }
//}
