use std::{
  fmt,
  hash::Hash,
};

use lurk_ff::{
  field::{
    FWrap,
    LurkField,
  },
  tag::{
    ExprTag,
    Tag,
  },
};

use crate::{
  expr::Expr,
  op::{
    Op1,
    Op2,
  },
  serde_f::{
    SerdeF,
    SerdeFError,
  },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ptr<F: LurkField> {
  pub tag: Tag,
  pub val: F,
}

#[allow(clippy::derive_hash_xor_eq)]
impl<F: LurkField> Hash for Ptr<F> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.tag.hash(state);
    FWrap(self.val).hash(state);
  }
}

impl<F: LurkField> Ptr<F> {
  pub fn immediate(self) -> Option<Expr<F>> {
    match self.tag.expr {
      ExprTag::Num => Some(Expr::Num(self.val)),
      ExprTag::Char => Some(Expr::Char(self.val)),
      ExprTag::U64 => Some(Expr::U64(self.val)),
      ExprTag::Str if self.val == F::zero() => Some(Expr::StrNil),
      ExprTag::Cons if self.val == F::zero() => Some(Expr::ConsNil),
      ExprTag::Sym if self.val == F::zero() => Some(Expr::SymNil),
      ExprTag::Outermost => Some(Expr::Outermost),
      ExprTag::Error => Some(Expr::Error),
      ExprTag::Dummy => Some(Expr::Dummy),
      ExprTag::Terminal => Some(Expr::Terminal),
      ExprTag::Op1 => Some(Expr::Op1(Op1::try_from(self.val.to_u16()?).ok()?)),
      ExprTag::Op2 => Some(Expr::Op2(Op2::try_from(self.val.to_u16()?).ok()?)),
      _ => None,
    }
  }

  pub fn is_immediate(self) -> bool { self.immediate().is_some() }

  pub fn child_ptr_arity(self) -> usize {
    if self.immediate().is_some() {
      0
    }
    else {
      match self.tag.expr {
        ExprTag::Cons => 2,
        ExprTag::Sym => 2,
        ExprTag::Fun => 3,
        ExprTag::Thunk => 2,
        ExprTag::Str => 2,
        ExprTag::Comm => 2,
        ExprTag::Key => 1,
        ExprTag::Map => 1,
        ExprTag::Link => 2,
        ExprTag::Call => 3,
        ExprTag::Call0 => 2,
        ExprTag::Call2 => 3,
        ExprTag::Tail => 2,
        ExprTag::Unop => 2,
        ExprTag::Binop => 4,
        ExprTag::Binop2 => 3,
        ExprTag::If => 2,
        ExprTag::Let => 4,
        ExprTag::LetRec => 4,
        ExprTag::Emit => 1,
        _ => 0,
      }
    }
  }
}

impl<F: LurkField> PartialOrd for Ptr<F> {
  fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
    (
      F::from_tag_unchecked(self.tag).to_repr().as_ref(),
      self.val.to_repr().as_ref(),
    )
      .partial_cmp(&(
        F::from_tag_unchecked(other.tag).to_repr().as_ref(),
        other.val.to_repr().as_ref(),
      ))
  }
}

impl<F: LurkField> Ord for Ptr<F> {
  fn cmp(&self, other: &Self) -> core::cmp::Ordering {
    (
      F::from_tag_unchecked(self.tag).to_repr().as_ref(),
      self.val.to_repr().as_ref(),
    )
      .cmp(&(
        F::from_tag_unchecked(other.tag).to_repr().as_ref(),
        other.val.to_repr().as_ref(),
      ))
  }
}

impl<F: LurkField> fmt::Display for Ptr<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.tag.expr)?;
    write!(f, "{}", self.val.hex_digits())
  }
}

impl<F: LurkField> SerdeF<F> for Ptr<F> {
  fn ser_f(&self) -> Vec<F> { vec![F::from_tag_unchecked(self.tag), self.val] }

  fn de_f(fs: &[F]) -> Result<Ptr<F>, SerdeFError<F>> {
    match fs {
      &[tag, val, ..] => {
        let tag = F::to_tag(&tag).ok_or(SerdeFError::UnknownTag(tag))?;
        Ok(Ptr { tag, val })
      },
      _ => Err(SerdeFError::Expected("Ptr".to_string())),
    }
  }
}

#[cfg(feature = "test-utils")]
pub mod test_utils {
  use blstrs::Scalar as Fr;
  use lurk_ff::field::FWrap;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;
  impl Arbitrary for Ptr<Fr> {
    fn arbitrary(g: &mut Gen) -> Self {
      Ptr { tag: Arbitrary::arbitrary(g), val: FWrap::arbitrary(g).0 }
    }
  }
}

#[cfg(all(test, feature = "test-utils"))]
pub mod tests {
  use blstrs::Scalar as Fr;

  use super::*;

  #[quickcheck]
  fn prop_ptr_serdef(ptr: Ptr<Fr>) -> bool {
    match Ptr::de_f(&ptr.ser_f()) {
      Ok(ptr2) => ptr == ptr2,
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }

  #[quickcheck]
  fn prop_ptr_serde(ptr: Ptr<Fr>) -> bool {
    match Ptr::de(&ptr.ser()) {
      Ok(ptr2) => ptr == ptr2,
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }
}
