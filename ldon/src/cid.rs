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
pub struct Cid<F: LurkField> {
  pub tag: Tag,
  pub val: F,
}

#[allow(clippy::derive_hash_xor_eq)]
impl<F: LurkField> Hash for Cid<F> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.tag.hash(state);
    FWrap(self.val).hash(state);
  }
}

impl<F: LurkField> Cid<F> {
  pub fn immediate(self) -> Option<Expr<F>> {
    match self.tag.expr {
      ExprTag::Num => Some(Expr::Num(self.val)),
      ExprTag::Char => Some(Expr::Char(self.val)),
      ExprTag::U64 => Some(Expr::U64(self.val)),
      ExprTag::Str if self.val == F::zero() => Some(Expr::StrNil),
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

  pub fn child_cid_arity(self) -> usize {
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

impl<F: LurkField> PartialOrd for Cid<F> {
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

impl<F: LurkField> Ord for Cid<F> {
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

impl<F: LurkField> fmt::Display for Cid<F> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.tag.expr)?;
    match self.tag.expr {
      ExprTag::Char => {
        if let Some(c) = F::to_char(&self.val) {
          write!(f, "{}", c)
        }
        else {
          write!(f, "{}", self.val.hex_digits())
        }
      },
      _ => write!(f, "{}", self.val.hex_digits()),
    }
  }
}

impl<F: LurkField> SerdeF<F> for Cid<F> {
  fn ser_f(&self) -> Vec<F> { vec![F::from_tag_unchecked(self.tag), self.val] }

  fn de_f(fs: &[F]) -> Result<Cid<F>, SerdeFError<F>> {
    match fs {
      &[tag, val, ..] => {
        let tag = F::to_tag(&tag).ok_or(SerdeFError::UnknownTag(tag))?;
        Ok(Cid { tag, val })
      },
      _ => Err(SerdeFError::Expected("Cid".to_string())),
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
  impl Arbitrary for Cid<Fr> {
    fn arbitrary(g: &mut Gen) -> Self {
      Cid { tag: Arbitrary::arbitrary(g), val: FWrap::arbitrary(g).0 }
    }
  }
}

#[cfg(all(test, feature = "test-utils"))]
pub mod tests {
  use blstrs::Scalar as Fr;

  use super::*;

  #[quickcheck]
  fn prop_ptr_serdef(ptr: Cid<Fr>) -> bool {
    match Cid::de_f(&ptr.ser_f()) {
      Ok(ptr2) => ptr == ptr2,
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }

  #[quickcheck]
  fn prop_ptr_serde(ptr: Cid<Fr>) -> bool {
    match Cid::de(&ptr.ser()) {
      Ok(ptr2) => ptr == ptr2,
      Err(e) => {
        println!("{:?}", e);
        false
      },
    }
  }
}
