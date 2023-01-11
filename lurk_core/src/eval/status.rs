use std::cmp::PartialEq;

use lurk_ff::{
  ExprTag,
  LurkField,
};

use crate::{
  error::LurkError,
  expr::Expr,
  ptr::Ptr,
  store::Store,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
  Terminal,
  Error,
  Incomplete,
}

impl Default for Status {
  fn default() -> Self { Self::Incomplete }
}

impl Status {
  pub fn is_complete(&self) -> bool {
    match self {
      Self::Terminal | Self::Error => true,
      Self::Incomplete => false,
    }
  }

  pub fn is_terminal(&self) -> bool {
    match self {
      Self::Terminal => true,
      Self::Incomplete | Self::Error => false,
    }
  }

  pub fn is_error(&self) -> bool {
    match self {
      Self::Error => true,
      Self::Terminal | Self::Incomplete => false,
    }
  }

  pub fn is_incomplete(&self) -> bool {
    match self {
      Self::Incomplete => true,
      Self::Terminal | Self::Error => false,
    }
  }

  pub fn to_cont<F: LurkField>(
    &self,
    s: &mut Store<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    match self {
      Self::Terminal => s.intern_expr(Expr::Terminal),
      Self::Error => s.intern_expr(Expr::Error),
      Self::Incomplete => Err(LurkError::Incomplete),
    }
  }
}

impl<F: LurkField> From<Ptr<F>> for Status {
  fn from(cont: Ptr<F>) -> Self {
    match cont.tag.expr {
      ExprTag::Terminal => Self::Terminal,
      ExprTag::Error => Self::Error,
      _ => Self::Incomplete,
    }
  }
}
