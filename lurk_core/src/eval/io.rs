use std::fmt;

use lurk_ff::{
  ExprTag,
  LurkField,
};

use crate::{
  error::LurkError,
  eval::{
    hash_witness::{
      ConsName,
      HashWitness,
    },
    status::Status,
  },
  expr::Expr,
  num::Num,
  ptr::Ptr,
  store::Store,
  writer::Write,
};

#[derive(Clone, Debug, PartialEq, Copy, Eq)]
pub struct IO<F: LurkField> {
  pub expr: Ptr<F>,
  pub env: Ptr<F>,
  pub cont: Ptr<F>,
}

impl<F: LurkField> IO<F> {
  // Returns any expression that was emitted in this IO (if an output) or
  // previous (if an input). The intention is that this method will be used to
  // extract and handle all output as needed.
  // TODO: Improve errors
  pub fn maybe_emitted_expression(
    &self,
    store: &Store<F>,
  ) -> Result<Ptr<F>, LurkError<F>> {
    match (self.expr.tag.expr, self.cont.tag.expr) {
      (ExprTag::Thunk, ExprTag::Dummy) => match store.get_expr(&self.expr)? {
        Expr::Thunk(val, cont) if cont.tag.expr == ExprTag::Emit => Ok(val),
        _ => Err(LurkError::Custom("maybe_emmitted_expression")),
      },
      _ => Err(LurkError::Custom("maybe_emmitted_expression")),
    }
  }

  // TODO: Improve errors
  pub fn to_vector(&self, store: &Store<F>) -> Result<Vec<F>, LurkError<F>> {
    let expr_cid = store.get_expr_hash(&self.expr)?;
    let env_cid = store.get_expr_hash(&self.env)?;
    let cont_cid = store.hash_expr(&self.cont)?;
    Ok(vec![
      F::from_tag(expr_cid.tag)
        .ok_or(LurkError::Custom("mismatched version and field"))?,
      expr_cid.val,
      F::from_tag(env_cid.tag)
        .ok_or(LurkError::Custom("mismatched version and field"))?,
      env_cid.val,
      F::from_tag(cont_cid.tag)
        .ok_or(LurkError::Custom("mismatched version and field"))?,
      cont_cid.val,
    ])
  }
}

// impl<F: LurkField> Write<F> for IO<F> {
//  fn fmt<W: std::io::Write>(
//    &self,
//    store: &Store<F>,
//    w: &mut W,
//  ) -> std::io::Result<()> {
//    write!(w, "IO {{ expr: ")?;
//    self.expr.fmt(store, w)?;
//    write!(w, ", env: ")?;
//    self.env.fmt(store, w)?;
//    write!(w, ", cont: ")?;
//    self.cont.fmt(store, w)?;
//    write!(w, " }}")
//  }
//}
