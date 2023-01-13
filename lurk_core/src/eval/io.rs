// use std::fmt;

// use log::info;
use lurk_ff::{
  ExprTag,
  LurkField,
};

use crate::{
  error::LurkError,
  eval::{
    evaluator::Evaluable,
    reduce,
    status::Status,
    witness::Witness,
  },
  expr::Expr,
  ptr::Ptr,
  store::Store,
};
// use crate::writer::Write,

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
      (ExprTag::Thunk, ExprTag::Dummy) => match store.get_expr(self.expr)? {
        Expr::Thunk(val, cont) if cont.tag.expr == ExprTag::Emit => Ok(val),
        _ => Err(LurkError::Custom("maybe_emmitted_expression")),
      },
      _ => Err(LurkError::Custom("maybe_emmitted_expression")),
    }
  }

  // TODO: Improve errors
  pub fn to_vector(&self, store: &Store<F>) -> Result<Vec<F>, LurkError<F>> {
    let expr_cid = store.get_expr_hash(self.expr)?;
    let env_cid = store.get_expr_hash(self.env)?;
    let cont_cid = store.hash_expr(self.cont)?;
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

impl<F: LurkField> Evaluable<F, Witness<F>> for IO<F> {
  fn reduce(
    &self,
    store: &mut Store<F>,
  ) -> Result<(Self, Witness<F>), LurkError<F>> {
    let (expr, env, cont, witness) =
      reduce(self.expr, self.env, self.cont, store)?;
    Ok((Self { expr, env, cont }, witness))
  }

  fn status(&self) -> Status { Status::from(self.cont) }

  fn is_complete(&self) -> bool { self.status().is_complete() }

  fn is_terminal(&self) -> bool { self.status().is_complete() }

  fn is_error(&self) -> bool { self.status().is_error() }

  fn log(&self, store: &Store<F>, i: usize) {}
  // fn log(&self, store: &Store<F>, i: usize) {
  //  info!(
  //    "Frame: {}\n\tExpr: {}\n\tEnv: {}\n\tCont: {}{}\n",
  //    i,
  //    self.expr.fmt_to_string(store),
  //    self.env.fmt_to_string(store),
  //    self.cont.fmt_to_string(store),
  //    if let Ok(emitted) = self.maybe_emitted_expression(store) {
  //      //format!("\n\tOutput: {}", emitted.fmt_to_string(store))
  //    }
  //    else {
  //      "".to_string()
  //    }
  //  );
  //}
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
