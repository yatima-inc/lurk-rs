use std::{
  cmp::PartialEq,
  iter::{
    Iterator,
    Take,
  },
};

use log::info;
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
};

pub mod cons;
pub mod frame;
pub mod hash_witness;
pub mod io;
pub mod status;
pub mod witness;

pub trait Evaluable<F: LurkField, W> {
  fn reduce(&self, store: &mut Store<F>) -> Result<(Self, W), LurkError<F>>
  where Self: Sized;

  fn status(&self) -> Status;
  fn is_complete(&self) -> bool;
  fn is_terminal(&self) -> bool;
  fn is_error(&self) -> bool;

  fn log(&self, store: &Store<F>, i: usize);
}

//  pub fn input_vector(
//    &self,
//    store: &Store<F>,
//  ) -> Result<Vec<F>, LurkError<F>> {
//    self.input.to_vector(store)
//  }
//
//  pub fn output_vector(
//    &self,
//    store: &Store<F>,
//  ) -> Result<Vec<F>, LurkError<F>> {
//    self.output.to_vector(store)
//  }
//}

// pub trait Evaluable<F: LurkField, W> {
//  fn reduce(&self, store: &mut Store<F>) -> Result<(Self, W), LurkError>
//  where Self: Sized;
//
//  fn status(&self) -> Status;
//  fn is_complete(&self) -> bool;
//  fn is_terminal(&self) -> bool;
//  fn is_error(&self) -> bool;
//
//  fn log(&self, store: &Store<F>, i: usize);
//}
// impl<F: LurkField> Evaluable<F, Witness<F>> for IO<F> {
//  fn reduce(
//    &self,
//    store: &mut Store<F>,
//  ) -> Result<(Self, Witness<F>), LurkError> {
//    let (expr, env, cont, witness) =
//      reduce(self.expr, self.env, self.cont, store)?;
//    Ok((Self { expr, env, cont }, witness))
//  }
//
//  fn status(&self) -> Status { Status::from(self.cont) }
//
//  fn is_complete(&self) -> bool { self.status().is_complete() }
//
//  fn is_terminal(&self) -> bool { self.status().is_complete() }
//
//  fn is_error(&self) -> bool { self.status().is_error() }
//
//  fn log(&self, store: &Store<F>, i: usize) {
//    info!(
//      "Frame: {}\n\tExpr: {}\n\tEnv: {}\n\tCont: {}{}\n",
//      i,
//      self.expr.fmt_to_string(store),
//      self.env.fmt_to_string(store),
//      self.cont.fmt_to_string(store),
//      if let Some(emitted) = self.maybe_emitted_expression(store) {
//        format!("\n\tOutput: {}", emitted.fmt_to_string(store))
//      }
//      else {
//        "".to_string()
//      }
//    );
//  }
//}
