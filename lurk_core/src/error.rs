use ldon::{
  self,
  Cid,
};
use lurk_ff::{
  ExprTag,
  LurkField,
};
use nova::errors::NovaError;

// use thiserror::Error;
use crate::ptr::Ptr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LurkError<F: LurkField> {
  Nova(NovaError),
  // bellperson::SynthesisError doesn't implement Clone and Eq, so if we hit
  // this convert it to its Debug string.
  Synthesis(String),
  Eval(String),
  Reduce(String),
  // StoreErrors
  ExpectedExpr(Ptr<F>),
  GetUnknownPtr(Ptr<F>),
  GetOpaque(ExprTag, usize),
  GetIndex(ExprTag, usize),
  CantCarCdr(Ptr<F>),
  UnknownCid(Cid<F>),
  InvalidOp1Ptr(Ptr<F>, String),
  InvalidOp2Ptr(Ptr<F>, String),
  LdonStore(ldon::StoreError<F>),
  MalformedStore(Ptr<F>),
  MalformedLdonStore(Cid<F>, ldon::Store<F>),
  Incomplete,
  Custom(&'static str),
}

impl<F: LurkField> LurkError<F> {
  pub fn synthesis_error(value: bellperson::SynthesisError) -> Self {
    Self::Synthesis(format!("{:?}", value))
  }
}
