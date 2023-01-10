use bellperson::SynthesisError;
use ldon::{
  self,
  Cid,
};
use lurk_ff::LurkField;
use nova::errors::NovaError;

// use thiserror::Error;
use crate::{
  ptr::Ptr,
  store::Store,
};

#[derive(Debug, Clone)]
pub enum LurkError<F: LurkField> {
  Eval(String),
  Reduce(String),
  Store(StoreError<F>),
  // Parser(#[from] ParserError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StoreError<F: LurkField> {
  ExpectedExpr(Ptr<F>),
  UnknownPtr(Ptr<F>),
  CantCarCdr(Ptr<F>),
  UnknownCid(Cid<F>),
  InvalidOp1Ptr(Ptr<F>, String),
  InvalidOp2Ptr(Ptr<F>, String),
  LdonErr(ldon::StoreError<F>),
  MalformedStore(Ptr<F>),
  MalformedLdonStore(Cid<F>, ldon::Store<F>),
  Custom(&'static str),
}
