use lurk_ff::LurkField;

use crate::{
  eval::hash_witness::HashWitness,
  ptr::Ptr,
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Witness<F: LurkField> {
  pub(crate) prethunk_output_expr: Ptr<F>,
  pub(crate) prethunk_output_env: Ptr<F>,
  pub(crate) prethunk_output_cont: Ptr<F>,

  pub(crate) closure_to_extend: Option<Ptr<F>>,
  pub(crate) apply_continuation_cont: Option<Ptr<F>>,
  pub(crate) hashes: HashWitness<F>,
}
