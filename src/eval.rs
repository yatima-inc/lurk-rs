use std::{
  cmp::PartialEq,
  iter::{
    Iterator,
    Take,
  },
};

use log::info;
use serde::{
  Deserialize,
  Serialize,
};

use crate::{
  error::LurkError,
  field::LurkField,
  hash_witness::{
    ConsName,
    HashWitness,
  },
  num::Num,
  store::{
    ContPtr,
    ContTag,
    Continuation,
    Expression,
    Op1,
    Op2,
    Pointer,
    Ptr,
    ScalarPointer,
    Store,
    Tag,
    Thunk,
  },
  writer::Write,
};

#[derive(Clone, Debug, PartialEq, Copy, Eq)]
pub struct IO<F: LurkField> {
  pub expr: Ptr<F>,
  pub env: Ptr<F>,
  pub cont: ContPtr<F>, /* This could be a Ptr too, if we want Continuations
                         * to be first class. */
}

impl<F: LurkField> Write<F> for IO<F> {
  fn fmt<W: std::io::Write>(
    &self,
    store: &Store<F>,
    w: &mut W,
  ) -> std::io::Result<()> {
    write!(w, "IO {{ expr: ")?;
    self.expr.fmt(store, w)?;
    write!(w, ", env: ")?;
    self.env.fmt(store, w)?;
    write!(w, ", cont: ")?;
    self.cont.fmt(store, w)?;
    write!(w, " }}")
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Frame<T: Copy, W: Copy> {
  pub input: T,
  pub output: T,
  pub i: usize,
  pub witness: W,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

  pub fn to_cont<F: LurkField>(&self, s: &mut Store<F>) -> Option<ContPtr<F>> {
    match self {
      Self::Terminal => Some(s.intern_cont_terminal()),
      Self::Error => Some(s.intern_cont_error()),
      Self::Incomplete => None,
    }
  }
}

impl<F: LurkField> From<ContPtr<F>> for Status {
  fn from(cont: ContPtr<F>) -> Self {
    match cont.tag() {
      ContTag::Terminal => Self::Terminal,
      ContTag::Error => Self::Error,
      _ => Self::Incomplete,
    }
  }
}

impl<F: LurkField, W: Copy> Frame<IO<F>, W> {
  pub fn precedes(&self, maybe_next: &Self) -> bool {
    let sequential = self.i + 1 == maybe_next.i;
    let io_match = self.output == maybe_next.input;

    sequential && io_match
  }

  pub fn is_complete(&self) -> bool {
    self.input == self.output && self.output.is_complete()
  }

  pub fn log(&self, store: &Store<F>) {
    // This frame's output is the input for the next frame.
    // Report that index. Otherwise we can't report the initial input.
    self.output.log(store, self.i + 1);
  }

  pub fn significant_frame_count(frames: &[Frame<IO<F>, W>]) -> usize {
    frames.iter().rev().skip_while(|frame| frame.is_complete()).count()
  }

  pub fn input_vector(&self, store: &Store<F>) -> Result<Vec<F>, LurkError> {
    self.input.to_vector(store)
  }

  pub fn output_vector(&self, store: &Store<F>) -> Result<Vec<F>, LurkError> {
    self.output.to_vector(store)
  }
}

pub trait Evaluable<F: LurkField, W> {
  fn reduce(&self, store: &mut Store<F>) -> Result<(Self, W), LurkError>
  where Self: Sized;

  fn status(&self) -> Status;
  fn is_complete(&self) -> bool;
  fn is_terminal(&self) -> bool;
  fn is_error(&self) -> bool;

  fn log(&self, store: &Store<F>, i: usize);
}

impl<F: LurkField> Evaluable<F, Witness<F>> for IO<F> {
  fn reduce(
    &self,
    store: &mut Store<F>,
  ) -> Result<(Self, Witness<F>), LurkError> {
    let (expr, env, cont, witness) =
      reduce(self.expr, self.env, self.cont, store)?;
    Ok((Self { expr, env, cont }, witness))
  }

  fn status(&self) -> Status { Status::from(self.cont) }

  fn is_complete(&self) -> bool { self.status().is_complete() }

  fn is_terminal(&self) -> bool { self.status().is_complete() }

  fn is_error(&self) -> bool { self.status().is_error() }

  fn log(&self, store: &Store<F>, i: usize) {
    info!(
      "Frame: {}\n\tExpr: {}\n\tEnv: {}\n\tCont: {}{}\n",
      i,
      self.expr.fmt_to_string(store),
      self.env.fmt_to_string(store),
      self.cont.fmt_to_string(store),
      if let Some(emitted) = self.maybe_emitted_expression(store) {
        format!("\n\tOutput: {}", emitted.fmt_to_string(store))
      }
      else {
        "".to_string()
      }
    );
  }
}

impl<F: LurkField> IO<F> {
  // Returns any expression that was emitted in this IO (if an output) or
  // previous (if an input). The intention is that this method will be used to
  // extract and handle all output as needed.
  pub fn maybe_emitted_expression(&self, store: &Store<F>) -> Option<Ptr<F>> {
    if self.expr.tag() == crate::store::Tag::Thunk
      && self.cont.tag() == crate::store::ContTag::Dummy
    {
      if let Some(Expression::Thunk(thunk)) = store.fetch(&self.expr) {
        if thunk.continuation.tag() == crate::store::ContTag::Emit {
          Some(thunk.value)
        }
        else {
          None
        }
      }
      else {
        None
      }
    }
    else {
      None
    }
  }

  pub fn to_vector(&self, store: &Store<F>) -> Result<Vec<F>, LurkError> {
    let expr_scalar_ptr = store
      .get_expr_hash(&self.expr)
      .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;
    let env_scalar_ptr = store
      .get_expr_hash(&self.env)
      .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;
    let cont_scalar_ptr = store
      .hash_cont(&self.cont)
      .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;
    Ok(vec![
      *expr_scalar_ptr.tag(),
      *expr_scalar_ptr.value(),
      *env_scalar_ptr.tag(),
      *env_scalar_ptr.value(),
      *cont_scalar_ptr.tag(),
      *cont_scalar_ptr.value(),
    ])
  }
}

impl<F: LurkField, T: Evaluable<F, Witness<F>> + Clone + PartialEq + Copy>
  Frame<T, Witness<F>>
{
  pub(crate) fn next(&self, store: &mut Store<F>) -> Result<Self, LurkError> {
    let input = self.output;
    let (output, witness) = input.reduce(store)?;

    // FIXME: Why isn't this method found?
    // self.log(store);
    self.output.log(store, self.i + 1);
    Ok(Self { input, output, i: self.i + 1, witness })
  }
}

impl<F: LurkField, T: Evaluable<F, Witness<F>> + Clone + PartialEq + Copy>
  Frame<T, Witness<F>>
{
  fn from_initial_input(
    input: T,
    store: &mut Store<F>,
  ) -> Result<Self, LurkError> {
    input.log(store, 0);
    let (output, witness) = input.reduce(store)?;
    Ok(Self { input, output, i: 0, witness })
  }
}

#[derive(Debug)]
pub struct FrameIt<'a, W: Copy, F: LurkField> {
  first: bool,
  frame: Frame<IO<F>, W>,
  store: &'a mut Store<F>,
}

impl<'a, F: LurkField> FrameIt<'a, Witness<F>, F> {
  fn new(
    initial_input: IO<F>,
    store: &'a mut Store<F>,
  ) -> Result<Self, LurkError> {
    let frame = Frame::from_initial_input(initial_input, store)?;
    Ok(Self { first: true, frame, store })
  }

  /// Like `.iter().take(n).last()`, but skips intermediary stages, to optimize
  /// for evaluation.
  fn next_n(
    mut self,
    n: usize,
  ) -> Result<
    (Frame<IO<F>, Witness<F>>, Frame<IO<F>, Witness<F>>, Vec<Ptr<F>>),
    LurkError,
  > {
    let mut previous_frame = self.frame.clone();
    let mut emitted: Vec<Ptr<F>> = Vec::new();
    for _ in 0..n {
      if self.frame.is_complete() {
        break;
      }
      let new_frame = self.frame.next(self.store)?;

      if let Some(expr) = new_frame.output.maybe_emitted_expression(self.store)
      {
        emitted.push(expr);
      }
      previous_frame = std::mem::replace(&mut self.frame, new_frame);
    }
    Ok((self.frame, previous_frame, emitted))
  }
}

// Wrapper struct to preserve errors that would otherwise be lost during
// iteration
#[derive(Debug)]
struct ResultFrame<'a, F: LurkField>(
  Result<FrameIt<'a, Witness<F>, F>, LurkError>,
);

impl<'a, F: LurkField> Iterator for ResultFrame<'a, F> {
  type Item = Result<Frame<IO<F>, Witness<F>>, LurkError>;

  fn next(&mut self) -> Option<<Self as Iterator>::Item> {
    let mut frame_it = match &mut self.0 {
      Ok(f) => f,
      Err(e) => return Some(Err(e.clone())),
    };
    // skip first iteration, as one evaluation happens on construction
    if frame_it.first {
      frame_it.first = false;
      return Some(Ok(frame_it.frame.clone()));
    }

    if frame_it.frame.is_complete() {
      return None;
    }

    frame_it.frame = match frame_it.frame.next(frame_it.store) {
      Ok(f) => f,
      Err(e) => return Some(Err(e)),
    };

    Some(Ok(frame_it.frame.clone()))
  }
}

impl<'a, F: LurkField> Iterator for FrameIt<'a, Witness<F>, F> {
  type Item = Frame<IO<F>, Witness<F>>;

  fn next(&mut self) -> Option<<Self as Iterator>::Item> {
    // skip first iteration, as one evaluation happens on construction
    if self.first {
      self.first = false;
      return Some(self.frame.clone());
    }

    if self.frame.is_complete() {
      return None;
    }

    // TODO: Error info lost here
    self.frame = self.frame.next(self.store).ok()?;

    Some(self.frame.clone())
  }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Witness<F: LurkField> {
  pub(crate) prethunk_output_expr: Ptr<F>,
  pub(crate) prethunk_output_env: Ptr<F>,
  pub(crate) prethunk_output_cont: ContPtr<F>,

  pub(crate) closure_to_extend: Option<Ptr<F>>,
  pub(crate) apply_continuation_cont: Option<ContPtr<F>>,
  pub(crate) hashes: HashWitness<F>,
}

fn reduce<F: LurkField>(
  expr: Ptr<F>,
  env: Ptr<F>,
  cont: ContPtr<F>,
  store: &mut Store<F>,
) -> Result<(Ptr<F>, Ptr<F>, ContPtr<F>, Witness<F>), LurkError> {
  let (ctrl, witness) = reduce_with_witness(expr, env, cont, store)?;
  let (new_expr, new_env, new_cont) = ctrl.into_results();

  Ok((new_expr, new_env, new_cont, witness))
}

#[derive(Debug, Clone)]
pub enum Control<F: LurkField> {
  Return(Ptr<F>, Ptr<F>, ContPtr<F>),
  MakeThunk(Ptr<F>, Ptr<F>, ContPtr<F>),
  ApplyContinuation(Ptr<F>, Ptr<F>, ContPtr<F>),
}

impl<F: LurkField> Control<F> {
  pub fn as_results(&self) -> (&Ptr<F>, &Ptr<F>, &ContPtr<F>) {
    match self {
      Self::Return(expr, env, cont) => (expr, env, cont),
      Self::MakeThunk(expr, env, cont) => (expr, env, cont),
      Self::ApplyContinuation(expr, env, cont) => (expr, env, cont),
    }
  }

  pub fn into_results(self) -> (Ptr<F>, Ptr<F>, ContPtr<F>) {
    match self {
      Self::Return(expr, env, cont) => (expr, env, cont),
      Self::MakeThunk(expr, env, cont) => (expr, env, cont),
      Self::ApplyContinuation(expr, env, cont) => (expr, env, cont),
    }
  }

  pub fn is_return(&self) -> bool { matches!(self, Self::Return(_, _, _)) }

  pub fn is_make_thunk(&self) -> bool {
    matches!(self, Self::MakeThunk(_, _, _))
  }

  pub fn is_apply_continuation(&self) -> bool {
    matches!(self, Self::ApplyContinuation(_, _, _))
  }
}

fn reduce_with_witness<F: LurkField>(
  expr: Ptr<F>,
  env: Ptr<F>,
  cont: ContPtr<F>,
  store: &mut Store<F>,
) -> Result<(Control<F>, Witness<F>), LurkError> {
  let hash_witness = &mut HashWitness::<F>::new_dummy();
  let mut closure_to_extend = None;
  let control = if cont.tag() == ContTag::Terminal {
    Control::Return(expr, env, cont)
  }
  else {
    match expr.tag() {
      Tag::Thunk => match store
        .fetch(&expr)
        .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
      {
        Expression::Thunk(thunk) => {
          Control::ApplyContinuation(thunk.value, env, thunk.continuation)
        },
        _ => unreachable!(),
      },
      // Self-evaluating
      Tag::Nil
      | Tag::Num
      | Tag::Fun
      | Tag::Char
      | Tag::Str
      | Tag::Comm
      | Tag::U64
      | Tag::Key => Control::ApplyContinuation(expr, env, cont),
      Tag::Sym => {
        if expr == store.lurk_sym("nil") || (expr == store.t()) {
          // NIL and T are self-evaluating symbols, pass them to the
          // continuation in a thunk.

          // CIRCUIT: sym_is_self_evaluating
          //          cond1
          Control::ApplyContinuation(expr, env, cont)
        }
        else {
          // Otherwise, look for a matching binding in env.

          // CIRCUIT: sym_otherwise
          if env.is_nil() {
            //     //assert!(!env.is_nil(), "Unbound variable: {:?}", expr);
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            let (binding, smaller_env) =
              hash_witness.car_cdr_named(ConsName::Env, store, &env);
            if binding.is_nil() {
              // If binding is NIL, it's empty. There is no match. Return an
              // error due to unbound variable.

              // CIRCUIT: binding_is_nil
              //          otherwise_and_binding_is_nil
              //          cond2
              Control::Return(expr, env, store.intern_cont_error())
            }
            else {
              // Binding is not NIL, so it is either a normal binding or a
              // recursive environment.

              // CIRCUIT: binding_not_nil
              //          otherwise_and_binding_not_nil
              let (var_or_rec_binding, val_or_more_rec_env) =
                hash_witness.car_cdr_named(ConsName::EnvCar, store, &binding);

              match var_or_rec_binding.tag() {
                Tag::Sym => {
                  // We are in a simple env (not a recursive env),
                  // looking at a binding's variable.

                  // CIRCUIT: var_or_rec_binding_is_sym
                  //          otherwise_and_sym
                  let v = var_or_rec_binding;
                  let val = val_or_more_rec_env;

                  if v == expr {
                    // expr matches the binding's var.

                    // CIRCUIT: v_is_expr1
                    //          v_is_expr1_real
                    //          otherwise_and_v_expr_and_sym
                    //          cond3

                    // Pass the binding's value to the continuation in a thunk.
                    Control::ApplyContinuation(val, env, cont)
                  }
                  else {
                    // expr does not match the binding's var.

                    // CIRCUIT: otherwise_and_v_not_expr
                    match cont.tag() {
                      ContTag::Lookup => {
                        // If performing a lookup, continue with remaining env.

                        // CIRCUIT: cont_is_lookup
                        //          cont_is_lookup_sym
                        //          cond4
                        Control::Return(expr, smaller_env, cont)
                      },
                      _ =>
                      // Otherwise, create a lookup continuation, packaging
                      // current env to be restored later.

                      // CIRCUIT: cont_not_lookup_sym
                      //          cond5
                      {
                        Control::Return(
                          expr,
                          smaller_env,
                          store.intern_cont_lookup(env, cont),
                        )
                      },
                    }
                  }
                },
                // Start of a recursive_env.
                Tag::Cons => {
                  // CIRCUIT: var_or_rec_binding_is_cons
                  //          otherwise_and_cons
                  let rec_env = binding;
                  let smaller_rec_env = val_or_more_rec_env;

                  let (v2, val2) = hash_witness.car_cdr_named(
                    ConsName::EnvCaar,
                    store,
                    &var_or_rec_binding,
                  );

                  if v2 == expr {
                    // CIRCUIT: v2_is_expr
                    //          v2_is_expr_real
                    //          cond6
                    let val_to_use = {
                      // CIRCUIT: val_to_use
                      match val2.tag() {
                        Tag::Fun => {
                          closure_to_extend = Some(val2);
                          // CIRCUIT: val2_is_fun

                          // We just found a closure in a recursive env.
                          // We need to extend its environment to include that
                          // recursive env.

                          extend_closure(&val2, &rec_env, store, hash_witness)?
                        },
                        _ => {
                          closure_to_extend = None;
                          val2
                        },
                      }
                    };
                    Control::ApplyContinuation(val_to_use, env, cont)
                  }
                  else {
                    // CIRCUIT: v2_not_expr
                    //          otherwise_and_v2_not_expr
                    //          cond7
                    let env_to_use = if smaller_rec_env.is_nil() {
                      // CIRCUIT: smaller_rec_env_is_nil
                      smaller_env
                    }
                    else {
                      // CIRCUIT: with_smaller_rec_env
                      hash_witness.cons_named(
                        ConsName::EnvToUse,
                        store,
                        smaller_rec_env,
                        smaller_env,
                      )
                    };
                    match cont.tag() {
                      ContTag::Lookup => {
                        // CIRCUIT: cont_is_lookup
                        //          cont_is_lookup_cons
                        //          cond8
                        Control::Return(expr, env_to_use, cont)
                      },
                      _ => Control::Return(
                        // CIRCUIT: cont_not_lookup_cons
                        //          cond9
                        expr,
                        env_to_use,
                        store.intern_cont_lookup(env, cont),
                      ),
                    }
                  }
                },
                _ => return Err(LurkError::Reduce("Bad form.".into())),
              }
            }
          }
        }
      },
      Tag::Cons => {
        // This should not fail, since expr is a Cons.
        let (head, rest) =
          hash_witness.car_cdr_named(ConsName::Expr, store, &expr);

        let lambda = store.lurk_sym("lambda");
        let quote = store.lurk_sym("quote");
        let dummy_arg = store.lurk_sym("_");

        if head == lambda {
          let (args, body) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          let (arg, _rest) = if args.is_nil() {
            // (LAMBDA () STUFF)
            // becomes (LAMBDA (DUMMY) STUFF)
            (dummy_arg, store.nil())
          }
          else {
            hash_witness.car_cdr_named(ConsName::ExprCadr, store, &args)
          };
          let (_, cdr_args) =
            hash_witness.car_cdr_named(ConsName::ExprCadr, store, &args);
          let inner_body = if cdr_args.is_nil() {
            body
          }
          else {
            // (LAMBDA (A B) STUFF)
            // becomes (LAMBDA (A) (LAMBDA (B) STUFF))
            let inner = hash_witness.cons_named(
              ConsName::InnerLambda,
              store,
              cdr_args,
              body,
            );
            let l =
              hash_witness.cons_named(ConsName::Lambda, store, lambda, inner);
            let nil = store.nil();
            hash_witness.cons_named(ConsName::InnerBody, store, l, nil)
          };
          let function = store.intern_fun(arg, inner_body, env);

          Control::ApplyContinuation(function, env, cont)
        }
        else if head == quote {
          let (quoted, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::ApplyContinuation(quoted, env, cont)
          }
        }
        else if head == store.lurk_sym("let")
          || head == store.lurk_sym("letrec")
        {
          let (bindings, body) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          let (body1, rest_body) =
            hash_witness.car_cdr_named(ConsName::ExprCddr, store, &body);
          // Only a single body form allowed for now.
          if !rest_body.is_nil() || body.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else if bindings.is_nil() {
            Control::Return(body1, env, cont)
          }
          else {
            let (binding1, rest_bindings) =
              hash_witness.car_cdr_named(ConsName::ExprCadr, store, &bindings);
            let (var, vals) =
              hash_witness.car_cdr_named(ConsName::ExprCaadr, store, &binding1);
            let (val, end) =
              hash_witness.car_cdr_named(ConsName::ExprCaaadr, store, &vals);

            if !end.is_nil() {
              Control::Return(expr, env, store.intern_cont_error())
            }
            else {
              let expanded = if rest_bindings.is_nil() {
                body1
              }
              else {
                // We know body is a proper list equivalent to (body1), if this
                // branch was taken, since end is nil.
                let expanded0 = hash_witness.cons_named(
                  ConsName::ExpandedInner,
                  store,
                  rest_bindings,
                  body,
                );
                hash_witness.cons_named(
                  ConsName::Expanded,
                  store,
                  head,
                  expanded0,
                )
              };
              let cont = if head == store.lurk_sym("let") {
                store.intern_cont_let(var, expanded, env, cont)
              }
              else {
                store.intern_cont_let_rec(var, expanded, env, cont)
              };
              Control::Return(val, env, cont)
            }
          }
        }
        else if head == store.lurk_sym("cons") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if more.is_nil() {
            Control::Return(arg1, env, store.intern_cont_error())
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_binop(Op2::Cons, env, more, cont),
            )
          }
        }
        else if head == store.lurk_sym("strcons") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if more.is_nil() {
            Control::Return(arg1, env, store.intern_cont_error())
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_binop(Op2::StrCons, env, more, cont),
            )
          }
        }
        else if head == store.lurk_sym("hide") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if more.is_nil() {
            Control::Return(arg1, env, store.intern_cont_error())
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_binop(Op2::Hide, env, more, cont),
            )
          }
        }
        else if head == store.lurk_sym("begin") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if more.is_nil() {
            Control::Return(arg1, env, cont)
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_binop(Op2::Begin, env, more, cont),
            )
          }
        }
        else if head == store.lurk_sym("car") {
          let (arg1, end) = match hash_witness.car_cdr_mut_named(
            ConsName::ExprCdr,
            store,
            &rest,
          ) {
            Ok((car, cdr)) => (car, cdr),
            Err(e) => return Err(LurkError::Reduce(e)),
          };
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Car, cont))
          }
        }
        else if head == store.lurk_sym("cdr") {
          let (arg1, end) = match hash_witness.car_cdr_mut_named(
            ConsName::ExprCdr,
            store,
            &rest,
          ) {
            Ok((car, cdr)) => (car, cdr),
            Err(e) => return Err(LurkError::Reduce(e)),
          };
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Cdr, cont))
          }
        }
        else if head == store.lurk_sym("commit") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_unop(Op1::Commit, cont),
            )
          }
        }
        else if head == store.lurk_sym("num") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Num, cont))
          }
        }
        else if head == store.lurk_sym("u64") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::U64, cont))
          }
        }
        else if head == store.lurk_sym("comm") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Comm, cont))
          }
        }
        else if head == store.lurk_sym("char") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Char, cont))
          }
        }
        else if head == store.lurk_sym("eval") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if more.is_nil() {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Eval, cont))
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_binop(Op2::Eval, env, more, cont),
            )
          }
        }
        else if head == store.lurk_sym("open") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Open, cont))
          }
        }
        else if head == store.lurk_sym("secret") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(
              arg1,
              env,
              store.intern_cont_unop(Op1::Secret, cont),
            )
          }
        }
        else if head == store.lurk_sym("atom") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Atom, cont))
          }
        }
        else if head == store.lurk_sym("emit") {
          let (arg1, end) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          if !end.is_nil() {
            Control::Return(expr, env, store.intern_cont_error())
          }
          else {
            Control::Return(arg1, env, store.intern_cont_unop(Op1::Emit, cont))
          }
        }
        else if head == store.lurk_sym("+") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Sum, env, more, cont),
          )
        }
        else if head == store.lurk_sym("-") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Diff, env, more, cont),
          )
        }
        else if head == store.lurk_sym("*") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Product, env, more, cont),
          )
        }
        else if head == store.lurk_sym("/") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Quotient, env, more, cont),
          )
        }
        else if head == store.lurk_sym("%") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Modulo, env, more, cont),
          )
        }
        else if head == store.lurk_sym("=") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::NumEqual, env, more, cont),
          )
        }
        else if head == store.lurk_sym("eq") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Equal, env, more, cont),
          )
        }
        else if head == store.lurk_sym("<") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Less, env, more, cont),
          )
        }
        else if head == store.lurk_sym(">") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::Greater, env, more, cont),
          )
        }
        else if head == store.lurk_sym("<=") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::LessEqual, env, more, cont),
          )
        }
        else if head == store.lurk_sym(">=") {
          let (arg1, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(
            arg1,
            env,
            store.intern_cont_binop(Op2::GreaterEqual, env, more, cont),
          )
        }
        else if head == store.lurk_sym("if") {
          let (condition, more) =
            hash_witness.car_cdr_named(ConsName::ExprCdr, store, &rest);
          Control::Return(condition, env, store.intern_cont_if(more, cont))
        }
        else if head == store.lurk_sym("current-env") {
          if !rest.is_nil() {
            Control::Return(env, env, store.intern_cont_error())
          }
          else {
            Control::ApplyContinuation(env, env, cont)
          }
        }
        else {
          // (fn . args)
          let fun_form = head;
          let args = rest;
          if args.is_nil() {
            Control::Return(fun_form, env, store.intern_cont_call0(env, cont))
          }
          else {
            let (arg, more_args) =
              hash_witness.car_cdr_named(ConsName::ExprCdr, store, &args);
            match more_args.tag() {
              // (fn arg)
              // Interpreting as call.
              Tag::Nil => Control::Return(
                fun_form,
                env,
                store.intern_cont_call(arg, env, cont),
              ),
              _ => {
                // Interpreting as multi-arg call.
                // (fn arg . more_args) => ((fn arg) . more_args)
                let nil = store.nil();
                let expanded_inner0 = hash_witness.cons_named(
                  ConsName::ExpandedInner0,
                  store,
                  arg,
                  nil,
                );
                let expanded_inner = hash_witness.cons_named(
                  ConsName::ExpandedInner,
                  store,
                  fun_form,
                  expanded_inner0,
                );
                let expanded = hash_witness.cons_named(
                  ConsName::FunExpanded,
                  store,
                  expanded_inner,
                  more_args,
                );
                Control::Return(expanded, env, cont)
              },
            }
          }
        }
      },
    }
  };

  let (new_expr, new_env, new_cont) = control.as_results();

  let mut witness = Witness {
    prethunk_output_expr: *new_expr,
    prethunk_output_env: *new_env,
    prethunk_output_cont: *new_cont,

    closure_to_extend,
    apply_continuation_cont: None,
    hashes: *hash_witness,
  };

  let control = apply_continuation(control, store, &mut witness)?;
  let ctrl = make_thunk(control, store, &mut witness)?;

  witness.hashes.assert_invariants(store);

  Ok((ctrl, witness))
}

fn apply_continuation<F: LurkField>(
  control: Control<F>,
  store: &mut Store<F>,
  witness: &mut Witness<F>,
) -> Result<Control<F>, LurkError> {
  if !control.is_apply_continuation() {
    return Ok(control);
  }

  let hash_witness = &mut witness.hashes;

  let (result, env, cont) = control.as_results();
  witness.apply_continuation_cont = Some(*cont);

  let control = match cont.tag() {
    ContTag::Terminal | ContTag::Error => Control::Return(*result, *env, *cont),
    ContTag::Dummy => {
      unreachable!("Dummy Continuation should never be applied.")
    },
    ContTag::Outermost => {
      Control::Return(*result, *env, store.intern_cont_terminal())
    },
    ContTag::Emit => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      // Although Emit has no effect within the computation, it has an
      // externally-visible side effect of manifesting an explicit Thunk
      // in the expr register of the execution trace.
      Continuation::Emit { continuation } => {
        Control::MakeThunk(*result, *env, continuation)
      },
      _ => unreachable!(),
    },
    ContTag::Call0 => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Call0 { saved_env, continuation } => match result.tag() {
        Tag::Fun => match store
          .fetch(result)
          .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
        {
          Expression::Fun(arg, body, closed_env) => {
            if arg == store.lurk_sym("_") {
              let (body_form, _) =
                hash_witness.car_cdr_named(ConsName::FunBody, store, &body);
              let cont = make_tail_continuation(saved_env, continuation, store);

              Control::Return(body_form, closed_env, cont)
            }
            else {
              // // Applying zero args to a non-zero arg function leaves it
              // unchanged. // This is arguably consistent with
              // auto-currying.
              Control::Return(*result, *env, continuation)
            }
          },
          _ => unreachable!(),
        }, // Bad function
        _ => Control::Return(*result, *env, store.intern_cont_error()),
      },
      _ => unreachable!(),
    },

    ContTag::Call => match result.tag() {
      Tag::Fun => match store
        .fetch_cont(cont)
        .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
      {
        Continuation::Call { unevaled_arg, saved_env, continuation } => {
          let function = result;
          let next_expr = unevaled_arg;
          let newer_cont =
            store.intern_cont_call2(*function, saved_env, continuation);
          Control::Return(next_expr, *env, newer_cont)
        },
        _ => unreachable!(),
      },
      _ => {
        // Bad function
        Control::Return(*result, *env, store.intern_cont_error())
      },
    },
    ContTag::Call2 => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Call2 { function, saved_env, continuation } => {
        match function.tag() {
          Tag::Fun => match store
            .fetch(&function)
            .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
          {
            Expression::Fun(arg, body, closed_env) => {
              if arg == store.lurk_sym("_") {
                return Ok(Control::Return(
                  *result,
                  *env,
                  store.intern_cont_error(),
                ));
              }
              let (body_form, _) =
                hash_witness.car_cdr_named(ConsName::FunBody, store, &body);
              let newer_env = hash_witness.extend_named(
                ConsName::ClosedEnv,
                closed_env,
                arg,
                *result,
                store,
              );
              let cont = make_tail_continuation(saved_env, continuation, store);
              Control::Return(body_form, newer_env, cont)
            },
            _ => unreachable!(),
          },
          _ => {
            // Call2 continuation contains a non-function
            Control::Return(*result, *env, store.intern_cont_error())
          },
        }
      },
      _ => unreachable!(),
    },
    ContTag::Let => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Let { var, body, saved_env, continuation } => {
        let extended_env =
          hash_witness.extend_named(ConsName::Env, *env, var, *result, store);
        let c = make_tail_continuation(saved_env, continuation, store);

        Control::Return(body, extended_env, c)
      },
      _ => unreachable!(),
    },
    ContTag::LetRec => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::LetRec { var, body, saved_env, continuation } => {
        let extended_env = extend_rec(*env, var, *result, store, witness);

        let c = make_tail_continuation(saved_env, continuation, store);

        Control::Return(body, extended_env?, c)
      },
      _ => unreachable!(),
    },
    ContTag::Unop => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Unop { operator, continuation } => {
        let val = match operator {
          Op1::Car => {
            match hash_witness.car_cdr_mut_named(
              ConsName::UnopConsLike,
              store,
              result,
            ) {
              Ok((car, _)) => car,
              // TODO: Replace with ControlError or StoreError
              Err(_) => {
                return Ok(Control::Return(
                  *result,
                  *env,
                  store.intern_cont_error(),
                ))
              }, // Err(_) => store.nil(),
            }
          },
          Op1::Cdr => {
            match hash_witness.car_cdr_mut_named(
              ConsName::UnopConsLike,
              store,
              result,
            ) {
              Ok((_, cdr)) => cdr,
              Err(_) => {
                return Ok(Control::Return(
                  *result,
                  *env,
                  store.intern_cont_error(),
                ))
              },
            }
          },
          Op1::Atom => match result.tag() {
            Tag::Cons => store.nil(),
            _ => store.t(),
          },
          Op1::Emit => {
            println!("{}", result.fmt_to_string(store));
            return Ok(Control::MakeThunk(
              *result,
              *env,
              store.intern_cont_emit(continuation),
            ));
          },
          Op1::Open => match result.tag() {
            Tag::Num | Tag::Comm => store.open_mut(*result)?.1,
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ))
            },
          },
          Op1::Secret => match result.tag() {
            Tag::Num | Tag::Comm => store.secret_mut(*result)?,
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ))
            },
          },
          Op1::Commit => store.hide(F::zero(), *result),
          Op1::Num => match result.tag() {
            Tag::Num | Tag::Comm | Tag::Char | Tag::U64 => {
              let scalar_ptr = store
                .get_expr_hash(result)
                .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;
              store.intern_num(crate::Num::Scalar::<F>(*scalar_ptr.value()))
            },
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ))
            },
          },
          Op1::U64 => match result.tag() {
            Tag::Num => {
              let scalar_ptr = store
                .get_expr_hash(result)
                .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;

              store.get_u64(scalar_ptr.value().to_u64_unchecked())
            },
            Tag::U64 => *result,
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ))
            },
          },
          Op1::Comm => match result.tag() {
            Tag::Num | Tag::Comm => {
              let scalar_ptr = store
                .get_expr_hash(result)
                .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;
              store.intern_maybe_opaque_comm(*scalar_ptr.value())
            },
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ))
            },
          },
          Op1::Char => match result.tag() {
            Tag::Num | Tag::Char => {
              let scalar_ptr = store
                .get_expr_hash(result)
                .ok_or_else(|| LurkError::Store("expr hash missing".into()))?;
              store.get_char(
                char::from_u32(scalar_ptr.value().to_u32().ok_or_else(
                  || LurkError::Eval("Ptr is invalid u32".into()),
                )?)
                .ok_or_else(|| LurkError::Eval("u32 is invalid char".into()))?,
              )
            },
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ))
            },
          },
          Op1::Eval => {
            match store
              .fetch_cont(cont)
              .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
            {
              Continuation::Unop { continuation, .. } => {
                return Ok(Control::Return(
                  *result,
                  empty_sym_env(store),
                  continuation,
                ))
              },
              _ => unreachable!(),
            }
          },
        };
        Control::MakeThunk(val, *env, continuation)
      },
      _ => unreachable!(),
    },
    ContTag::Binop => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Binop {
        operator,
        saved_env,
        unevaled_args,
        continuation,
      } => {
        let (arg2, rest) = hash_witness.car_cdr_named(
          ConsName::UnevaledArgs,
          store,
          &unevaled_args,
        );
        if operator == Op2::Begin {
          if rest.is_nil() {
            Control::Return(arg2, saved_env, continuation)
          }
          else {
            let begin = store.lurk_sym("begin");
            let begin_again = hash_witness.cons_named(
              ConsName::Begin,
              store,
              begin,
              unevaled_args,
            );
            Control::Return(begin_again, saved_env, continuation)
          }
        }
        else if !rest.is_nil() {
          Control::Return(*result, *env, store.intern_cont_error())
        }
        else {
          Control::Return(
            arg2,
            saved_env,
            store.intern_cont_binop2(operator, *result, continuation),
          )
        }
      },
      _ => unreachable!(),
    },
    ContTag::Binop2 => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Binop2 { operator, evaled_arg, continuation } => {
        let arg2 = result;

        let num_num = |store: &mut Store<F>,
                       operator,
                       a: Num<F>,
                       b: Num<F>|
         -> Result<Ptr<F>, Control<F>> {
          match operator {
            Op2::Sum => {
              let mut tmp = a;
              tmp += b;
              Ok(store.intern_num(tmp))
            },
            Op2::Diff => {
              let mut tmp = a;
              tmp -= b;
              Ok(store.intern_num(tmp))
            },
            Op2::Product => {
              let mut tmp = a;
              tmp *= b;
              Ok(store.intern_num(tmp))
            },
            Op2::Quotient => {
              let mut tmp = a;
              let b_is_zero: bool = b.is_zero();
              if b_is_zero {
                Err(Control::Return(*result, *env, store.intern_cont_error()))
              }
              else {
                tmp /= b;
                Ok(store.intern_num(tmp))
              }
            },
            Op2::Modulo => {
              // Modulo requires both args be UInt.
              Err(Control::Return(*result, *env, store.intern_cont_error()))
            },
            Op2::Equal | Op2::NumEqual => Ok(store.as_lurk_boolean(a == b)),
            Op2::Less => Ok(store.as_lurk_boolean(a < b)),
            Op2::Greater => Ok(store.as_lurk_boolean(a > b)),
            Op2::LessEqual => Ok(store.as_lurk_boolean(a <= b)),
            Op2::GreaterEqual => Ok(store.as_lurk_boolean(a >= b)),
            _ => unreachable!(),
          }
        };

        let result = match (
          store
            .fetch(&evaled_arg)
            .ok_or_else(|| LurkError::Store("Fetch failed".into()))?,
          store
            .fetch(arg2)
            .ok_or_else(|| LurkError::Store("Fetch failed".into()))?,
        ) {
          (Expression::Num(a), Expression::Num(b)) if operator.is_numeric() => {
            match num_num(store, operator, a, b) {
              Ok(x) => x,
              Err(control) => return Ok(control),
            }
          },
          (Expression::Num(a), _) if operator == Op2::Hide => {
            store.hide(a.into_scalar(), *arg2)
          },
          (Expression::UInt(a), Expression::UInt(b))
            if operator.is_numeric() =>
          {
            match operator {
              Op2::Sum => store.get_u64((a + b).into()),
              Op2::Diff => store.get_u64((a - b).into()),
              Op2::Product => store.get_u64((a * b).into()),
              Op2::Quotient => {
                if b.is_zero() {
                  return Ok(Control::Return(
                    *result,
                    *env,
                    store.intern_cont_error(),
                  ));
                }
                else {
                  store.get_u64((a / b).into())
                }
              },
              Op2::Modulo => store.get_u64((a % b).into()),
              Op2::Equal | Op2::NumEqual => store.as_lurk_boolean(a == b),
              Op2::Less => store.as_lurk_boolean(a < b),
              Op2::Greater => store.as_lurk_boolean(a > b),
              Op2::LessEqual => store.as_lurk_boolean(a <= b),
              Op2::GreaterEqual => store.as_lurk_boolean(a >= b),
              _ => unreachable!(),
            }
          },
          (Expression::Num(a), Expression::UInt(b))
            if operator.is_numeric() =>
          {
            match num_num(store, operator, a, b.into()) {
              Ok(x) => x,
              Err(control) => return Ok(control),
            }
          },
          (Expression::UInt(a), Expression::Num(b))
            if operator.is_numeric() =>
          {
            match num_num(store, operator, a.into(), b) {
              Ok(x) => x,
              Err(control) => return Ok(control),
            }
          },
          (Expression::Char(_), Expression::Str(_))
            if matches!(operator, Op2::StrCons) =>
          {
            hash_witness.strcons_named(
              ConsName::TheCons,
              store,
              evaled_arg,
              *arg2,
            )
          },
          _ => match operator {
            Op2::Equal => {
              store.as_lurk_boolean(store.ptr_eq(&evaled_arg, arg2)?)
            },
            Op2::Cons => hash_witness.cons_named(
              ConsName::TheCons,
              store,
              evaled_arg,
              *arg2,
            ),
            Op2::Eval => {
              return Ok(Control::Return(evaled_arg, *arg2, continuation));
            },
            _ => {
              return Ok(Control::Return(
                *result,
                *env,
                store.intern_cont_error(),
              ));
            },
          },
        };
        Control::MakeThunk(result, *env, continuation)
      },
      _ => unreachable!(),
    },
    ContTag::If => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::If { unevaled_args, continuation } => {
        let condition = result;
        let (arg1, more) = hash_witness.car_cdr_named(
          ConsName::UnevaledArgs,
          store,
          &unevaled_args,
        );

        // NOTE: as formulated here, IF operates on any condition. Every
        // value but NIL is considered true.
        //
        // We can implement this in constraints:
        // X * (1-X) = 0
        // C * X = 0
        // (X + C) * Q = 1
        //
        // where X is a constrained boolean which is true (1) iff C == 0. Q
        // is a value non-deterministically supplied by the prover to
        // demonstrate that both X and C are not 0. If both were 0, the
        // constraint C * X = 0 would hold. But in that case, X should be 1
        // since C = 0.
        //
        // Now X can be used as the known-boolean conditional in a
        // conditional selection: (B - A) * X = B - C
        //
        // where C is the result, A is the 'true' result, and B is the
        // false/else result. i.e. if X then A else B.
        //
        // All of the above is just 'how to implement an exact equality
        // check' in the case that the value checked against is zero. We
        // will need this throughout, when branching on symbols (effectively
        // a CASE expression). Since symbols are field elements with
        // equality, this is relatively efficient. When doing this, the
        // value being checked against is not zero, so that value should
        // first be subtracted from the value being checked.

        let (arg2, end) =
          hash_witness.car_cdr_named(ConsName::UnevaledArgsCdr, store, &more);
        if !end.is_nil() {
          Control::Return(arg1, *env, store.intern_cont_error())
        }
        else if condition.is_nil() {
          Control::Return(arg2, *env, continuation)
        }
        else {
          Control::Return(arg1, *env, continuation)
        }
      },
      _ => unreachable!(),
    },
    ContTag::Lookup => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Lookup { saved_env, continuation } => {
        Control::MakeThunk(*result, saved_env, continuation)
      },
      _ => unreachable!(),
    },
    ContTag::Tail => match store
      .fetch_cont(cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Tail { saved_env, continuation } => {
        Control::MakeThunk(*result, saved_env, continuation)
      },
      _ => {
        unreachable!();
      },
    },
  };

  if control.is_apply_continuation() {
    unreachable!();
  }

  Ok(control)
}

// Returns (Expression::Thunk, Expression::Env, Continuation)
fn make_thunk<F: LurkField>(
  control: Control<F>,
  store: &mut Store<F>,
  _witness: &mut Witness<F>,
) -> Result<Control<F>, LurkError> {
  if !control.is_make_thunk() {
    return Ok(control);
  }

  let (result, env, cont) = control.into_results();

  if let Tag::Thunk = result.tag() {
    unreachable!("make_thunk should never be called with a thunk");
  };

  match cont.tag() {
    ContTag::Tail => match store
      .fetch_cont(&cont)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Continuation::Tail { saved_env, continuation } => {
        let thunk = store.intern_thunk(Thunk { value: result, continuation });
        Ok(Control::Return(thunk, saved_env, store.intern_cont_dummy()))
      },
      _ => unreachable!(),
    },
    // If continuation is outermost, we don't actually make a thunk. Instead, we
    // signal that this is the terminal result by returning a Terminal
    // continuation.
    ContTag::Outermost => {
      Ok(Control::Return(result, env, store.intern_cont_terminal()))
    },
    _ => {
      let thunk =
        store.intern_thunk(Thunk { value: result, continuation: cont });
      Ok(Control::Return(thunk, env, store.intern_cont_dummy()))
    },
  }
}

fn make_tail_continuation<F: LurkField>(
  env: Ptr<F>,
  continuation: ContPtr<F>,
  store: &mut Store<F>,
) -> ContPtr<F> {
  // Result must be either a Tail or Outermost continuation.
  match continuation.tag() {
    // If continuation is already tail, just return it.
    ContTag::Tail => continuation,
    // Otherwise, package it along with supplied env as a new Tail continuation.
    _ => store.intern_cont_tail(env, continuation),
  }
  // Since this is the only place Tail continuation are created, this ensures
  // Tail continuations never point to one another: they can only be nested
  // one deep.
}

pub struct Evaluator<'a, F: LurkField> {
  expr: Ptr<F>,
  env: Ptr<F>,
  store: &'a mut Store<F>,
  limit: usize,
  terminal_frame: Option<Frame<IO<F>, Witness<F>>>,
}

impl<'a, F: LurkField> Evaluator<'a, F>
where IO<F>: Copy
{
  pub fn new(
    expr: Ptr<F>,
    env: Ptr<F>,
    store: &'a mut Store<F>,
    limit: usize,
  ) -> Self {
    Evaluator { expr, env, store, limit, terminal_frame: None }
  }

  pub fn eval(&mut self) -> Result<(IO<F>, usize, Vec<Ptr<F>>), LurkError> {
    let initial_input = self.initial();
    let frame_iterator = FrameIt::new(initial_input, self.store)?;

    // Initial input performs one reduction, so we need limit - 1 more.
    let (ultimate_frame, _penultimate_frame, emitted) =
      frame_iterator.next_n(self.limit - 1)?;
    let output = ultimate_frame.output;

    let was_terminal = ultimate_frame.is_complete();
    let i = ultimate_frame.i;
    if was_terminal {
      self.terminal_frame = Some(ultimate_frame);
    }
    let iterations = if was_terminal { i } else { i + 1 };
    // NOTE: We compute a terminal frame but don't include it in the iteration
    // count.
    Ok((output, iterations, emitted))
  }

  pub fn initial(&mut self) -> IO<F> {
    IO {
      expr: self.expr,
      env: self.env,
      cont: self.store.intern_cont_outermost(),
    }
  }

  pub fn iter(
    &mut self,
  ) -> Result<Take<FrameIt<'_, Witness<F>, F>>, LurkError> {
    let initial_input = self.initial();

    Ok(FrameIt::new(initial_input, self.store)?.take(self.limit))
  }

  // Wraps frames in Result type in order to fail gracefully
  pub fn get_frames(
    &mut self,
  ) -> Result<Vec<Frame<IO<F>, Witness<F>>>, LurkError> {
    let frame = FrameIt::new(self.initial(), self.store)?;
    let result_frame = ResultFrame(Ok(frame)).into_iter().take(self.limit);
    let ret: Result<Vec<_>, _> = result_frame.collect();
    ret
  }

  pub fn generate_frames<Fp: Fn(usize) -> bool>(
    expr: Ptr<F>,
    env: Ptr<F>,
    store: &'a mut Store<F>,
    limit: usize,
    needs_frame_padding: Fp,
  ) -> Result<Vec<Frame<IO<F>, Witness<F>>>, LurkError> {
    let mut evaluator = Self::new(expr, env, store, limit);

    let mut frames = evaluator.get_frames()?;
    assert!(!frames.is_empty());

    // TODO: We previously had an optimization here. If the limit was not
    // reached, the final frame should be an identity reduction suitable for
    // padding. If it's not needed for that purpose, we can pop it from frames.
    // In the worst case, this could save creating one multi-frame filled
    // only with this identity padding. However, knowing when it is safe to
    // do that is complicated, because for Groth16/SnarkPack+, we may need to
    // pad the total number of proofs to a power of two. For now, we omit
    // the optimization. With more thought and care, we could add it back
    // later.

    if !frames.is_empty() {
      let padding_frame = frames[frames.len() - 1].clone();
      while needs_frame_padding(frames.len()) {
        frames.push(padding_frame.clone());
      }
    }

    Ok(frames)
  }
}

pub fn empty_sym_env<F: LurkField>(store: &Store<F>) -> Ptr<F> {
  store.get_nil()
}

// Only used in tests. Real evalution should use extend_name.
#[allow(dead_code)]
fn extend<F: LurkField>(
  env: Ptr<F>,
  var: Ptr<F>,
  val: Ptr<F>,
  store: &mut Store<F>,
) -> Ptr<F> {
  let cons = store.cons(var, val);
  store.cons(cons, env)
}

fn extend_rec<F: LurkField>(
  env: Ptr<F>,
  var: Ptr<F>,
  val: Ptr<F>,
  store: &mut Store<F>,
  witness: &mut Witness<F>,
) -> Result<Ptr<F>, LurkError> {
  let hash_witness = &mut witness.hashes;

  let (binding_or_env, rest) =
    hash_witness.car_cdr_named(ConsName::Env, store, &env);
  let (var_or_binding, _val_or_more_bindings) =
    hash_witness.car_cdr_named(ConsName::EnvCar, store, &binding_or_env);
  match var_or_binding.tag() {
    // It's a var, so we are extending a simple env with a recursive env.
    Tag::Sym | Tag::Nil => {
      let cons = hash_witness.cons_named(ConsName::NewRecCadr, store, var, val);
      let nil = store.nil();
      let list = hash_witness.cons_named(ConsName::NewRec, store, cons, nil);
      let res =
        hash_witness.cons_named(ConsName::ExtendedRec, store, list, env);

      Ok(res)
    },
    // It's a binding, so we are extending a recursive env.
    Tag::Cons => {
      let cons = hash_witness.cons_named(ConsName::NewRecCadr, store, var, val);
      let cons2 =
        hash_witness.cons_named(ConsName::NewRec, store, cons, binding_or_env);
      let res =
        hash_witness.cons_named(ConsName::ExtendedRec, store, cons2, rest);

      Ok(res)
    },
    _ => Err(LurkError::Store("Bad input form.".into())),
  }
}

fn extend_closure<F: LurkField>(
  fun: &Ptr<F>,
  rec_env: &Ptr<F>,
  store: &mut Store<F>,
  hash_witness: &mut HashWitness<F>,
) -> Result<Ptr<F>, LurkError> {
  match fun.tag() {
    Tag::Fun => match store
      .fetch(fun)
      .ok_or_else(|| LurkError::Store("Fetch failed".into()))?
    {
      Expression::Fun(arg, body, closed_env) => {
        let extended = hash_witness.cons_named(
          ConsName::ExtendedClosureEnv,
          store,
          *rec_env,
          closed_env,
        );
        Ok(store.intern_fun(arg, body, extended))
      },
      _ => unreachable!(),
    },
    _ => Err(LurkError::Eval(format!(
      "extend_closure received non-Fun: {:?}",
      fun
    ))),
  }
}

impl<F: LurkField> Store<F> {
  fn as_lurk_boolean(&mut self, x: bool) -> Ptr<F> {
    if x {
      self.t()
    }
    else {
      self.nil()
    }
  }
}

#[allow(dead_code)]
// This clarifies the lookup logic and is used in tests.
fn lookup<F: LurkField>(
  env: &Ptr<F>,
  var: &Ptr<F>,
  store: &Store<F>,
) -> Result<Ptr<F>, LurkError> {
  assert!(matches!(var.tag(), Tag::Sym));
  match env.tag() {
    Tag::Nil => Ok(store.get_nil()),
    Tag::Cons => {
      let (binding, smaller_env) = store.car_cdr(env);
      let (v, val) = store.car_cdr(&binding);
      if v == *var {
        Ok(val)
      }
      else {
        lookup(&smaller_env, var, store)
      }
    },
    _ => Err(LurkError::Store("Env must be a list.".into())),
  }
}

// Convenience functions, mostly for use in tests.

pub fn eval_to_ptr<F: LurkField>(
  s: &mut Store<F>,
  src: &str,
) -> Result<Ptr<F>, LurkError> {
  let expr = s.read(src).unwrap();
  let limit = 1000000;
  Ok(Evaluator::new(expr, empty_sym_env(s), s, limit).eval()?.0.expr)
}

#[cfg(test)]
mod test {
  use blstrs::Scalar as Fr;

  use super::*;
  use crate::writer::Write;

  fn test_aux(
    s: &mut Store<Fr>,
    expr: &str,
    expected_result: Option<Ptr<Fr>>,
    expected_env: Option<Ptr<Fr>>,
    expected_cont: Option<ContPtr<Fr>>,
    expected_emitted: Option<Vec<Ptr<Fr>>>,
    expected_iterations: usize,
  ) {
    let ptr = s.read(expr).unwrap();

    test_aux2(
      s,
      &ptr,
      expected_result,
      expected_env,
      expected_cont,
      expected_emitted,
      expected_iterations,
    )
  }

  fn test_aux2(
    s: &mut Store<Fr>,
    expr: &Ptr<Fr>,
    expected_result: Option<Ptr<Fr>>,
    expected_env: Option<Ptr<Fr>>,
    expected_cont: Option<ContPtr<Fr>>,
    expected_emitted: Option<Vec<Ptr<Fr>>>,
    expected_iterations: usize,
  ) {
    let limit = 100000;
    let env = empty_sym_env(&s);
    let (
      IO { expr: new_expr, env: new_env, cont: new_cont },
      iterations,
      emitted,
    ) = Evaluator::new(*expr, env, s, limit).eval().unwrap();

    if let Some(expected_result) = expected_result {
      assert!(s.ptr_eq(&expected_result, &new_expr).unwrap());
    }
    if let Some(expected_env) = expected_env {
      assert!(s.ptr_eq(&expected_env, &new_env).unwrap());
    }
    if let Some(expected_cont) = expected_cont {
      assert_eq!(expected_cont, new_cont);
    }
    else {
      assert_eq!(s.intern_cont_terminal(), new_cont);
    }
    if let Some(expected_emitted) = expected_emitted {
      assert_eq!(expected_emitted.len(), emitted.len());

      assert!(expected_emitted
        .iter()
        .zip(emitted)
        .all(|(a, b)| s.ptr_eq(a, &b).unwrap()));
    }
    assert_eq!(expected_iterations, iterations);
  }

  #[test]
  fn test_lookup() {
    let mut store = Store::<Fr>::default();
    let env = empty_sym_env(&store);
    let var = store.sym("variable");
    let val = store.num(123);

    assert!(lookup(&env, &var, &store).unwrap().is_nil());

    let new_env = extend(env, var, val, &mut store);
    assert_eq!(val, lookup(&new_env, &var, &store).unwrap());
  }

  #[test]
  fn test_reduce_simple() {
    let mut store = Store::<Fr>::default();

    {
      let num = store.num(123);
      let (result, _new_env, _cont, _witness) = reduce(
        num,
        empty_sym_env(&store),
        store.intern_cont_outermost(),
        &mut store,
      )
      .unwrap();
      assert_eq!(num, result);
    }

    {
      let (result, _new_env, _cont, _witness) = reduce(
        store.nil(),
        empty_sym_env(&store),
        store.intern_cont_outermost(),
        &mut store,
      )
      .unwrap();
      assert!(result.is_nil());
    }
  }

  #[test]
  fn evaluate_simple() {
    let s = &mut Store::<Fr>::default();

    let expr = "999";
    let expected = s.num(999);
    test_aux(s, expr, Some(expected), None, None, None, 1);
  }

  #[test]
  fn evaluate_lookup() {
    let mut store = Store::<Fr>::default();

    let limit = 20;
    let val = store.num(999);
    let var = store.sym("apple");
    let val2 = store.num(888);
    let var2 = store.sym("banana");
    let env = extend(empty_sym_env(&store), var, val, &mut store);

    {
      let (
        IO { expr: result_expr, env: _env, cont: _cont },
        iterations,
        _emitted,
      ) = Evaluator::new(var, env, &mut store, limit).eval().unwrap();

      assert_eq!(1, iterations);
      assert_eq!(&result_expr, &val);
    }
    {
      let env2 = extend(env, var2, val2, &mut store);
      let (
        IO { expr: result_expr, env: _env, cont: _cont },
        iterations,
        _emitted,
      ) = Evaluator::new(var, env2, &mut store, limit).eval().unwrap();

      assert_eq!(2, iterations);
      assert_eq!(&result_expr, &val);
    }
  }

  #[test]
  fn print_expr() {
    let mut s = Store::<Fr>::default();
    let nil = s.nil();
    let x = s.sym("x");
    let lambda = s.lurk_sym("lambda");
    let val = s.num(123);
    let lambda_args = s.cons(x, nil);
    let body = s.cons(x, nil);
    let rest = s.cons(lambda_args, body);
    let whole_lambda = s.cons(lambda, rest);
    let lambda_arguments = s.cons(val, nil);
    let expr = s.cons(whole_lambda, lambda_arguments);
    let output = expr.fmt_to_string(&s);

    assert_eq!("((LAMBDA (X) X) 123)".to_string(), output);
  }

  #[test]
  fn evaluate_cons() {
    let s = &mut Store::<Fr>::default();
    let expr = "(cons 1 2)";

    let car = s.num(1);
    let cdr = s.num(2);
    let expected = s.cons(car, cdr);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
  }

  #[test]
  fn emit_output() {
    let s = &mut Store::<Fr>::default();
    let expr = "(emit 123)";

    let expected = s.num(123);
    let emitted = vec![expected];
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), Some(emitted), 3);
  }

  #[test]
  fn evaluate_lambda() {
    let s = &mut Store::<Fr>::default();
    let expr = "((lambda(x) x) 123)";

    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 4);
  }

  #[test]
  fn evaluate_lambda2() {
    let s = &mut Store::<Fr>::default();
    let expr = "((lambda (y) ((lambda (x) y) 321)) 123)";

    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 9);
  }

  #[test]
  fn evaluate_lambda3() {
    let s = &mut Store::<Fr>::default();
    let expr = "((lambda (y) ((lambda (x) ((lambda (z) z) x)) y)) 123)";

    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 10);
  }

  #[test]
  fn evaluate_lambda4() {
    let s = &mut Store::<Fr>::default();
    let expr =
        // NOTE: We pass two different values. This tests which is returned.
            "((lambda (y) ((lambda (x) ((lambda (z) z) x)) 888)) 999)";

    let expected = s.num(888);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 10);
  }

  #[test]
  fn evaluate_lambda5() {
    let s = &mut Store::<Fr>::default();
    let expr =
        // Bind a function to the name FN, then call it.
            "(((lambda (fn) (lambda (x) (fn x))) (lambda (y) y)) 999)";

    let expected = s.num(999);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 13);
  }

  #[test]
  fn evaluate_sum() {
    let s = &mut Store::<Fr>::default();
    let expr = "(+ 2 (+ 3 4))";

    let expected = s.num(9);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 6);
  }

  #[test]
  fn evaluate_diff() {
    let s = &mut Store::<Fr>::default();
    let expr = "(- 9 5)";

    let expected = s.num(4);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
  }

  #[test]
  fn evaluate_product() {
    let s = &mut Store::<Fr>::default();
    let expr = "(* 9 5)";

    let expected = s.num(45);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
  }

  #[test]
  fn evaluate_quotient() {
    let s = &mut Store::<Fr>::default();
    let expr = "(/ 21 7)";

    let expected = s.num(3);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
  }

  #[test]
  fn evaluate_quotient_divide_by_zero() {
    let s = &mut Store::<Fr>::default();
    let expr = "(/ 21 0)";

    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 3);
  }

  #[test]
  fn evaluate_num_equal() {
    let s = &mut Store::<Fr>::default();

    {
      let expr = "(= 5 5)";

      // TODO: Consider special-casing T, like NIL, and force it to the
      // immediate value 1 (with Symbol type-tag). That way boolean logic
      // will work out. It might be more consistent to have an explicit
      // boolean type (like Scheme), though. Otherwise we will have to
      // think about handling of symbol names (if made explicit), since
      // neither T/NIL as 1/0 will *not* be hashes of their symbol names.
      let expected = s.t();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
    {
      let expr = "(= 5 6)";

      let expected = s.nil();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
  }

  #[test]
  fn evaluate_adder1() {
    let s = &mut Store::<Fr>::default();
    let expr = "(((lambda (x) (lambda (y) (+ x y))) 2) 3)";

    let expected = s.num(5);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 13);
  }

  // Enable this when we have LET.
  #[test]
  fn evaluate_adder2() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((make-adder (lambda (x) (lambda (y) (+ x y)))))
                   ((make-adder 2) 3))";

    let expected = s.num(5);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 15);
  }

  #[test]
  fn evaluate_let_simple() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((a 1)) a)";

    let expected = s.num(1);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
  }

  #[test]
  fn evaluate_empty_let_bug() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let () (+ 1 2))";

    let expected = s.num(3);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 4);
  }

  #[test]
  fn evaluate_let() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((a 1)
                        (b 2))
                   (+ a b))";

    let expected = s.num(3);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 10);
  }

  #[test]
  fn evaluate_let_empty_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let)";

    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn evaluate_let_empty_body_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((a 1)))";

    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn evaluate_letrec_empty_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec)";

    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn evaluate_letrec_empty_body_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((a 1)))";

    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn evaluate_letrec_body_nil() {
    let s = &mut Store::<Fr>::default();
    let expr = "(eq nil (let () nil))";

    let expected = s.t();
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 4);
  }

  #[test]
  fn evaluate_let_parallel_binding() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((a 1) (b a)) b)";

    let expected = s.num(1);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 5)
  }

  #[test]
  fn evaluate_arithmetic_let() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((a 5)
                        (b 1)
                        (c 2))
                   (/ (+ a b) c))";

    let expected = s.num(3);
    let new_env = s.nil();
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), Some(new_env), Some(terminal), None, 18);
  }

  #[test]
  // Not because it's efficient, but to prove we can.
  fn evaluate_fundamental_conditional() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(let ((true (lambda (a)
                                    (lambda (b)
                                      a)))
                            (false (lambda (a)
                                     (lambda (b)
                                      b)))
                            (iff (lambda (a)
                                   (lambda (b)
                                     (lambda (cond)
                                       ((cond a) b))))))
                       (((iff 5) 6) true))";

      let expected = s.num(5);
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 35);
    }
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(let ((true (lambda (a)
                                    (lambda (b)
                                   a)))
                            (false (lambda (a)
                                  (lambda (b)
                                   b)))
                            (iff (lambda (a)
                                   (lambda (b)
                                     (lambda (cond)
                                       ((cond a) b))))))
                       (((iff 5) 6) false))";

      let expected = s.num(6);
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 32);
    }
  }

  #[test]
  fn evaluate_if() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(if t 5 6)";

      let expected = s.num(5);
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(if nil 5 6)";

      let expected = s.num(6);
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
  }

  #[test]
  fn evaluate_fully_evaluates() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(if t (+ 5 5) 6)";

      let expected = s.num(10);
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 5);
    }
  }

  #[test]
  fn evaluate_recursion1() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((exp (lambda (base)
                                  (lambda (exponent)
                                    (if (= 0 exponent)
                                        1
                                        (* base ((exp base) (- exponent \
                1))))))))
                   ((exp 5) 3))";

    let expected = s.num(125);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 91);
  }

  #[test]
  fn evaluate_recursion2() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((exp (lambda (base)
                                  (lambda (exponent)
                                     (lambda (acc)
                                       (if (= 0 exponent)
                                          acc
                                          (((exp base) (- exponent 1)) (* acc \
                base))))))))
                   (((exp 5) 5) 1))";

    let expected = s.num(3125);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 201);
  }

  #[test]
  fn evaluate_recursion_multiarg() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((exp (lambda (base exponent)
                                  (if (= 0 exponent)
                                      1
                                      (* base (exp base (- exponent 1)))))))
                          (exp 5 3))";

    let expected = s.num(125);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 95);
  }

  #[test]
  fn evaluate_recursion_optimized() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((exp (lambda (base)
                               (letrec ((base-inner
                                          (lambda (exponent)
                                            (if (= 0 exponent)
                                                1
                                                (* base (base-inner (- \
                exponent 1)))))))
                                        base-inner))))
                    ((exp 5) 3))";

    let expected = s.num(125);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 75);
  }

  #[test]
  fn evaluate_tail_recursion() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((exp (lambda (base)
                                  (lambda (exponent-remaining)
                                    (lambda (acc)
                                      (if (= 0 exponent-remaining)
                                          acc
                                          (((exp base) (- exponent-remaining \
                1)) (* acc base))))))))
                          (((exp 5) 3) 1))";

    let expected = s.num(125);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 129);
  }

  #[test]
  fn evaluate_tail_recursion_somewhat_optimized() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((exp (lambda (base)
                             (letrec ((base-inner
                                        (lambda (exponent-remaining)
                                          (lambda (acc)
                                            (if (= 0 exponent-remaining)
                                              acc
                                             ((base-inner (- \
                exponent-remaining 1)) (* acc base)))))))
                                      base-inner))))
                   (((exp 5) 3) 1))";

    let expected = s.num(125);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 110);
  }

  #[test]
  fn evaluate_multiple_letrec_bindings() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((double (lambda (x) (* 2 x)))
                           (square (lambda (x) (* x x))))
                   (+ (square 3) (double 2)))";

    let expected = s.num(13);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 22);
  }

  #[test]
  fn evaluate_multiple_letrec_bindings_referencing() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((double (lambda (x) (* 2 x)))
                           (double-inc (lambda (x) (+ 1 (double x)))))
                   (+ (double 3) (double-inc 2)))";

    let expected = s.num(11);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 31);
  }

  #[test]
  fn evaluate_multiple_letrec_bindings_recursive() {
    let s = &mut Store::<Fr>::default();
    let expr = "(letrec ((exp (lambda (base exponent)
                                  (if (= 0 exponent)
                                      1
                                      (* base (exp base (- exponent 1))))))
                           (exp2 (lambda (base exponent)
                                  (if (= 0 exponent)
                                      1
                                      (* base (exp2 base (- exponent 1))))))
                           (exp3 (lambda (base exponent)
                                  (if (= 0 exponent)
                                      1
                                      (* base (exp3 base (- exponent 1)))))))
                   (+ (+ (exp 3 2) (exp2 2 3))
                      (exp3 4 2)))";

    let expected = s.num(33);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 242);
  }

  #[test]
  fn nested_let_closure_regression() {
    let s = &mut Store::<Fr>::default();
    let terminal = s.get_cont_terminal();
    let expected = s.num(6);

    {
      // This always works.
      let expr = "(let ((x 6)
                              (data-function (lambda () 123))
                              (data (data-function)))
                          x)";
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 13);
    }
    {
      // This fails if zero-arg functions don't save and restore the env.
      let expr = "(let ((data-function (lambda () 123))
                              (x 6)
                              (data (data-function)))
                          x)";
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 14);
    }
  }

  #[test]
  fn evaluate_eq() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(eq 'a 'a)";

      let expected = s.t();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(eq 1 1)";

      let expected = s.t();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(eq 'a 1)";

      let expected = s.nil();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }

    {
      let s = &mut Store::<Fr>::default();
      let expr = "(eq 1 'a)";

      let expected = s.nil();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
  }
  #[test]
  fn evaluate_zero_arg_lambda() {
    let s = &mut Store::<Fr>::default();
    let terminal = s.get_cont_terminal();
    {
      let expr = "((lambda () 123))";

      let expected = s.num(123);
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
    {
      let expected = {
        let arg = s.sym("x");
        let num = s.num(123);
        let body = s.list(&[num]);
        let env = s.nil();
        s.intern_fun(arg, body, env)
      };

      // One arg expected but zero supplied.
      let expr = "((lambda (x) 123))";
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
    }
    {
      let expr = "(letrec ((x 9) (f (lambda () (+ x 1)))) (f))";

      let expected = s.num(10);
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 12);
    }
  }

  #[test]
  fn evaluate_zero_arg_lambda_variants() {
    {
      let mut s = Store::<Fr>::default();
      let limit = 20;
      let expr = s.read("((lambda (x) 123))").unwrap();

      let (
        IO { expr: result_expr, env: _new_env, cont: _continuation },
        iterations,
        _emitted,
      ) =
        Evaluator::new(expr, empty_sym_env(&s), &mut s, limit).eval().unwrap();

      assert_eq!(crate::store::Tag::Fun, result_expr.tag());
      assert_eq!(3, iterations);
    }
    {
      let s = &mut Store::<Fr>::default();
      let expr = "((lambda () 123) 1)";

      let error = s.get_cont_error();
      test_aux(s, expr, None, None, Some(error), None, 3);
    }
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(123)";

      let error = s.get_cont_error();
      test_aux(s, expr, None, None, Some(error), None, 2);
    }
  }

  #[test]
  fn evaluate_make_tree() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(letrec ((mapcar (lambda (f list)
                                                        (if (eq list nil)
                                                            nil
                                                            (cons (f (car \
                  list)) (mapcar f (cdr list))))))
                                    (make-row (lambda (list)
                                                (if (eq list nil)
                                                    nil
                                                    (let ((cdr (cdr list)))
                                                    (cons (cons (car list) \
                  (car cdr))
                                                            (make-row (cdr \
                  cdr)))))))
                                    (make-tree-aux (lambda (list)
                                                    (let ((row (make-row \
                  list)))
                                                    (if (eq (cdr row) nil)
                                                        row
                                                        (make-tree-aux row)))))
                                    (make-tree (lambda (list)
                                                (car (make-tree-aux list))))
                                    (reverse-tree (lambda (tree)
                                                    (if (atom tree)
                                                        tree
                                                        (cons (reverse-tree \
                  (cdr tree))
                                                            (reverse-tree (car \
                  tree)))))))
                        (reverse-tree (make-tree '(a b c d e f g h))))";

      let expected =
        s.read("(((h . g) . (f . e)) . ((d . c) . (b . a)))").unwrap();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 493);
    }
  }

  #[test]
  fn evaluate_make_tree_minimal_regression() {
    {
      let mut s = Store::<Fr>::default();
      let limit = 100;
      let expr = s
        .read(
          "(letrec ((fn-1 (lambda (x)
                                    (let ((y x))
                                       y)))
                               (fn-2 (lambda (list)
                                       (let ((z (fn-1 list)))
                                         (fn-2 z)))))
                                 (fn-2 '(a b c d e f g h)))",
        )
        .unwrap();

      let (
        IO { expr: _result_expr, env: _new_env, cont: _continuation },
        iterations,
        _emitted,
      ) =
        Evaluator::new(expr, empty_sym_env(&s), &mut s, limit).eval().unwrap();

      assert_eq!(100, iterations);
    }
  }
  #[test]
  fn evaluate_map_tree_bug() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(letrec ((map-tree (lambda (f tree)
                      (if (atom tree)
                          (f tree)
                          (cons (map-tree f (car tree))
                                (map-tree f (cdr tree)))))))
         (map-tree (lambda (x) (+ 1 x)) '((1 . 2) . (3 . 4))))";

      let expected = s.read("((2 . 3) . (4 . 5))").unwrap();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 170);
    }
  }

  #[test]
  fn evaluate_map_tree_numequal_bug() {
    {
      // Reuse map-tree failure case to test Relop behavior.
      // This failed initially and tests regression.
      let s = &mut Store::<Fr>::default();
      let expr = "(letrec ((map-tree (lambda (f tree)
                                           (if (atom tree)
                                             (f tree)
                                               (= (map-tree f (car tree))
                                                  (map-tree f (cdr tree)))))))
                       (map-tree (lambda (x) (+ 1 x)) '((1 . 2) . (3 . 4))))";
      let expected = s.nil();
      let error = s.get_cont_error();
      test_aux(s, expr, Some(expected), None, Some(error), None, 169);
    }
  }

  #[test]
  fn env_lost_bug() {
    {
      // previously, an unbound variable `u` error
      let s = &mut Store::<Fr>::default();
      let expr = "
(letrec
    (
     (id
      (lambda (x) x))
     (id2
      (lambda (x) (id x)))
     (foo
      (lambda (u)
        (if (id2 0)
            u
            0)))
     )
  (foo '()))
";
      let expected = s.nil();
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 25);
    }
  }

  #[test]
  fn dont_discard_rest_env() {
    {
      // previously: Unbound variable: Sym("Z")
      let s = &mut Store::<Fr>::default();
      let expr = "(let ((z 9))
                       (letrec ((a 1)
                                 (b 2)
                                 (l (lambda (x) (+ z x))))
                         (l 9)))";
      let expected = s.num(18);
      let terminal = s.get_cont_terminal();
      test_aux(s, expr, Some(expected), None, Some(terminal), None, 22);
    }
  }

  #[test]
  fn test_str_car_cdr_cons() {
    let s = &mut Store::<Fr>::default();
    let a = s.read(r#"#\a"#).unwrap();
    let apple = s.read(r#" "apple" "#).unwrap();
    let a_pple = s.read(r#" (#\a . "pple") "#).unwrap();
    let pple = s.read(r#" "pple" "#).unwrap();
    let empty = s.intern_str(&"");
    let nil = s.nil();
    let terminal = s.get_cont_terminal();
    let error = s.get_cont_error();

    test_aux(s, r#"(car "apple")"#, Some(a), None, Some(terminal), None, 2);
    test_aux(s, r#"(cdr "apple")"#, Some(pple), None, Some(terminal), None, 2);
    test_aux(s, r#"(car "")"#, Some(nil), None, Some(terminal), None, 2);
    test_aux(s, r#"(cdr "")"#, Some(empty), None, Some(terminal), None, 2);
    test_aux(
      s,
      r#"(cons #\a "pple")"#,
      Some(a_pple),
      None,
      Some(terminal),
      None,
      3,
    );
    test_aux(
      s,
      r#"(strcons #\a "pple")"#,
      Some(apple),
      None,
      Some(terminal),
      None,
      3,
    );
    test_aux(s, r#"(strcons #\a #\b)"#, None, None, Some(error), None, 3);
    test_aux(s, r#"(strcons "a" "b")"#, None, None, Some(error), None, 3);
    test_aux(s, r#"(strcons 1 2)"#, None, None, Some(error), None, 3);
  }

  #[test]
  fn test_one_arg_cons_error() {
    let s = &mut Store::<Fr>::default();
    let error = s.get_cont_error();
    test_aux(s, r#"(cons "")"#, None, None, Some(error), None, 1);
  }

  #[test]
  fn test_car_nil() {
    let s = &mut Store::<Fr>::default();
    let expected = s.nil();
    let terminal = s.get_cont_terminal();
    test_aux(s, r#"(car NIL)"#, Some(expected), None, Some(terminal), None, 2);
  }

  #[test]
  fn test_cdr_nil() {
    let s = &mut Store::<Fr>::default();
    let expected = s.nil();
    let terminal = s.get_cont_terminal();
    test_aux(s, r#"(cdr NIL)"#, Some(expected), None, Some(terminal), None, 2);
  }

  #[test]
  fn test_car_cdr_invalid_tag_error_sym() {
    let s = &mut Store::<Fr>::default();
    let error = s.get_cont_error();
    test_aux(s, r#"(car car)"#, None, None, Some(error), None, 2);
    test_aux(s, r#"(cdr car)"#, None, None, Some(error), None, 2);
  }

  #[test]
  fn test_car_cdr_invalid_tag_error_char() {
    let s = &mut Store::<Fr>::default();
    let error = s.get_cont_error();
    test_aux(s, r#"(car #\a)"#, None, None, Some(error), None, 2);
    test_aux(s, r#"(cdr #\a)"#, None, None, Some(error), None, 2);
  }

  #[test]
  fn test_car_cdr_invalid_tag_error_num() {
    let s = &mut Store::<Fr>::default();
    let error = s.get_cont_error();
    test_aux(s, r#"(car 42)"#, None, None, Some(error), None, 2);
    test_aux(s, r#"(cdr 42)"#, None, None, Some(error), None, 2);
  }

  #[test]
  fn test_car_cdr_invalid_tag_error_lambda() {
    let s = &mut Store::<Fr>::default();
    let error = s.get_cont_error();
    test_aux(s, r#"(car (lambda (x) x))"#, None, None, Some(error), None, 2);
    test_aux(s, r#"(cdr (lambda (x) x))"#, None, None, Some(error), None, 2);
  }

  #[test]
  fn go_translate() {
    // func Foo(a int, b int) int {
    //     x := a * b + 4
    //     for i := 0; i < b; i++ {
    //         x += a
    //         a += b * 2
    //    }
    //    return x
    // }

    let s = &mut Store::<Fr>::default();
    let n = s.num(0x1044);
    test_aux(
      s,
      r#"
(let ((foo (lambda (a b)
              (letrec ((aux (lambda (i a x)
                               (if (= i b)
                                     x
                                     (let ((x (+ x a))
                                            (a (+ a (* b 2))))
                                       (aux (+ i 1) a x))))))
                       (let ((x (+ (* a b) 4)))
                         (aux 0 a x))))))
  (foo 10 16))
"#,
      Some(n),
      None,
      None,
      None,
      1114,
    );
  }

  #[test]
  fn begin_current_env() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(begin (current-env))";
      let expected = s.nil();
      test_aux(s, expr, Some(expected), None, None, None, 2);
    }
  }

  #[test]
  fn begin_current_env1() {
    {
      let s = &mut Store::<Fr>::default();
      let expr = "(let ((a 1))
                       (begin 123 (current-env)))";
      let a = s.sym("a");
      let one = s.num(1);
      let binding = s.cons(a, one);
      let expected = s.list(&[binding]);
      test_aux(s, expr, Some(expected), None, None, None, 5);
    }
  }

  #[test]
  fn hide_open() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open (hide 123 'x))";
    let x = s.sym("x");
    test_aux(s, expr, Some(x), None, None, None, 5);
  }

  #[test]
  fn hide_opaque_open_available() {
    use crate::{
      store::ScalarPointer,
      Num,
    };

    let s = &mut Store::<Fr>::default();
    let commitment = eval_to_ptr(s, "(hide 123 'x)").unwrap();

    let c_scalar = s.hash_expr(&commitment).unwrap();
    let c = c_scalar.value();
    let comm = s.intern_maybe_opaque_comm(*c);

    assert!(!comm.is_opaque());

    let open = s.sym("open");
    let x = s.sym("x");

    {
      let expr = s.list(&[open, comm]);
      test_aux2(s, &expr, Some(x), None, None, None, 2);
    }

    {
      let secret = s.sym("secret");
      let expr = s.list(&[secret, comm]);
      let sec = s.num(123);
      test_aux2(s, &expr, Some(sec), None, None, None, 2);
    }

    {
      // We can open a commitment identified by its corresponding `Num`.
      let comm_num = s.num(Num::from_scalar(*c));
      let expr2 = s.list(&[open, comm_num]);
      test_aux2(s, &expr2, Some(x), None, None, None, 2);
    }
  }

  #[test]
  #[should_panic]
  fn hide_opaque_open_unavailable() {
    use crate::store::ScalarPointer;

    let s = &mut Store::<Fr>::default();
    let commitment = eval_to_ptr(s, "(hide 123 'x)").unwrap();

    let c_scalar = s.hash_expr(&commitment).unwrap();
    let c = c_scalar.value();

    let s2 = &mut Store::<Fr>::default();
    let comm = s2.intern_maybe_opaque_comm(*c);
    let open = s2.lurk_sym("open");
    let x = s2.sym("x");

    let expr = s2.list(&[open, comm]);

    test_aux2(s2, &expr, Some(x), None, None, None, 2);
  }

  #[test]
  fn commit_open_sym() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open (commit 'x))";
    let x = s.sym("x");
    test_aux(s, expr, Some(x), None, None, None, 4);
  }

  #[test]
  fn commitment_value() {
    let s = &mut Store::<Fr>::default();
    let expr = "(num (commit 123))";
    let x = s
      .read(
        "0x5655b8656a51cf3bb9f9c9ac7b7dd80c0e2481b039594c39f56efb1e0f81c64a",
      )
      .unwrap();
    test_aux(s, expr, Some(x), None, None, None, 4);
  }

  #[test]
  fn commit_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(commit 123 456)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn open_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open 123 456)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn secret_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret 123 456)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn num_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(num 123 456)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn comm_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(comm 123 456)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn char_error() {
    let s = &mut Store::<Fr>::default();
    let expr = "(char 123 456)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 1);
  }

  #[test]
  fn prove_commit_secret() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret (commit 123))";
    let expected = s.num(0);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 4);
  }

  #[test]
  fn num() {
    let s = &mut Store::<Fr>::default();
    let expr = "(num 123)";
    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 2);
  }

  #[test]
  fn num_char() {
    let s = &mut Store::<Fr>::default();
    let expr = r#"(num #\a)"#;
    let expected = s.num(97);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 2);
  }

  #[test]
  fn char_num() {
    let s = &mut Store::<Fr>::default();
    let expr = r#"(char 97)"#;
    let expected_a = s.read(r#"#\a"#).unwrap();
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected_a), None, Some(terminal), None, 2);
  }

  #[test]
  fn commit_num() {
    let s = &mut Store::<Fr>::default();
    let expr = "(num (commit 123))";
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, None, None, Some(terminal), None, 4);
  }

  #[test]
  fn hide_open_comm_num() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open (comm (num (hide 123 456))))";
    let expected = s.num(456);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 9);
  }

  #[test]
  fn hide_secret_comm_num() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret (comm (num (hide 123 456))))";
    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 9);
  }

  #[test]
  fn commit_open_comm_num() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open (comm (num (commit 123))))";
    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 8);
  }

  #[test]
  fn commit_secret_comm_num() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret (comm (num (commit 123))))";
    let expected = s.num(0);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 8);
  }

  #[test]
  fn commit_num_open() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open (num (commit 123)))";
    let expected = s.num(123);
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(expected), None, Some(terminal), None, 6);
  }

  #[test]
  fn num_invalid_tag() {
    let s = &mut Store::<Fr>::default();
    let expr = "(num (quote x))";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 2);
  }

  #[test]
  fn comm_invalid_tag() {
    let s = &mut Store::<Fr>::default();
    let expr = "(comm (quote x))";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 2);
  }

  #[test]
  fn char_invalid_tag() {
    let s = &mut Store::<Fr>::default();
    let expr = "(char (quote x))";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 2);
  }

  #[test]
  fn terminal_sym() {
    let s = &mut Store::<Fr>::default();
    let expr = "(quote x)";
    let x = s.sym("x");
    let terminal = s.get_cont_terminal();
    test_aux(s, expr, Some(x), None, Some(terminal), None, 1);
  }

  #[test]
  #[should_panic = "hidden value could not be opened"]
  fn open_opaque_commit() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open 123)";
    test_aux(s, expr, None, None, None, None, 2);
  }

  #[test]
  fn open_wrong_type() {
    let s = &mut Store::<Fr>::default();
    let expr = "(open 'asdf)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 2);
  }

  #[test]
  fn secret_wrong_type() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret 'asdf)";
    let error = s.get_cont_error();
    test_aux(s, expr, None, None, Some(error), None, 2);
  }

  #[test]
  #[should_panic]
  fn secret_invalid_tag() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret 123)";
    test_aux(s, expr, None, None, None, None, 2);
  }

  #[test]
  #[should_panic = "secret could not be extracted"]
  fn secret_opaque_commit() {
    let s = &mut Store::<Fr>::default();
    let expr = "(secret (comm 123))";
    test_aux(s, expr, None, None, None, None, 2);
  }

  fn relational_aux(s: &mut Store<Fr>, op: &str, a: &str, b: &str, res: bool) {
    let expr = &format!("({} {} {})", op, a, b);
    let expected = if res { s.t() } else { s.nil() };
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(expected), None, Some(terminal), None, 3);
  }

  #[test]
  fn test_relational() {
    let s = &mut Store::<Fr>::default();
    let lt = "<";
    let gt = ">";
    let lte = "<=";
    let gte = ">=";
    let zero = "0";
    let one = "1";
    let two = "2";

    let most_negative = &format!("{}", Num::<Fr>::most_negative());
    let most_positive = &format!("{}", Num::<Fr>::most_positive());
    use ff::Field;
    let neg_one = &format!("{}", Num::<Fr>::Scalar(Fr::zero() - Fr::one()));

    relational_aux(s, lt, one, two, true);
    relational_aux(s, gt, one, two, false);
    relational_aux(s, lte, one, two, true);
    relational_aux(s, gte, one, two, false);

    relational_aux(s, lt, two, one, false);
    relational_aux(s, gt, two, one, true);
    relational_aux(s, lte, two, one, false);
    relational_aux(s, gte, two, one, true);

    relational_aux(s, lt, one, one, false);
    relational_aux(s, gt, one, one, false);
    relational_aux(s, lte, one, one, true);
    relational_aux(s, gte, one, one, true);

    relational_aux(s, lt, zero, two, true);
    relational_aux(s, gt, zero, two, false);
    relational_aux(s, lte, zero, two, true);
    relational_aux(s, gte, zero, two, false);

    relational_aux(s, lt, two, zero, false);
    relational_aux(s, gt, two, zero, true);
    relational_aux(s, lte, two, zero, false);
    relational_aux(s, gte, two, zero, true);

    relational_aux(s, lt, zero, zero, false);
    relational_aux(s, gt, zero, zero, false);
    relational_aux(s, lte, zero, zero, true);
    relational_aux(s, gte, zero, zero, true);

    relational_aux(s, lt, most_negative, zero, true);
    relational_aux(s, gt, most_negative, zero, false);
    relational_aux(s, lte, most_negative, zero, true);
    relational_aux(s, gte, most_negative, zero, false);

    relational_aux(s, lt, zero, most_negative, false);
    relational_aux(s, gt, zero, most_negative, true);
    relational_aux(s, lte, zero, most_negative, false);
    relational_aux(s, gte, zero, most_negative, true);

    relational_aux(s, lt, most_negative, most_positive, true);
    relational_aux(s, gt, most_negative, most_positive, false);
    relational_aux(s, lte, most_negative, most_positive, true);
    relational_aux(s, gte, most_negative, most_positive, false);

    relational_aux(s, lt, most_positive, most_negative, false);
    relational_aux(s, gt, most_positive, most_negative, true);
    relational_aux(s, lte, most_positive, most_negative, false);
    relational_aux(s, gte, most_positive, most_negative, true);

    relational_aux(s, lt, most_negative, most_negative, false);
    relational_aux(s, gt, most_negative, most_negative, false);
    relational_aux(s, lte, most_negative, most_negative, true);
    relational_aux(s, gte, most_negative, most_negative, true);

    relational_aux(s, lt, one, most_positive, true);
    relational_aux(s, gt, one, most_positive, false);
    relational_aux(s, lte, one, most_positive, true);
    relational_aux(s, gte, one, most_positive, false);

    relational_aux(s, lt, most_positive, one, false);
    relational_aux(s, gt, most_positive, one, true);
    relational_aux(s, lte, most_positive, one, false);
    relational_aux(s, gte, most_positive, one, true);

    relational_aux(s, lt, one, most_negative, false);
    relational_aux(s, gt, one, most_negative, true);
    relational_aux(s, lte, one, most_negative, false);
    relational_aux(s, gte, one, most_negative, true);

    relational_aux(s, lt, most_negative, one, true);
    relational_aux(s, gt, most_negative, one, false);
    relational_aux(s, lte, most_negative, one, true);
    relational_aux(s, gte, most_negative, one, false);

    relational_aux(s, lt, neg_one, most_positive, true);
    relational_aux(s, gt, neg_one, most_positive, false);
    relational_aux(s, lte, neg_one, most_positive, true);
    relational_aux(s, gte, neg_one, most_positive, false);

    relational_aux(s, lt, most_positive, neg_one, false);
    relational_aux(s, gt, most_positive, neg_one, true);
    relational_aux(s, lte, most_positive, neg_one, false);
    relational_aux(s, gte, most_positive, neg_one, true);

    relational_aux(s, lt, neg_one, most_negative, false);
    relational_aux(s, gt, neg_one, most_negative, true);
    relational_aux(s, lte, neg_one, most_negative, false);
    relational_aux(s, gte, neg_one, most_negative, true);

    relational_aux(s, lt, most_negative, neg_one, true);
    relational_aux(s, gt, most_negative, neg_one, false);
    relational_aux(s, lte, most_negative, neg_one, true);
    relational_aux(s, gte, most_negative, neg_one, false);
  }

  #[test]
  fn test_relational_edge_case_identity() {
    let s = &mut Store::<Fr>::default();
    let t = s.t();
    let terminal = s.get_cont_terminal();

    // Normally, a value cannot be less than the result of incrementing it.
    // However, the most positive field element (when viewed as signed)
    // is the exception. Incrementing it yields the most negative element,
    // which is less than the most positive.
    {
      let expr = "(let ((most-positive (/ (- 0 1) 2))
                          (most-negative (+ 1 most-positive)))
                      (< most-negative most-positive))";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 19);
    }

    // Regression: comparisons with negative numbers should *not* be exceptions.
    {
      let expr = "(let ((most-positive (/ (- 0 1) 2))
                              (most-negative (+ 1 most-positive))
                              (less-negative (+ 1 most-negative)))
                      (< most-negative  less-negative)) ";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 24);
    }
  }

  #[test]
  fn test_num_syntax_implications() {
    let s = &mut Store::<Fr>::default();
    let t = s.t();
    let terminal = s.get_cont_terminal();

    {
      let expr = "(let ((most-positive -1/2)
                              (most-negative 1/2))
                          (< most-negative most-positive))";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 10);
    }

    {
      let expr = "(= (* 6 3/2) 9)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 6);
    }

    {
      let expr = "(= (* 2/3 3/2) 1)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 6);
    }

    {
      let expr = "(= (* -2/3 3/2) -1)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 6);
    }

    {
      let expr = "(= (+ 1/3 1/2) 5/6)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 6);
    }

    // Comparisons of field elements produced by fractional notation don't yield
    // the results their rational equivalents would.
    {
      // This obviously must be true, since 1/2 is the most negative Num,
      // but this violates expectations if you consider 1/2 to behave like a
      // rational.
      let expr = "(< 1/2 1/3)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 3);
    }

    {
      // This isn't a weird edge case like the above, but it's also not the
      // behavior expected if fractional notation yielded true rational
      // numbers.
      let expr = "(< 3/4 5/8)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 3);
    }
    {
      // It's not that they *can't* compare in the naively expected way, though.
      let expr = "(< 3/5 3/4)";

      test_aux(s, expr, Some(t), None, Some(terminal), None, 3);
    }
  }

  #[test]
  fn test_quoted_symbols() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((|foo bar| 9)
                          (|Foo \\| Bar| (lambda (|X|) (* x x))))
                      (|Foo \\| Bar| |foo bar|))";
    let res = s.num(81);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 13);
  }

  #[test]
  fn test_eval() {
    let s = &mut Store::<Fr>::default();
    let expr = "(* 3 (eval (cons '+ (cons 1 (cons 2 nil)))))";
    let expr2 = "(* 5 (eval '(+ 1 a) '((a . 3))))"; // two-arg eval, optional second arg is env.
    let res = s.num(9);
    let res2 = s.num(20);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 17);
    test_aux(s, expr2, Some(res2), None, Some(terminal), None, 9);
  }

  #[test]
  fn test_eval_env_regression() {
    let s = &mut Store::<Fr>::default();
    let expr = "(let ((a 1)) (eval 'a))";
    let expr2 = "(let ((a 1)) (eval 'a (current-env)))";
    let res = s.num(1);
    let error = s.get_cont_error();
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, None, None, Some(error), None, 5);
    test_aux(s, expr2, Some(res), None, Some(terminal), None, 6);
  }

  #[test]
  fn test_u64_self_evaluating() {
    let s = &mut Store::<Fr>::default();

    let expr = "123u64";
    let res = s.uint64(123);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 1);
  }

  #[test]
  fn test_u64_mul() {
    let s = &mut Store::<Fr>::default();

    let expr = "(* (u64 18446744073709551615) (u64 2))";
    let expr2 = "(* 18446744073709551615u64 2u64)";
    let expr3 = "(* (- 0u64 1u64) 2u64)";
    let res = s.uint64(18446744073709551614);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 7);
    test_aux(s, expr2, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr3, Some(res), None, Some(terminal), None, 6);
  }

  #[test]
  fn test_u64_add() {
    let s = &mut Store::<Fr>::default();

    let expr = "(+ 18446744073709551615u64 2u64)";
    let expr2 = "(+ (- 0u64 1u64) 2u64)";
    let res = s.uint64(1);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(res), None, Some(terminal), None, 6);
  }

  #[test]
  fn test_u64_sub() {
    let s = &mut Store::<Fr>::default();

    let expr = "(- 2u64 1u64)";
    let expr2 = "(- 0u64 1u64)";
    let expr3 = "(+ 1u64 (- 0u64 1u64))";
    let res = s.uint64(1);
    let res2 = s.uint64(18446744073709551615);
    let res3 = s.uint64(0);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(res2), None, Some(terminal), None, 3);
    test_aux(s, expr3, Some(res3), None, Some(terminal), None, 6);
  }

  #[test]
  fn test_u64_div() {
    let s = &mut Store::<Fr>::default();

    let expr = "(/ 100u64 2u64)";
    let res = s.uint64(50);

    let expr2 = "(/ 100u64 3u64)";
    let res2 = s.uint64(33);

    let expr3 = "(/ 100u64 0u64)";

    let terminal = s.get_cont_terminal();
    let error = s.get_cont_error();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(res2), None, Some(terminal), None, 3);
    test_aux(s, expr3, None, None, Some(error), None, 3);
  }

  #[test]
  fn test_u64_mod() {
    let s = &mut Store::<Fr>::default();

    let expr = "(% 100u64 2u64)";
    let res = s.uint64(0);

    let expr2 = "(% 100u64 3u64)";
    let res2 = s.uint64(1);

    let expr3 = "(/ 100u64 0u64)";

    let terminal = s.get_cont_terminal();
    let error = s.get_cont_error();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(res2), None, Some(terminal), None, 3);
    test_aux(s, expr3, None, None, Some(error), None, 3);
  }

  #[test]
  fn test_num_mod() {
    let s = &mut Store::<Fr>::default();

    let expr = "(% 100 3)";
    let expr2 = "(% 100 3u64)";
    let expr3 = "(% 100u64 3)";

    let error = s.get_cont_error();

    test_aux(s, expr, None, None, Some(error), None, 3);
    test_aux(s, expr2, None, None, Some(error), None, 3);
    test_aux(s, expr3, None, None, Some(error), None, 3);
  }

  #[test]
  fn test_u64_comp() {
    let s = &mut Store::<Fr>::default();

    let expr = "(< 0u64 1u64)";
    let expr2 = "(< 1u64 0u64)";
    let expr3 = "(<= 0u64 1u64)";
    let expr4 = "(<= 1u64 0u64)";

    let expr5 = "(> 0u64 1u64)";
    let expr6 = "(> 1u64 0u64)";
    let expr7 = "(>= 0u64 1u64)";
    let expr8 = "(>= 1u64 0u64)";

    let expr9 = "(<= 0u64 0u64)";
    let expr10 = "(>= 0u64 0u64)";

    let t = s.t();
    let nil = s.nil();
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(t), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(nil), None, Some(terminal), None, 3);
    test_aux(s, expr3, Some(t), None, Some(terminal), None, 3);
    test_aux(s, expr4, Some(nil), None, Some(terminal), None, 3);

    test_aux(s, expr5, Some(nil), None, Some(terminal), None, 3);
    test_aux(s, expr6, Some(t), None, Some(terminal), None, 3);
    test_aux(s, expr7, Some(nil), None, Some(terminal), None, 3);
    test_aux(s, expr8, Some(t), None, Some(terminal), None, 3);

    test_aux(s, expr9, Some(t), None, Some(terminal), None, 3);
    test_aux(s, expr10, Some(t), None, Some(terminal), None, 3);
  }

  #[test]
  fn test_u64_conversion() {
    let s = &mut Store::<Fr>::default();

    let expr = "(+ 0 1u64)";
    let expr2 = "(num 1u64)";
    let expr3 = "(+ 1 1u64)";
    let expr4 = "(u64 (+ 1 1))";
    let res = s.intern_num(1);
    let res2 = s.intern_num(2);
    let res3 = s.get_u64(2);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(res), None, Some(terminal), None, 2);
    test_aux(s, expr3, Some(res2), None, Some(terminal), None, 3);
    test_aux(s, expr4, Some(res3), None, Some(terminal), None, 5);
  }

  #[test]
  fn test_numeric_type_error() {
    let s = &mut Store::<Fr>::default();
    let error = s.get_cont_error();

    let mut test = |op| {
      let expr = &format!("({} 0 'a)", op);
      let expr2 = &format!("({} 0u64 'a)", op);

      test_aux(s, expr, None, None, Some(error), None, 3);
      test_aux(s, expr2, None, None, Some(error), None, 3);
    };

    test("+");
    test("-");
    test("*");
    test("/");
    test("%");
    test(">");
    test("<");
    test(">=");
    test("<=");
    test("=");
  }

  #[test]
  fn test_u64_num_comparison() {
    let s = &mut Store::<Fr>::default();

    let expr = "(= 1 1u64)";
    let expr2 = "(= 1 2u64)";
    let t = s.t();
    let nil = s.nil();
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(t), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(nil), None, Some(terminal), None, 3);
  }

  #[test]
  fn test_u64_num_cons() {
    let s = &mut Store::<Fr>::default();

    let expr = "(cons 1 1u64)";
    let expr2 = "(cons 1u64 1)";
    let res = s.read("(1 . 1u64)").unwrap();
    let res2 = s.read("(1u64 . 1)").unwrap();
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 3);
    test_aux(s, expr2, Some(res2), None, Some(terminal), None, 3);
  }

  #[test]
  fn test_hide_u64_secret() {
    let s = &mut Store::<Fr>::default();

    let expr = "(hide 0u64 123)";
    let error = s.get_cont_error();

    test_aux(s, expr, None, None, Some(error), None, 3);
  }

  #[test]
  fn test_keyword() {
    let s = &mut Store::<Fr>::default();

    let expr = ":asdf";
    let expr2 = "(eq :asdf :asdf)";
    let expr3 = "(eq :asdf 'asdf)";
    let res = s.key("ASDF");
    let res2 = s.get_t();
    let res3 = s.get_nil();

    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 1);
    test_aux(s, expr2, Some(res2), None, Some(terminal), None, 3);
    test_aux(s, expr3, Some(res3), None, Some(terminal), None, 3);
  }

  #[test]
  fn test_root_sym() {
    use ff::Field;

    use crate::sym::Sym;

    let s = &mut Store::<Fr>::default();

    let sym = Sym::root();
    let x = s.intern_sym(&sym);

    let scalar_ptr = &s.get_expr_hash(&x).unwrap();

    assert_eq!(&Fr::zero(), scalar_ptr.value());
    assert_eq!(&Tag::Sym.as_field::<Fr>(), scalar_ptr.tag());
  }

  #[test]
  fn test_sym_hash_values() {
    use crate::{
      store::ScalarPointer,
      sym::Sym,
    };

    let s = &mut Store::<Fr>::default();

    let sym = s.sym(".ASDF.FDSA");
    let key = s.sym(":ASDF.FDSA");
    let expr = s.read("(cons \"FDSA\" '.ASDF)").unwrap();

    let limit = 10;
    let env = empty_sym_env(&s);
    let (IO { expr: new_expr, env: _, cont: _ }, _iterations, _emitted) =
      Evaluator::new(expr, env, s, limit).eval().unwrap();

    let toplevel_sym = s.sym(".ASDF");

    let root = Sym::root();
    let root_sym = s.intern_sym(&root);

    let asdf = s.str("ASDF");
    let consed_with_root = s.cons(asdf, root_sym);

    let cons_scalar_ptr = &s.get_expr_hash(&new_expr).unwrap();
    let sym_scalar_ptr = &s.get_expr_hash(&sym).unwrap();
    let key_scalar_ptr = &s.get_expr_hash(&key).unwrap();

    let consed_with_root_scalar_ptr =
      &s.get_expr_hash(&consed_with_root).unwrap();
    let toplevel_scalar_ptr = &s.get_expr_hash(&toplevel_sym).unwrap();

    // Symbol and keyword scalar hash values are the same as
    // those of the name string consed onto the parent symbol.
    assert_eq!(cons_scalar_ptr.value(), sym_scalar_ptr.value());
    assert_eq!(cons_scalar_ptr.value(), key_scalar_ptr.value());

    // Toplevel symbols also have this property, and their parent symbol is the
    // root symbol.
    assert_eq!(
      consed_with_root_scalar_ptr.value(),
      toplevel_scalar_ptr.value()
    );

    // The tags differ though.
    assert_eq!(&Tag::Sym.as_field::<Fr>(), sym_scalar_ptr.tag());
    assert_eq!(&Tag::Key.as_field::<Fr>(), key_scalar_ptr.tag());
  }

  #[test]
  fn test_fold_cons_regression() {
    let s = &mut Store::<Fr>::default();

    let expr = "(letrec ((fold (lambda (op acc l)
                                     (if l
                                         (fold op (op acc (car l)) (cdr l))
                                         acc))))
                      (fold (lambda (x y) (+ x y)) 0 '(1 2 3)))";
    let res = s.num(6);
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, Some(res), None, Some(terminal), None, 152);
  }

  #[test]
  fn test_lambda_args_regression() {
    let s = &mut Store::<Fr>::default();

    let expr = "(cons (lambda (x y) nil) nil)";
    let terminal = s.get_cont_terminal();

    test_aux(s, expr, None, None, Some(terminal), None, 3);
  }
}
