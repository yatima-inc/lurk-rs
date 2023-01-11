use std::iter::Take;

use lurk_ff::LurkField;

use crate::{
  error::LurkError,
  eval::{
    frame::{
      Frame,
      FrameIt,
      ResultFrame,
    },
    io::IO,
    status::Status,
    witness::Witness,
  },
  expr::Expr,
  ptr::Ptr,
  store::Store,
};

pub trait Evaluable<F: LurkField, W> {
  fn reduce(&self, store: &mut Store<F>) -> Result<(Self, W), LurkError<F>>
  where Self: Sized;

  fn status(&self) -> Status;
  fn is_complete(&self) -> bool;
  fn is_terminal(&self) -> bool;
  fn is_error(&self) -> bool;

  fn log(&self, store: &Store<F>, i: usize);
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

  pub fn eval(&mut self) -> Result<(IO<F>, usize, Vec<Ptr<F>>), LurkError<F>> {
    let initial_input = self.initial()?;
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

  pub fn initial(&mut self) -> Result<IO<F>, LurkError<F>> {
    Ok(IO {
      expr: self.expr,
      env: self.env,
      cont: self.store.intern_expr(Expr::Outermost)?,
    })
  }

  pub fn iter(
    &mut self,
  ) -> Result<Take<FrameIt<'_, Witness<F>, F>>, LurkError<F>> {
    let initial_input = self.initial()?;

    Ok(FrameIt::new(initial_input, self.store)?.take(self.limit))
  }

  // Wraps frames in Result type in order to fail gracefully
  pub fn get_frames(
    &mut self,
  ) -> Result<Vec<Frame<IO<F>, Witness<F>>>, LurkError<F>> {
    let frame = FrameIt::new(self.initial()?, self.store)?;
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
  ) -> Result<Vec<Frame<IO<F>, Witness<F>>>, LurkError<F>> {
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
