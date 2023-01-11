use lurk_ff::LurkField;

use crate::{
  error::LurkError,
  eval::{
    evaluator::Evaluable,
    io::IO,
    witness::Witness,
  },
  ptr::Ptr,
  store::Store,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Frame<T: Copy, W: Copy> {
  pub input: T,
  pub output: T,
  pub i: usize,
  pub witness: W,
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

  // pub fn log(&self, store: &Store<F>) {
  //  // This frame's output is the input for the next frame.
  //  // Report that index. Otherwise we can't report the initial input.
  //  self.output.log(store, self.i + 1);
  //}

  pub fn significant_frame_count(frames: &[Frame<IO<F>, W>]) -> usize {
    frames.iter().rev().skip_while(|frame| frame.is_complete()).count()
  }

  pub fn input_vector(&self, store: &Store<F>) -> Result<Vec<F>, LurkError<F>> {
    self.input.to_vector(store)
  }

  pub fn output_vector(
    &self,
    store: &Store<F>,
  ) -> Result<Vec<F>, LurkError<F>> {
    self.output.to_vector(store)
  }
}

impl<F: LurkField, T: Evaluable<F, Witness<F>> + Clone + PartialEq + Copy>
  Frame<T, Witness<F>>
{
  pub(crate) fn next(
    &self,
    store: &mut Store<F>,
  ) -> Result<Self, LurkError<F>> {
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
  ) -> Result<Self, LurkError<F>> {
    input.log(store, 0);
    let (output, witness) = input.reduce(store)?;
    Ok(Self { input, output, i: 0, witness })
  }
}

#[derive(Debug)]
pub struct FrameIt<'a, W: Copy, F: LurkField> {
  pub first: bool,
  pub frame: Frame<IO<F>, W>,
  pub store: &'a mut Store<F>,
}

impl<'a, F: LurkField> FrameIt<'a, Witness<F>, F> {
  pub fn new(
    initial_input: IO<F>,
    store: &'a mut Store<F>,
  ) -> Result<Self, LurkError<F>> {
    let frame = Frame::from_initial_input(initial_input, store)?;
    Ok(Self { first: true, frame, store })
  }

  /// Like `.iter().take(n).last()`, but skips intermediary stages, to optimize
  /// for evaluation.
  pub fn next_n(
    mut self,
    n: usize,
  ) -> Result<
    (Frame<IO<F>, Witness<F>>, Frame<IO<F>, Witness<F>>, Vec<Ptr<F>>),
    LurkError<F>,
  > {
    let mut previous_frame = self.frame.clone();
    let mut emitted: Vec<Ptr<F>> = Vec::new();
    for _ in 0..n {
      if self.frame.is_complete() {
        break;
      }
      let new_frame = self.frame.next(self.store)?;

      if let Ok(expr) = new_frame.output.maybe_emitted_expression(self.store) {
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
pub struct ResultFrame<'a, F: LurkField>(
  pub Result<FrameIt<'a, Witness<F>, F>, LurkError<F>>,
);

impl<'a, F: LurkField> Iterator for ResultFrame<'a, F> {
  type Item = Result<Frame<IO<F>, Witness<F>>, LurkError<F>>;

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
