
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Frame<T: Copy, W: Copy> {
  pub input: T,
  pub output: T,
  pub i: usize,
  pub witness: W,
}
