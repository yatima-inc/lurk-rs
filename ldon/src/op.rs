use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
#[repr(u16)]
pub enum Op1 {
  Car,
  Cdr,
  Atom,
  Emit,
  Open,
  Secret,
  Commit,
  Num,
  Comm,
  Char,
  Eval,
  U64,
}

impl fmt::Display for Op1 {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Op1::Car => write!(f, "car#"),
      Op1::Cdr => write!(f, "cdr#"),
      Op1::Atom => write!(f, "atom#"),
      Op1::Emit => write!(f, "emit#"),
      Op1::Open => write!(f, "open#"),
      Op1::Secret => write!(f, "secret#"),
      Op1::Commit => write!(f, "commit#"),
      Op1::Num => write!(f, "num#"),
      Op1::Comm => write!(f, "comm#"),
      Op1::Char => write!(f, "char#"),
      Op1::Eval => write!(f, "eval#"),
      Op1::U64 => write!(f, "u64#"),
    }
  }
}

impl From<Op1> for u16 {
  fn from(val: Op1) -> Self { val as u16 }
}

impl TryFrom<u16> for Op1 {
  type Error = String;

  fn try_from(x: u16) -> Result<Self, Self::Error> {
    match x {
      x if x == Op1::Car as u16 => Ok(Op1::Car),
      x if x == Op1::Cdr as u16 => Ok(Op1::Cdr),
      x if x == Op1::Atom as u16 => Ok(Op1::Atom),
      x if x == Op1::Emit as u16 => Ok(Op1::Emit),
      x if x == Op1::Open as u16 => Ok(Op1::Open),
      x if x == Op1::Secret as u16 => Ok(Op1::Secret),
      x if x == Op1::Commit as u16 => Ok(Op1::Commit),
      x if x == Op1::Num as u16 => Ok(Op1::Num),
      x if x == Op1::Comm as u16 => Ok(Op1::Comm),
      x if x == Op1::Char as u16 => Ok(Op1::Char),
      x if x == Op1::Eval as u16 => Ok(Op1::Eval),
      x if x == Op1::U64 as u16 => Ok(Op1::U64),
      x => Err(format!("Invalid Op1 value: {}", x)),
    }
  }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
#[repr(u16)]
pub enum Op2 {
  Sum = 0b0011_0000_0000_0000,
  Diff,
  Product,
  Quotient,
  Equal,
  NumEqual,
  Less,
  Greater,
  LessEqual,
  GreaterEqual,
  Cons,
  StrCons,
  Begin,
  Hide,
  Modulo,
  Eval,
}
impl fmt::Display for Op2 {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Op2::Sum => write!(f, "sum#"),
      Op2::Diff => write!(f, "diff#"),
      Op2::Product => write!(f, "prod#"),
      Op2::Quotient => write!(f, "quot#"),
      Op2::Equal => write!(f, "eql#"),
      Op2::NumEqual => write!(f, "numeql#"),
      Op2::Less => write!(f, "lth#"),
      Op2::Greater => write!(f, "gth#"),
      Op2::LessEqual => write!(f, "leq#"),
      Op2::GreaterEqual => write!(f, "geq#"),
      Op2::Cons => write!(f, "cons#"),
      Op2::StrCons => write!(f, "strcons#"),
      Op2::Begin => write!(f, "begin#"),
      Op2::Hide => write!(f, "hide#"),
      Op2::Modulo => write!(f, "modulo#"),
      Op2::Eval => write!(f, "eval#"),
    }
  }
}

impl From<Op2> for u16 {
  fn from(val: Op2) -> Self { val as u16 }
}

impl TryFrom<u16> for Op2 {
  type Error = String;

  fn try_from(x: u16) -> Result<Self, Self::Error> {
    match x {
      x if x == Op2::Sum as u16 => Ok(Op2::Sum),
      x if x == Op2::Diff as u16 => Ok(Op2::Diff),
      x if x == Op2::Product as u16 => Ok(Op2::Product),
      x if x == Op2::Quotient as u16 => Ok(Op2::Quotient),
      x if x == Op2::Equal as u16 => Ok(Op2::Equal),
      x if x == Op2::NumEqual as u16 => Ok(Op2::NumEqual),
      x if x == Op2::Less as u16 => Ok(Op2::Less),
      x if x == Op2::Greater as u16 => Ok(Op2::Greater),
      x if x == Op2::LessEqual as u16 => Ok(Op2::LessEqual),
      x if x == Op2::GreaterEqual as u16 => Ok(Op2::GreaterEqual),
      x if x == Op2::Cons as u16 => Ok(Op2::Cons),
      x if x == Op2::StrCons as u16 => Ok(Op2::StrCons),
      x if x == Op2::Begin as u16 => Ok(Op2::Begin),
      x if x == Op2::Hide as u16 => Ok(Op2::Hide),
      x if x == Op2::Modulo as u16 => Ok(Op2::Modulo),
      x if x == Op2::Eval as u16 => Ok(Op2::Eval),
      x => Err(format!("Invalid Op2 value: {}", x)),
    }
  }
}

#[cfg(feature = "test-utils")]
pub mod test_utils {
  use lurk_ff::test_utils::frequency;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;

  impl Arbitrary for Op1 {
    fn arbitrary(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Self>)> = vec![
        (100, Box::new(|_| Self::Car)),
        (100, Box::new(|_| Self::Cdr)),
        (100, Box::new(|_| Self::Atom)),
        (100, Box::new(|_| Self::Emit)),
        (100, Box::new(|_| Self::Open)),
        (100, Box::new(|_| Self::Secret)),
        (100, Box::new(|_| Self::Commit)),
        (100, Box::new(|_| Self::Num)),
        (100, Box::new(|_| Self::Comm)),
        (100, Box::new(|_| Self::Char)),
        (100, Box::new(|_| Self::Eval)),
        (100, Box::new(|_| Self::U64)),
      ];
      frequency(g, input)
    }
  }
  impl Arbitrary for Op2 {
    fn arbitrary(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Self>)> = vec![
        (100, Box::new(|_| Self::Sum)),
        (100, Box::new(|_| Self::Diff)),
        (100, Box::new(|_| Self::Product)),
        (100, Box::new(|_| Self::Quotient)),
        (100, Box::new(|_| Self::Equal)),
        (100, Box::new(|_| Self::NumEqual)),
        (100, Box::new(|_| Self::Less)),
        (100, Box::new(|_| Self::Greater)),
        (100, Box::new(|_| Self::LessEqual)),
        (100, Box::new(|_| Self::GreaterEqual)),
        (100, Box::new(|_| Self::Cons)),
        (100, Box::new(|_| Self::StrCons)),
        (100, Box::new(|_| Self::Begin)),
        (100, Box::new(|_| Self::Hide)),
        (100, Box::new(|_| Self::Modulo)),
        (100, Box::new(|_| Self::Eval)),
      ];
      frequency(g, input)
    }
  }
}

#[cfg(test)]
#[cfg(feature = "test-utils")]
pub mod tests {

  use super::*;

  #[quickcheck]
  fn prop_op1_into_u16(t: Op1) -> bool {
    let x: u16 = t.into();
    if let Ok(t2) = Op1::try_from(x) {
      t2 == t
    }
    else {
      false
    }
  }
  #[quickcheck]
  fn prop_op2_into_u16(t: Op2) -> bool {
    let x: u16 = t.into();
    if let Ok(t2) = Op2::try_from(x) {
      t2 == t
    }
    else {
      false
    }
  }
}
