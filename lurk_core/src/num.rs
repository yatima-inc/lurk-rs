use std::{
  cmp::Ordering,
  fmt::Display,
  hash::Hash,
  ops::{
    AddAssign,
    DivAssign,
    MulAssign,
    SubAssign,
  },
};

// use serde::{
//  Deserialize,
//  Serialize,
//};
use lurk_ff::LurkField;

use crate::uint::UInt;

/// Finite field element type for Lurk. Has different internal representations
/// to optimize evaluation. The distinction between `Num` and `uint::UInt` is
/// that while `Num` might internally sometimes be a u64, a `UInt::U64` is
/// guaranteed to always be a `u64`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Num<F: LurkField> {
  Scalar(F),
  U64(u64),
}

impl<F: LurkField> Copy for Num<F> {}

impl<F: LurkField> Display for Num<F> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Num::Scalar(s) => {
        let le_bytes = s.to_repr();
        write!(f, "0x")?;
        for &b in le_bytes.as_ref().iter().rev() {
          write!(f, "{:02x}", b)?;
        }
        Ok(())
      },
      Num::U64(n) => write!(f, "{}", n),
    }
  }
}

#[allow(clippy::derive_hash_xor_eq)]
impl<F: LurkField> Hash for Num<F> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    match self {
      Num::Scalar(s) => s.to_repr().as_ref().hash(state),

      Num::U64(n) => {
        let mut bytes = F::Repr::default();

        bytes.as_mut()[..8].copy_from_slice(&n.to_le_bytes());
        bytes.as_ref().hash(state);
      },
    }
  }
}

impl<F: LurkField> PartialOrd for Num<F> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    if self == other {
      return Some(Ordering::Equal);
    };

    if self.is_less_than(other) {
      Some(Ordering::Less)
    }
    else {
      Some(Ordering::Greater)
    }
  }
}

impl<F: LurkField> AddAssign for Num<F> {
  fn add_assign(&mut self, rhs: Self) {
    match (*self, rhs) {
      (Num::U64(ref mut a), Num::U64(b)) => {
        if let Some(res) = a.checked_add(b) {
          *self = Num::U64(res);
        }
        else {
          *self = Num::Scalar(F::from(*a) + F::from(b));
        }
      },
      (Num::Scalar(ref mut a), Num::Scalar(b)) => {
        *a += b;
        *self = Num::Scalar(*a);
      },
      (Num::Scalar(ref mut a), Num::U64(b)) => {
        *a += F::from(b);
        *self = Num::Scalar(*a);
      },
      (Num::U64(a), Num::Scalar(b)) => {
        *self = Num::Scalar(F::from(a) + b);
      },
    }
  }
}

impl<F: LurkField> SubAssign for Num<F> {
  fn sub_assign(&mut self, rhs: Self) {
    match (*self, rhs) {
      (Num::U64(ref mut a), Num::U64(b)) => {
        if let Some(res) = a.checked_sub(b) {
          *self = Num::U64(res);
        }
        else {
          *self = Num::Scalar(F::from(*a) - F::from(b));
        }
      },
      (Num::Scalar(ref mut a), Num::Scalar(b)) => {
        *a -= b;
        *self = Num::Scalar(*a);
      },
      (Num::Scalar(ref mut a), Num::U64(b)) => {
        *a -= F::from(b);
        *self = Num::Scalar(*a);
      },
      (Num::U64(a), Num::Scalar(b)) => {
        *self = Num::Scalar(F::from(a) - b);
      },
    }
  }
}

impl<F: LurkField> MulAssign for Num<F> {
  fn mul_assign(&mut self, rhs: Self) {
    match (*self, rhs) {
      (Num::U64(ref mut a), Num::U64(b)) => {
        if let Some(res) = a.checked_mul(b) {
          *self = Num::U64(res);
        }
        else {
          *self = Num::Scalar(F::from(*a) * F::from(b));
        }
      },
      (Num::Scalar(ref mut a), Num::Scalar(b)) => {
        *a *= b;
        *self = Num::Scalar(*a);
      },
      (Num::Scalar(ref mut a), Num::U64(b)) => {
        *a *= F::from(b);
        *self = Num::Scalar(*a);
      },
      (Num::U64(a), Num::Scalar(b)) => {
        *self = Num::Scalar(F::from(a) * b);
      },
    }
  }
}

impl<F: LurkField> DivAssign for Num<F> {
  fn div_assign(&mut self, rhs: Self) {
    assert!(!rhs.is_zero(), "can not divide by 0");
    match (*self, rhs) {
      (Num::U64(ref mut a), Num::U64(b)) => {
        // The result will only be Num::U64 if b divides a.
        if *a % b == 0 {
          *self = Num::U64(*a / b);
        }
        else {
          *self = Num::Scalar(F::from(*a) * F::from(b).invert().unwrap());
        }
      },
      (Num::Scalar(ref mut a), Num::Scalar(b)) => {
        *a *= b.invert().unwrap();
        *self = Num::Scalar(*a);
      },
      (Num::Scalar(ref mut a), Num::U64(b)) => {
        *a *= F::from(b).invert().unwrap();
        *self = Num::Scalar(*a);
      },
      (Num::U64(a), Num::Scalar(b)) => {
        *self = Num::Scalar(F::from(a) * b.invert().unwrap());
      },
    }
  }
}

impl<F: LurkField> Num<F> {
  pub fn is_zero(&self) -> bool {
    match self {
      Num::Scalar(s) => s.is_zero_vartime(),
      Num::U64(n) => n == &0,
    }
  }

  fn is_less_than(&self, other: &Num<F>) -> bool {
    match (self.is_negative(), other.is_negative()) {
      // Both positive or both negative
      (true, true) | (false, false) => self.is_less_than_aux(*other),
      (true, false) => true,
      (false, true) => false,
    }
  }

  fn is_less_than_aux(&self, other: Num<F>) -> bool {
    match (self, other) {
      (Num::U64(s), Num::U64(other)) => s < &other,
      (Num::Scalar(s), Num::Scalar(other)) => {
        Num::Scalar(*s - other).is_negative()
      },
      (a, b) => Num::Scalar(a.into_scalar()) < Num::Scalar(b.into_scalar()),
    }
  }

  pub fn most_negative() -> Self { Num::Scalar(F::most_negative()) }

  pub fn most_positive() -> Self { Num::Scalar(F::most_positive()) }

  /// Returns true if `self` is negative.
  pub fn is_negative(&self) -> bool {
    match self {
      // This assumes field modulus is >= 65 bits.
      Num::U64(_) => false,
      Num::Scalar(s) => s.is_negative(),
    }
  }

  pub fn into_scalar(self) -> F {
    match self {
      Num::U64(n) => F::from(n),
      Num::Scalar(s) => s,
    }
  }

  pub fn from_scalar(s: F) -> Self { Num::Scalar(s) }
}

impl<F: LurkField> From<u64> for Num<F> {
  fn from(n: u64) -> Self { Num::<F>::U64(n) }
}

impl<F: LurkField> From<UInt> for Num<F> {
  fn from(n: UInt) -> Self {
    match n {
      UInt::U64(n) => Num::<F>::U64(n),
    }
  }
}

// impl<F: LurkField> Serialize for Num<F> {
//  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//  where S: serde::Serializer {
//    match self {
//      Num::Scalar(f) => FWrap::serialize(&FWrap(*f), serializer),
//      Num::U64(x) => FWrap::serialize(&FWrap(F::from(*x)), serializer),
//    }
//  }
//}
// impl<'de, F: LurkField> Deserialize<'de> for Num<F> {
//  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//  where D: serde::Deserializer<'de> {
//    let f = FWrap::deserialize(deserializer)?;
//    Ok(Num::Scalar(f.0))
//  }
//}
