use std::{
  convert::TryFrom,
  fmt,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Tag {
  pub version: Version, // u32
  pub field: FieldTag,  // u16
  pub expr: ExprTag,    // u16
}

impl From<Tag> for u64 {
  fn from(val: Tag) -> Self {
    let v: u32 = val.version.into();
    let x: u16 = val.expr.into();
    ((v as u64) << 32) + ((val.field as u64) << 16) + (x as u64)
  }
}

impl TryFrom<u64> for Tag {
  type Error = String;

  fn try_from(f: u64) -> Result<Self, Self::Error> {
    let version = Version::from((f >> 32) as u32);
    let field = FieldTag::try_from(((f & 0xffff_0000) >> 16) as u16)?;
    let expr = ExprTag::try_from((f & 0xffff) as u16)?;
    Ok(Tag { version, field, expr })
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum FieldTag {
  BLS12_381 = 0b0000_0000_0000_0000,
  Pallas,
  Vesta,
}

impl From<FieldTag> for u16 {
  fn from(val: FieldTag) -> Self { val as u16 }
}

impl TryFrom<u16> for FieldTag {
  type Error = String;

  fn try_from(x: u16) -> Result<Self, Self::Error> {
    match x {
      f if f == FieldTag::BLS12_381 as u16 => Ok(FieldTag::BLS12_381),
      f if f == FieldTag::Pallas as u16 => Ok(FieldTag::Pallas),
      f if f == FieldTag::Vesta as u16 => Ok(FieldTag::Vesta),
      x => Err(format!("Invalid FieldTag value: {}", x)),
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Version {
  pub major: u16,
  pub minor: u8,
  pub patch: u8,
}

impl From<Version> for u32 {
  fn from(val: Version) -> Self {
    ((val.major as u32) << 16) + ((val.minor as u32) << 8) + (val.patch as u32)
  }
}

impl From<u32> for Version {
  fn from(x: u32) -> Self {
    Version {
      major: (x >> 16) as u16,
      minor: ((x >> 8) & 0xff) as u8,
      patch: (x & 0xff) as u8,
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum ExprTag {
  Cons = 0b0000_0000_0000_0000,
  Sym,
  Fun,
  Num,
  Thunk,
  Str,
  Char,
  Comm,
  U64,
  Map,
  Link,
  Outermost = 0b0001_0000_0000_0000,
  Call0,
  Call,
  Call2,
  Tail,
  Error,
  Lookup,
  Unop,
  Binop,
  Binop2,
  If,
  Let,
  LetRec,
  Dummy,
  Terminal,
  Emit,
  Op1 = 0b0010_0000_0000_0000,
  Op2 = 0b0011_0000_0000_0000,
}

impl From<ExprTag> for u16 {
  fn from(val: ExprTag) -> Self { val as u16 }
}

impl TryFrom<u16> for ExprTag {
  type Error = String;

  fn try_from(x: u16) -> Result<Self, <ExprTag as TryFrom<u16>>::Error> {
    match x {
      f if f == ExprTag::Cons as u16 => Ok(ExprTag::Cons),
      f if f == ExprTag::Sym as u16 => Ok(ExprTag::Sym),
      f if f == ExprTag::Fun as u16 => Ok(ExprTag::Fun),
      f if f == ExprTag::Thunk as u16 => Ok(ExprTag::Thunk),
      f if f == ExprTag::Num as u16 => Ok(ExprTag::Num),
      f if f == ExprTag::Str as u16 => Ok(ExprTag::Str),
      f if f == ExprTag::Char as u16 => Ok(ExprTag::Char),
      f if f == ExprTag::Comm as u16 => Ok(ExprTag::Comm),
      f if f == ExprTag::U64 as u16 => Ok(ExprTag::U64),
      f if f == ExprTag::Map as u16 => Ok(ExprTag::Map),
      f if f == ExprTag::Link as u16 => Ok(ExprTag::Link),
      f if f == ExprTag::Outermost as u16 => Ok(ExprTag::Outermost),
      f if f == ExprTag::Call0 as u16 => Ok(ExprTag::Call0),
      f if f == ExprTag::Call as u16 => Ok(ExprTag::Call),
      f if f == ExprTag::Call2 as u16 => Ok(ExprTag::Call2),
      f if f == ExprTag::Tail as u16 => Ok(ExprTag::Tail),
      f if f == ExprTag::Error as u16 => Ok(ExprTag::Error),
      f if f == ExprTag::Lookup as u16 => Ok(ExprTag::Lookup),
      f if f == ExprTag::Unop as u16 => Ok(ExprTag::Unop),
      f if f == ExprTag::Binop as u16 => Ok(ExprTag::Binop),
      f if f == ExprTag::Binop2 as u16 => Ok(ExprTag::Binop2),
      f if f == ExprTag::If as u16 => Ok(ExprTag::If),
      f if f == ExprTag::Let as u16 => Ok(ExprTag::Let),
      f if f == ExprTag::LetRec as u16 => Ok(ExprTag::LetRec),
      f if f == ExprTag::Dummy as u16 => Ok(ExprTag::Dummy),
      f if f == ExprTag::Terminal as u16 => Ok(ExprTag::Terminal),
      f if f == ExprTag::Emit as u16 => Ok(ExprTag::Emit),
      f if f == ExprTag::Op1 as u16 => Ok(ExprTag::Op1),
      f if f == ExprTag::Op2 as u16 => Ok(ExprTag::Op2),
      f => Err(format!("Invalid ExprTag value: {}", f)),
    }
  }
}

impl fmt::Display for ExprTag {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ExprTag::Cons => write!(f, "cons#"),
      ExprTag::Sym => write!(f, "sym#"),
      ExprTag::Fun => write!(f, "fun#"),
      ExprTag::Num => write!(f, "num#"),
      ExprTag::Thunk => write!(f, "thunk#"),
      ExprTag::Str => write!(f, "str#"),
      ExprTag::Char => write!(f, "char#"),
      ExprTag::Comm => write!(f, "comm#"),
      ExprTag::U64 => write!(f, "u64#"),
      ExprTag::Map => write!(f, "map#"),
      ExprTag::Link => write!(f, "link#"),
      ExprTag::Outermost => write!(f, "outermost#"),
      ExprTag::Call0 => write!(f, "call0#"),
      ExprTag::Call => write!(f, "call#"),
      ExprTag::Call2 => write!(f, "call2#"),
      ExprTag::Tail => write!(f, "tail#"),
      ExprTag::Error => write!(f, "error#"),
      ExprTag::Lookup => write!(f, "lookup#"),
      ExprTag::Unop => write!(f, "unop#"),
      ExprTag::Binop => write!(f, "binop#"),
      ExprTag::Binop2 => write!(f, "binop2#"),
      ExprTag::If => write!(f, "if#"),
      ExprTag::Let => write!(f, "let#"),
      ExprTag::LetRec => write!(f, "letrec#"),
      ExprTag::Dummy => write!(f, "dummy#"),
      ExprTag::Terminal => write!(f, "terminal#"),
      ExprTag::Emit => write!(f, "emit#"),
      ExprTag::Op1 => write!(f, "op1#"),
      ExprTag::Op2 => write!(f, "op2#"),
    }
  }
}

#[cfg(feature = "test-utils")]
pub mod test_utils {
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use super::*;
  use crate::test_utils::frequency;
  impl Arbitrary for Version {
    fn arbitrary(g: &mut Gen) -> Self {
      Version {
        major: Arbitrary::arbitrary(g),
        minor: Arbitrary::arbitrary(g),
        patch: Arbitrary::arbitrary(g),
      }
    }
  }

  impl Arbitrary for FieldTag {
    fn arbitrary(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Self>)> = vec![
        (100, Box::new(|_| Self::BLS12_381)),
        (100, Box::new(|_| Self::Pallas)),
        (100, Box::new(|_| Self::Vesta)),
      ];
      frequency(g, input)
    }
  }

  impl Arbitrary for ExprTag {
    fn arbitrary(g: &mut Gen) -> Self {
      #[allow(clippy::type_complexity)]
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> Self>)> = vec![
        (100, Box::new(|_| Self::Cons)),
        (100, Box::new(|_| Self::Sym)),
        (100, Box::new(|_| Self::Fun)),
        (100, Box::new(|_| Self::Num)),
        (100, Box::new(|_| Self::Thunk)),
        (100, Box::new(|_| Self::Str)),
        (100, Box::new(|_| Self::Char)),
        (100, Box::new(|_| Self::Comm)),
        (100, Box::new(|_| Self::U64)),
        (100, Box::new(|_| Self::Outermost)),
        (100, Box::new(|_| Self::Call0)),
        (100, Box::new(|_| Self::Call)),
        (100, Box::new(|_| Self::Call2)),
        (100, Box::new(|_| Self::Tail)),
        (100, Box::new(|_| Self::Error)),
        (100, Box::new(|_| Self::Lookup)),
        (100, Box::new(|_| Self::Unop)),
        (100, Box::new(|_| Self::Binop)),
        (100, Box::new(|_| Self::Binop2)),
        (100, Box::new(|_| Self::If)),
        (100, Box::new(|_| Self::Let)),
        (100, Box::new(|_| Self::LetRec)),
        (100, Box::new(|_| Self::Dummy)),
        (100, Box::new(|_| Self::Terminal)),
        (100, Box::new(|_| Self::Emit)),
        (100, Box::new(|_| Self::Op1)),
        (100, Box::new(|_| Self::Op2)),
      ];
      frequency(g, input)
    }
  }

  impl Arbitrary for Tag {
    fn arbitrary(g: &mut Gen) -> Self {
      Tag {
        version: Arbitrary::arbitrary(g),
        field: Arbitrary::arbitrary(g),
        expr: Arbitrary::arbitrary(g),
      }
    }
  }
}

#[cfg(test)]
#[cfg(feature = "test-utils")]
pub mod tests {

  use super::*;

  #[quickcheck]
  fn prop_version_into_u32(v: Version) -> bool {
    let x: u32 = v.into();
    let v2: Version = x.into();
    v2 == v
  }

  #[quickcheck]
  fn prop_version_from_u32(x: u32) -> bool {
    let v: Version = x.into();
    let x2: u32 = v.into();
    x2 == x
  }

  #[quickcheck]
  fn prop_expr_tag_into_u16(t: ExprTag) -> bool {
    let x: u16 = t.into();
    if let Ok(t2) = ExprTag::try_from(x) {
      t2 == t
    }
    else {
      false
    }
  }

  #[quickcheck]
  fn prop_field_tag_into_u16(f: FieldTag) -> bool {
    let x: u16 = f.into();
    if let Ok(f2) = FieldTag::try_from(x) {
      f2 == f
    }
    else {
      false
    }
  }

  #[quickcheck]
  fn prop_tag_into_u64(t: Tag) -> bool {
    let x: u64 = t.into();
    if let Ok(t2) = Tag::try_from(x) {
      t2 == t
    }
    else {
      false
    }
  }
}
