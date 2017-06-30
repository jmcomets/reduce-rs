use std::fmt;
use std::str::FromStr;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, PartialOrd, Deserialize, Clone, Default)]
pub struct AutoInt(i32);

impl From<i32> for AutoInt {
    fn from(i: i32) -> AutoInt {
        AutoInt(i)
    }
}

impl Into<i32> for AutoInt {
    fn into(self) -> i32 {
        self.0
    }
}

macro_rules! derive_binary_op {
    ($t:tt, $m:tt) => {
        impl $t for AutoInt {
            type Output = AutoInt;

            fn $m(self, rhs: AutoInt) -> AutoInt {
                AutoInt((self.0).$m(rhs.0))
            }
        }
    }
}

use std::ops::{Add, Sub, Mul, Div};
derive_binary_op!(Add, add);
derive_binary_op!(Sub, sub);
derive_binary_op!(Mul, mul);
derive_binary_op!(Div, div);

impl FromStr for AutoInt {
    type Err = <i32 as FromStr>::Err;

    fn from_str(s: &str) -> Result<AutoInt, Self::Err> {
        i32::from_str(s).map(AutoInt)
    }
}

impl Display for AutoInt {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        write!(format, "{}", self.0)
    }
}

impl Debug for AutoInt {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        write!(format, "{:?}", self.0)
    }
}

impl From<bool> for AutoInt {
    fn from(b: bool) -> AutoInt {
        if b {
            AutoInt(1)
        } else {
            AutoInt(0)
        }
    }
}
