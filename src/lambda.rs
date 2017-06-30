use std::str::FromStr;

use expr::{Expr, BinaryOps};

pub use expr::ParseError;

#[derive(Debug)]
pub struct Lambda(Expr);

pub trait Value : BinaryOps + Clone + Default
{
}

impl<T> Value for T where T: BinaryOps + Clone + Default
{
}

impl Lambda {
    pub fn eval<V>(&self, args: &[V], zero: V) -> V
        where V: Value
    {
        self.0.eval(&|i| if i == 0 {
            zero.clone()
        } else if i > args.len() {
            V::default()
        } else {
            args[i - 1].clone()
        })
    }
}

impl FromStr for Lambda {
    type Err = <Expr as FromStr>::Err;

    fn from_str(s: &str) -> Result<Lambda, ParseError> {
        Expr::from_str(s).map(Lambda)
    }
}
