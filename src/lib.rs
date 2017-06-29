#[macro_use]
extern crate nom;

use std::fmt;
use std::fmt::{Display, Debug, Formatter};

use std::str;
use std::str::FromStr;

use std::ops::{Add, Sub, Mul, Div};

use nom::{digit, IError};

#[derive(Debug)]
pub struct Lambda(Expr);

impl Lambda {
    pub fn from_str(s: &str) -> Result<Lambda, ParseError> {
        expr(s.as_bytes())
            .to_full_result()
            .map(Lambda)
            .map_err(ParseError)
    }

    pub fn eval<V>(&self, vals: &[V], acc: V) -> Result<V, ValidationError>
        where V: Add<Output = V> + Sub<Output = V> + Mul<Output = V> + Div<Output = V> + Clone
    {
        if !self.0.iter().all(|i| i <= vals.len()) {
            return Err(ValidationError);
        }

        Ok(self.0
               .eval_with(&|i| if i == 0 {
                              acc.clone()
                          } else {
                              vals[i - 1].clone()
                          }))
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParseError(IError);

#[derive(Eq, PartialEq, Debug)]
pub struct ValidationError;

type Arg = usize;

pub enum Expr {
    Value(Arg),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

impl Expr {
    pub fn eval_with<F, V>(&self, f: &F) -> V
        where F: Fn(Arg) -> V,
              V: Add<Output = V> + Sub<Output = V> + Mul<Output = V> + Div<Output = V>
    {
        use Expr::*;
        match *self {
            Value(val) => f(val),
            Add(ref left, ref right) => left.eval_with(f) + right.eval_with(f),
            Sub(ref left, ref right) => left.eval_with(f) - right.eval_with(f),
            Mul(ref left, ref right) => left.eval_with(f) * right.eval_with(f),
            Div(ref left, ref right) => left.eval_with(f) / right.eval_with(f),
            Paren(ref expr) => expr.eval_with(f),
        }
    }

    pub fn iter(&self) -> ExprIter {
        ExprIter { stack: vec![self] }
    }
}

pub struct ExprIter<'a> {
    stack: Vec<&'a Expr>,
}

impl<'a> Iterator for ExprIter<'a> {
    type Item = Arg;

    fn next(&mut self) -> Option<Self::Item> {
        use Expr::*;

        while let Some(current) = self.stack.pop() {
            match *current {
                Value(val) => return Some(val),

                Add(ref left, ref right) |
                Sub(ref left, ref right) |
                Mul(ref left, ref right) |
                Div(ref left, ref right) => {
                    self.stack.push(right);
                    self.stack.push(left);
                }

                Paren(ref expr) => {
                    self.stack.push(expr);
                }
            }
        }

        None
    }
}

pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use Expr::*;
        match *self {
            Value(ref val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Paren(ref expr) => write!(format, "({})", expr),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, format: &mut Formatter) -> fmt::Result {
        use Expr::*;
        match *self {
            Value(ref val) => write!(format, "{:?}", val),
            Add(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Paren(ref expr) => write!(format, "[{:?}]", expr),
        }
    }
}

named!(parens< Expr >, ws!(
    delimited!(
        tag!("("),
        map!(
            map!(
                expr,
                Box::new
            ),
            Expr::Paren
        ),
        tag!(")"))
    )
);

named!(factor< Expr >, alt_complete!(
        map!(
            arg,
            Expr::Value
        )
        | parens
    )
);

named!(arg< Arg >, ws!(
        map_res!(
            map_res!(
                do_parse!(
                    char!('$') >>
                    d: digit >>
                    (d)
                ),
                str::from_utf8
                ),
                FromStr::from_str)
        )
      );

fn fold_exprs(initial: Expr, remainder: Vec<(Oper, Expr)>) -> Expr {
    remainder
        .into_iter()
        .fold(initial, |acc, pair| {
            let (oper, expr) = pair;
            match oper {
                Oper::Add => Expr::Add(Box::new(acc), Box::new(expr)),
                Oper::Sub => Expr::Sub(Box::new(acc), Box::new(expr)),
                Oper::Mul => Expr::Mul(Box::new(acc), Box::new(expr)),
                Oper::Div => Expr::Div(Box::new(acc), Box::new(expr)),
            }
        })
}

named!(term< Expr >, do_parse!(
    initial: factor >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("*") >> mul: factor >> (Oper::Mul, mul)) |
             do_parse!(tag!("/") >> div: factor >> (Oper::Div, div))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

named!(expr< Expr >, do_parse!(
    initial: term >>
    remainder: many0!(
           alt!(
             do_parse!(tag!("+") >> add: term >> (Oper::Add, add)) |
             do_parse!(tag!("-") >> sub: term >> (Oper::Sub, sub))
           )
         ) >>
    (fold_exprs(initial, remainder))
));

#[cfg(test)]
mod tests {
    use super::*;

    use nom::IResult;

    #[test]
    fn factor_test() {
        assert_eq!(factor(&b"  $3  "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("3")));
    }

    #[test]
    fn term_test() {
        assert_eq!(term(&b" $3 *  $5   "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(3 * 5)")));
    }

    #[test]
    fn expr_test() {
        assert_eq!(expr(&b" $1 + $2 *  $3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("(1 + (2 * 3))")));
        assert_eq!(expr(&b" $1 + $2 *  $3 / $4 - $5 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("((1 + ((2 * 3) / 4)) - 5)")));
        assert_eq!(expr(&b" $72 / $2 / $3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("((72 / 2) / 3)")));
    }

    #[test]
    fn parens_test() {
        assert_eq!(expr(&b" ( $1 + $2 ) *  $3 "[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], String::from("([(1 + 2)] * 3)")));
    }

    #[test]
    fn multidigit_test() {
        assert_eq!(expr(&b"$10"[..]).map(|x| format!("{:?}", x)),
                IResult::Done(&b""[..], "10".to_string()));
    }

    #[test]
    fn eval_test() {
        assert_eq!(expr(&b" ( $1 + $2 ) *  $3 "[..]).map(|x| x.eval_with(&|v| v)),
                IResult::Done(&b""[..], 9));
    }
}
