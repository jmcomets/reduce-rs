#[macro_use]
extern crate nom;

extern crate serde;
#[macro_use] extern crate serde_derive;

mod lambda;
mod expr;
mod auto_int;

pub use lambda::{Lambda, ParseError};
pub use auto_int::AutoInt;
