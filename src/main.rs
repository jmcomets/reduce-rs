extern crate reduce;

extern crate docopt;
extern crate serde;
#[macro_use] extern crate serde_derive;

use std::fmt;

use docopt::Docopt;

use serde::de::{Deserialize, Deserializer, Error, Visitor};

const USAGE: &str = r#"
Usage:
    reduce [options] <lambda> [<init>]
    reduce (-h|--help)

Apply the lambda expression to the input as a fold, with the lines being the
list and the columns being the arguments tuple. Integer, floating or boolean
arithmetics are available, depending on the type chosen (defaults to integer).

Arguments:
    LAMBDA      Valid haskell-y lambda expression (`args -> expression`).
                The expression should return a tuple of values
                (parens can be ignored for a singleton).
    INIT        Start value for the fold. Defaults:
                    "0" for integers
                    "0.0" for doubles
                    "true" for booleans

Options:
    --type=<type>               Sets the input type for the lambda expression [default: int].
    --separator=<sep>           Sets the separator [default: " "].
    --exit                      Use the exit code to output the result
                                (conflicts with multi-column output).
"#;


struct ReturnTypeVisitor;

impl<'de> Visitor<'de> for ReturnTypeVisitor {
    type Value = ReturnType;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a type int, float or bool")
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
        where E: Error
    {
        match s {
            "bool"  => Ok(ReturnType::Boolean),
            "float" => Ok(ReturnType::Floating),
            "int"   => Ok(ReturnType::Integer),
            _       => Err(Error::custom(format!("invalid type {}", s))),
        }
    }
}

#[derive(Debug)]
enum ReturnType {
    Boolean,
    Floating,
    Integer,
}

impl<'de> Deserialize<'de> for ReturnType {
    fn deserialize<D>(d: D) -> Result<ReturnType, D::Error>
        where D: Deserializer<'de>
    {
        d.deserialize_str(ReturnTypeVisitor)
    }
}


#[derive(Debug, Deserialize)]
struct Args {
    arg_lambda: String,
    arg_init: Option<String>,
    flag_type: ReturnType,
    flag_separator: String,
    flag_exit: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    println!("{:?}", args);

    let lambda = parse_input(&args.arg_lambda, args.flag_type).unwrap();
    println!("{:?}", lambda);
}

fn parse_input(s: &str, _: ReturnType) -> Result<reduce::Lambda, reduce::Error> {
    reduce::Lambda::from_str(s)
}
