use nom::bytes::complete::tag;
use nom::character::complete::{char, alphanumeric1, digit1, space0, space1, one_of};
use nom::combinator::{map, map_res, all_consuming, recognize, opt};
use nom::error::{VerboseError};
use nom::sequence::{pair, terminated, preceded, separated_pair, delimited};
use nom::multi::{many0, separated_list};
use nom::{branch::alt, IResult};

use std::num::ParseIntError;
use std::rc::Rc;

use super::{Expr, Value};

pub fn parse(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    all_consuming(alt((
        eq,
        element,
    )))(input)
}

pub fn element(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    alt((
        keyword,
        number,
        variable,
        ap,
        list,
        linear,
    ))(input)
}

fn keyword(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map_res(terminated(alphanumeric1, space0), |word: &str| {
        word.parse().map(|value| Expr::new(value))
    })(input)
}

fn number(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map_res(
        terminated(recognize(pair(opt(char('-')), digit1)), space0),
        |num: &str| -> Result<Rc<Expr>, ParseIntError> {
            Ok(Expr::new(Value::Num(num.parse()?)))
        },
    )(input)
}

fn ap(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    preceded(
        terminated(tag("ap"), space1),
        map(pair(element, element), |(arg1, arg2)| {
            Expr::new_apply(arg1, arg2,)
        }),
    )(input)
}

fn eq(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map(
        separated_pair(element, terminated(tag("="), space1), element),
        |(arg1, arg2)| {
            Expr::new_fn(&[
                Expr::new(Value::Eq),
                arg1,
                arg2,
            ])
        },
    )(input)
}

fn variable(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map_res(
        terminated(preceded(char('x'), digit1), space0),
        |num: &str| -> Result<Rc<Expr>, ParseIntError> {
            Ok(Expr::new(Value::Variable(num.parse()?)))
        },
    )(input)
}

fn list(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map(
        delimited(
            terminated(tag("("), space0),
            separated_list(terminated(tag(","), space0), element),
            terminated(tag(")"), space0),
        ),
        |list| Expr::new_list(list),
    )(input)
}

fn linear(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map_res(
        delimited(
            terminated(tag("["), space0),
            many0(one_of("01")),
            terminated(tag("]"), space0),
        ),
        |bitstring| -> Result<Rc<Expr>, ParseIntError> {
            Ok(Expr::new(
                Value::Linear(bitstring.chunks(8).map(|s| {
                    let mut s: String = s.iter().collect();
                    while s.len() < 8 {
                        s.push('0')
                    }
                    u8::from_str_radix(&s, 2)
                }).collect::<Result<Vec<_>, ParseIntError>>()?),
            ))
        },
    )(input)
}
