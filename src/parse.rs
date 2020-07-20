use nom::bytes::complete::tag;
use nom::character::complete::{char, alphanumeric1, digit1, space0, space1, one_of};
use nom::combinator::{map, map_res, all_consuming, recognize, opt};
use nom::error::{VerboseError};
use nom::sequence::{pair, terminated, preceded, separated_pair, delimited};
use nom::multi::{many0, separated_list, many_till};
use nom::{branch::alt, IResult};

use std::iter::FromIterator;
use std::num::ParseIntError;
use std::rc::Rc;

use super::{Expr, Value, apply};

pub fn parse_line(input: &str) -> Result<(Rc<Expr>, Rc<Expr>), nom::Err<VerboseError<&str>>> {
    all_consuming(
        separated_pair(element, terminated(tag("="), space1), element),
    )(input).map(|(_, expr)| expr)
}

pub fn parse(input: &str) -> Result<Rc<Expr>, nom::Err<VerboseError<&str>>> {
    all_consuming(element)(input)
        .map(|(_, expr)| expr)
}

pub fn element(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    alt((
        cons,
        ap,
        symbol,
        keyword,
        number,
        variable,
        list,
        linear,
    ))(input)
}

fn cons(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map(
        many_till(
            preceded(
                terminated(tag("ap ap cons"), space1),
                element,
            ),
            terminated(tag("nil"), space0),
        ),
        |(items, _nil)| {
            Expr::new_list(&items)
        }
    )(input)
}

fn symbol(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    map_res(
        terminated(preceded(char(':'), digit1), space0),
        |num: &str| -> Result<Rc<Expr>, ParseIntError> {
            Ok(Expr::new(Value::Symbol(num.parse()?)))
        },
    )(input)

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
            apply(arg1, arg2)
        }),
    )(input)
}

// fn eq(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
//     map(
//         separated_pair(element, terminated(tag("="), space1), element),
//         |(arg1, arg2)| {
//             Expr::new_fn(&[
//                 Expr::new(Value::Eq),
//                 arg1,
//                 arg2,
//             ])
//         },
//     )(input)
// }

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
        |list| Expr::new_list(&list),
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
            Ok(Expr::new(Value::Linear(String::from_iter(bitstring))))
        },
    )(input)
}
