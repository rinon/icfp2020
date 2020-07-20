/// (De-)modulation of numbers
use bitvec::bitvec;

use bitvec::order::Msb0;
use bitvec::slice::AsBits;
use bitvec::vec::BitVec;
use nom::bytes::complete::{take, tag};
use nom::character::complete::char;
use nom::branch::alt;
use nom::combinator::map;
use nom::multi::{count, many0_count};
use nom::sequence::{pair, preceded, terminated};
use nom::{error::VerboseError, IResult};

use std::rc::Rc;

use super::{Environment, Expr, Value, cons};


pub fn modulate(input: &Expr) -> Result<String, String> {
    modulate_element(input)
}

fn modulate_element(input: &Expr) -> Result<String, String> {
    match input.value() {
        Value::Num(num) => modulate_num(num),
        Value::T | Value::Nil => Ok("00".to_string()),
        _ => match input.cons_value() {
            Some((x, xs)) => {
                let mut s = "11".to_string();
                s += &modulate_element(&*x)?;
                s += &modulate_element(&*xs)?;
                Ok(s)
            }
            None => Err(format!("Cannot modulate value {:?}", input)),
        }
    }
}

fn modulate_num(num: i64) -> Result<String, String> {
    let mut s = if num < 0 {
        "10".to_string()
    } else {
        "01".to_string()
    };
    let num = num.wrapping_abs() as u64;
    let len: usize = if num == 0 {
        0
    } else {
        ((64 - num.leading_zeros() + 3) / 4) as usize
    };
    s.extend(vec!['1'; len]);
    s.push('0');
    if len > 0 {
        s += &format!("{:0width$b}", num, width=len*4);
    }
    Ok(s)
}

pub fn demodulate(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    alt((
        preceded(
            tag("11"),
            map(pair(demodulate, demodulate), |(v1, v2)| {
                cons(v1, v2)
            }),
        ),
        value,
    ))(input)
}

fn value(input: &str) -> IResult<&str, Rc<Expr>, VerboseError<&str>> {
    let (input, s) = sign(input)?;
    if let Sign::Nil = s {
        return Ok((input, Expr::new(Value::Nil)));
    }

    let (input, len) = terminated(many0_count(char('1')), char('0'))(input)?;

    let (input, value) = take(4*len)(input)?;

    let value = if len == 0 {
        0i64
    } else {
        i64::from_str_radix(value, 2)
            .expect("Could not parse integer")
    };

    match s {
        Sign::Positive => Ok((input, Expr::new(value.into()))),
        Sign::Negative => Ok((input, Expr::new((-value).into()))),
        _ => unreachable!("Handled above"),
    }
}

#[derive(Debug)]
enum Sign {
    Nil,
    Positive,
    Negative,
}

fn sign(input: &str) -> IResult<&str, Sign, VerboseError<&str>> {
    alt((
        map(tag("00"), |_| Sign::Nil),
        map(tag("01"), |_| Sign::Positive),
        map(tag("10"), |_| Sign::Negative),
    ))(input)
}

// #[test]
// fn demodulate_msg13() {
//     assert_eq!(demodulate(&[0b01000000]).unwrap().1, &[Some(0)]);
//     assert_eq!(demodulate(&[0b01100001]).unwrap().1, &[Some(1)]);
//     assert_eq!(demodulate(&[0b10100001]).unwrap().1, &[Some(-1)]);
//     assert_eq!(demodulate(&[0b01100010]).unwrap().1, &[Some(2)]);
//     assert_eq!(demodulate(&[0b10100010]).unwrap().1, &[Some(-2)]);

//     assert_eq!(
//         demodulate(&[0b01110000, 0b10000000]).unwrap().1,
//         &[Some(16)]
//     );
//     assert_eq!(
//         demodulate(&[0b10110000, 0b10000000]).unwrap().1,
//         &[Some(-16)]
//     );

//     assert_eq!(
//         demodulate(&[0b01110111, 0b11111000]).unwrap().1,
//         &[Some(255)]
//     );
//     assert_eq!(
//         demodulate(&[0b10110111, 0b11111000]).unwrap().1,
//         &[Some(-255)]
//     );
//     assert_eq!(
//         demodulate(&[0b01111000, 0b01000000, 0b00000000]).unwrap().1,
//         &[Some(256)]
//     );
//     assert_eq!(
//         demodulate(&[0b10111000, 0b01000000, 0b00000000]).unwrap().1,
//         &[Some(-256)]
//     );
// }
