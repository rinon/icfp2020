/// (De-)modulation of numbers
use bitvec::bitvec;

use bitvec::order::Msb0;
use bitvec::slice::AsBits;
use bitvec::vec::BitVec;
use nom::bits::bits;
use nom::bits::streaming::{tag, take};
use nom::branch::alt;
use nom::combinator::map;
use nom::multi::{count, many0_count};
use nom::sequence::{pair, preceded};
use nom::IResult;

use std::rc::Rc;

use super::{Environment, Expr, Value};

pub fn modulate(input: &Expr, env: &Environment) -> Result<Vec<u8>, String> {
    modulate_element(input, env).map(BitVec::into_vec)
}

fn modulate_element(input: &Expr, env: &Environment) -> Result<BitVec<Msb0, u8>, String> {
    match input.value() {
        Some(Value::Num(num)) => {
            let mut vec = if num < 0 {
                bitvec![Msb0, u8; 1,0]
            } else {
                bitvec![Msb0, u8; 0,1]
            };
            let num = num.wrapping_abs() as u64;
            let len: usize = if num == 0 {
                0
            } else {
                ((64 - num.leading_zeros() + 3) / 4) as usize
            };
            vec.extend_from_slice(&bitvec![1; len]);
            vec.push(false);
            vec.extend_from_slice(&num.bits::<Msb0>()[64 - len * 4..64]);
            return Ok(vec);
        }
        Some(Value::Nil) => return Ok(bitvec![Msb0, u8; 0, 0]),
        _ => {}
    }

    match input.eval_cons(env) {
        Some((x, xs)) => {
            let mut vec = bitvec![Msb0, u8; 1, 1];
            vec.append(&mut modulate_element(&*x, env)?);
            vec.append(&mut modulate_element(&*xs, env)?);
            Ok(vec)
        }
        None => Err(format!("Cannot modulate value {:?}", input)),
    }
}


pub fn demodulate(input: &[u8]) -> IResult<&[u8], Rc<Expr>> {
    bits(element)(input)
}

fn element(input: (&[u8], usize)) -> IResult<(&[u8], usize), Rc<Expr>> {
    alt((
        preceded(
            tag(0b11, 2usize),
            map(pair(element, element), |(v1, v2)| {
                Expr::new_fn(&[
                    Expr::new(Value::Cons),
                    v1,
                    v2,
                ])
            }),
        ),
        value,
    ))(input)
}

fn value(input: (&[u8], usize)) -> IResult<(&[u8], usize), Rc<Expr>> {
    let (input, s) = sign(input)?;
    if let Sign::Nil = s {
        return Ok((input, Expr::new(Value::Nil)));
    }

    let (input, len) = many0_count(tag(1, 1usize))(input)?;
    let (input, _) = tag(0, 1usize)(input)?;

    let (input, words) = count(take(4usize), len)(input)?;

    let value = words.iter().fold(0, |acc, item| (acc << 4) | item);

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

fn sign(input: (&[u8], usize)) -> IResult<(&[u8], usize), Sign> {
    alt((
        map(tag(0b00, 2usize), |_| Sign::Nil),
        map(tag(0b01, 2usize), |_| Sign::Positive),
        map(tag(0b10, 2usize), |_| Sign::Negative),
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
