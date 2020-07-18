/// Cyborg Cats Riding Velociraptor Dragons - ICFP Contest 2020

mod modulate;
mod parse;

use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt;
use std::{str::FromStr, rc::Rc};

pub use parse::parse;

#[derive(Eq, PartialEq)]
pub struct Expr(RefCell<ExprKind>);

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.0.borrow() {
            ExprKind::Evaluated(val) => write!(f, "{:?}", val),
            ExprKind::Unevaluated{args} => {
                f.debug_list().entries(args.iter()).finish()
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum ExprKind {
    Evaluated(Value),
    Unevaluated {
        args: Vec<Rc<Expr>>,
    }
}

impl Expr {
    pub fn new(value: Value) -> Rc<Self> {
        Rc::new(Expr(RefCell::new(ExprKind::Evaluated(value))))
    }

    pub fn new_fn(args: &[Rc<Expr>]) -> Rc<Self> {
        Rc::new(Expr(RefCell::new(ExprKind::Unevaluated {
            args: args.to_vec(),
        })))
    }

    pub fn new_apply(arg0: Rc<Expr>, arg1: Rc<Expr>) -> Rc<Expr> {
        Self::new_fn(&[
            Self::new(Value::Ap),
            arg0,
            arg1,
        ])
    }

    pub fn new_list(list: Vec<Rc<Expr>>) -> Rc<Expr> {
        list.into_iter().rfold(Expr::new(Value::Nil), |list, item| {
            Expr::new_apply(
                Expr::new_apply(
                    Expr::new(Value::Cons),
                    item,
                ),
                list,
            )
        })
    }

    pub fn evaluate(&self) -> Option<Value> {
        let mut inner = self.0.borrow_mut();
        match &mut *inner {
            ExprKind::Evaluated(val) => Some(val.clone()),
            ExprKind::Unevaluated { args } => {
                if let Some(res) = evaluate(args) {
                    *inner = res;
                    drop(inner);
                    self.evaluate()
                } else {
                    None
                }
            }
        }
    }

    fn apply(&self, arg: &Rc<Expr>) -> ExprKind {
        {
            let inner = self.0.borrow();
            match &*inner {
                ExprKind::Evaluated(val) => {
                    ExprKind::Unevaluated {
                        args: vec![Self::new(val.clone()), arg.clone()],
                    }
                }
                ExprKind::Unevaluated { args } => {
                    let mut args = args.clone();
                    args.push(arg.clone());
                    ExprKind::Unevaluated {
                        args,
                    }
                }
            }
        }
    }

    fn value(&self) -> Option<Value> {
        self.evaluate();
        match &*self.0.borrow() {
            ExprKind::Evaluated(val) => Some(val.clone()),
            ExprKind::Unevaluated{..} => None,
        }
    }

    fn oper(&self) -> Option<Value> {
        self.evaluate();
        match &*self.0.borrow() {
            ExprKind::Evaluated(val) => Some(val.clone()),
            ExprKind::Unevaluated{args} => args[0].value(),
        }
    }

    pub fn cons(&self) -> Option<(Rc<Expr>, Rc<Expr>)> {
        self.evaluate();
        match &*self.0.borrow() {
            ExprKind::Evaluated(..) => None,
            ExprKind::Unevaluated{args} => {
                if args.get(0)?.value()? == Value::Cons {
                    Some((args.get(1)?.clone(), args.get(2)?.clone()))
                } else {
                    None
                }
            }
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Value {
        if value {
            Value::T
        } else {
            Value::F
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Value {
        Value::Num(value)
    }
}

impl From<Option<i64>> for Value {
    fn from(value: Option<i64>) -> Value {
        match value {
            None => Value::Nil,
            Some(num) => Value::Num(num),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Value {
    Num(i64),
    Eq,
    Inc,
    Dec,
    Add,
    Variable(u64),
    Mul,
    Div,
    Lt,
    Mod,
    Dem,
    Send,
    Neg,
    Ap,
    S,
    C,
    B,
    T,
    F,
    Pwr2,
    I,
    Cons,
    Car,
    Cdr,
    Nil,
    IsNil,
    Draw,
    Checkerboard,
    MultipleDraw,
    If0,
    Interact,
    Linear(Vec<u8>),
}

impl FromStr for Value {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Value, &'static str> {
        let value = match input {
            "eq" => Value::Eq,
            "inc" => Value::Inc,
            "dec" => Value::Dec,
            "add" => Value::Add,
            "mul" => Value::Mul,
            "div" => Value::Div,
            "lt" => Value::Lt,
            "mod" => Value::Mod,
            "dem" => Value::Dem,
            "send" => Value::Send,
            "neg" => Value::Neg,
            // Ap has a custom parser
            "s" => Value::S,
            "c" => Value::C,
            "b" => Value::B,
            "t" => Value::T,
            "f" => Value::F,
            "pwr2" => Value::Pwr2,
            "i" => Value::I,
            "cons" => Value::Cons,
            "vec" => Value::Cons,
            "car" => Value::Car,
            "cdr" => Value::Cdr,
            "nil" => Value::Nil,
            "isnil" => Value::IsNil,
            "draw" => Value::Draw,
            "checkerboard" => Value::Checkerboard,
            "multipledraw" => Value::MultipleDraw,
            "if0" => Value::If0,
            "interact" => Value::Interact,
            _ => return Err("Could not match value string"),
        };
        Ok(value)
    }
}

fn evaluate(args: &Vec<Rc<Expr>>) -> Option<ExprKind> {
    dbg!(&args);
    let op = args[0].value()?;
    match op {
        op if args.len() == 1 => {
            Some(ExprKind::Evaluated(op))
        }
        Value::Variable(..) => None,
        Value::Eq => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let arg1 = args.get(2)?;
            arg1.evaluate();
            dbg!("eq", &arg0, &arg1);
            Some(ExprKind::Evaluated((arg0 == arg1).into()))
        }
        Value::Inc => Some(ExprKind::Evaluated((args.get(1)?.value()?.num() + 1).into())),
        Value::Dec => Some(ExprKind::Evaluated((args.get(1)?.value()?.num() - 1).into())),
        Value::Add => Some(ExprKind::Evaluated((args.get(1)?.value()?.num() + args.get(2)?.value()?.num()).into())),
        Value::Mul => Some(ExprKind::Evaluated((args.get(1)?.value()?.num() * args.get(2)?.value()?.num()).into())),
        Value::Div => Some(ExprKind::Evaluated((args.get(1)?.value()?.num() / args.get(2)?.value()?.num()).into())),
        Value::Lt => Some(ExprKind::Evaluated((args.get(1)?.value()?.num() < args.get(2)?.value()?.num()).into())),
        Value::Mod => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let modulated = modulate::modulate(&*arg0);
            dbg!(&modulated);
            Some(ExprKind::Evaluated(Value::Linear(modulated.ok()?)))
        }
        Value::Dem => {
            if let Value::Linear(modulated) = args.get(1)?.value()? {
                let expr = modulate::demodulate(&modulated)
                    .expect("Could not demodulate")
                    .1;
                let kind = Some(expr.0.borrow().clone());
                kind
                // if values.len() == 0 {
                //     Some(ExprKind::Evaluated(values[0].into()))
                // } else {
                //     let values: Vec<_> = values.into_iter().map(|v| {
                //         Expr::new(v.into())
                //     }).collect();
                //     Some(Expr::new_list(values).0.borrow().clone())
                // }
            } else {
                None
            }
        }
        Value::Send => unimplemented!(),
        Value::Neg => Some(ExprKind::Evaluated((-args.get(1)?.value()?.num()).into())),
        Value::Ap => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let arg1 = args.get(2)?;
            arg1.evaluate();
            Some(arg0.apply(arg1))
        }
        Value::S => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let arg1 = args.get(2)?;
            arg1.evaluate();
            let arg2 = args.get(3)?;
            arg2.evaluate();
            Some(ExprKind::Unevaluated {
                args: vec![
                    Expr::new(Value::Ap),
                    Expr::new_apply(arg0.clone(), arg2.clone()),
                    Expr::new_apply(arg1.clone(), arg2.clone()),
                ]
            })
        }
        Value::C => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let arg1 = args.get(2)?;
            arg1.evaluate();
            let arg2 = args.get(3)?;
            arg2.evaluate();
            Some(ExprKind::Unevaluated {
                args: vec![
                    Expr::new(Value::Ap),
                    Expr::new_apply(arg0.clone(), arg2.clone()),
                    arg1.clone(),
                ]
            })
        }
        Value::B => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let arg1 = args.get(2)?;
            arg1.evaluate();
            let arg2 = args.get(3)?;
            arg2.evaluate();
            Some(ExprKind::Unevaluated {
                args: vec![
                    Expr::new(Value::Ap),
                    arg0.clone(),
                    Expr::new_apply(arg1.clone(), arg2.clone()),
                ]
            })
        }
        Value::T => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            args.get(2)?;
            Some(arg0.0.borrow().clone())
        }
        Value::F => {
            args.get(1)?;
            let arg1 = args.get(2)?;
            arg1.evaluate();
            Some(arg1.0.borrow().clone())
        }
        Value::Pwr2 => {
            let arg0 = args.get(1)?.value()?.num();
            if arg0 == -1 {
                Some(ExprKind::Evaluated(0.into()))
            } else {
                Some(ExprKind::Evaluated(2i64.pow(arg0.try_into().unwrap()).into()))
            }
        }
        Value::I => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            Some(arg0.0.borrow().clone())
        }
        Value::Cons => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            let arg1 = args.get(2)?;
            arg1.evaluate();
            let arg2 = args.get(3)?;
            arg2.evaluate();
            Some(ExprKind::Unevaluated {
                args: vec![
                    Expr::new(Value::Ap),
                    Expr::new_apply(arg2.clone(), arg0.clone()),
                    arg1.clone(),
                ]
            })
        }
        Value::Car => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            Some(ExprKind::Unevaluated {
                args: vec![
                    Expr::new(Value::Ap),
                    arg0.clone(),
                    Expr::new(Value::T),
                ]
            })
        }
        Value::Cdr => {
            let arg0 = args.get(1)?;
            arg0.evaluate();
            Some(ExprKind::Unevaluated {
                args: vec![
                    Expr::new(Value::Ap),
                    arg0.clone(),
                    Expr::new(Value::F),
                ]
            })
        }
        Value::Nil => {
            args.get(1)?;
            Some(ExprKind::Evaluated(Value::T))
        }
        Value::IsNil => {
            let arg0 = args.get(1)?;
            Some(ExprKind::Evaluated((arg0.oper()? == Value::Nil).into()))
        }
        _ => unimplemented!("Op {:?} is not yet implemented", op),
    }
}

impl Value {
    fn num(self) -> i64 {
        match self {
            Value::Num(val) => val,
            _ => panic!("Could not interpret value {:?} as a number", self),
        }
    }
}

#[cfg(test)]
fn eval_tests(cases: &[&str]) {
    for case in cases {
        dbg!(case);
        assert_eq!(parse(case).unwrap().1.evaluate(), Some(Value::T));
    }
}

#[test]
fn msg_17() {
    eval_tests(&[
        "ap inc ap inc 0   =   2",
        "ap inc ap inc ap inc 0   =   3",
        // "ap inc ap dec x0   =   x0",
        // "ap dec ap inc x0   =   x0",
        // "ap dec ap ap add x0 1   =   x0",
        "ap ap add ap ap add 2 3 4   =   9",
        "ap ap add 2 ap ap add 3 4   =   9",
        "ap ap add ap ap mul 2 3 4   =   10",
        "ap ap mul 2 ap ap add 3 4   =   14",
        // "inc   =   ap add 1",
        // "dec   =   ap add ap neg 1",
    ]);
}

#[test]
fn msg_18() {
    eval_tests(&[
        "ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2",
        "ap ap ap s add inc 1   =   3",
        "ap ap ap s mul ap add 1 6   =   42",
    ]);
}

#[test]
fn msg_19() {
    eval_tests(&[
        "ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1",
        "ap ap ap c add 1 2   =   3",
    ]);
}

#[test]
fn msg_20() {
    eval_tests(&[
        "ap ap ap b x0 x1 x2   =   ap x0 ap x1 x2",
        // "ap ap ap b inc dec x0   =   x0",
    ]);
}

#[test]
fn msg_21() {
    eval_tests(&[
        "ap ap t x0 x1   =   x0",
        "ap ap t 1 5   =   1",
        "ap ap t t i   =   t",
        "ap ap t t ap inc 5   =   t",
        "ap ap t ap inc 5 t   =   6",
    ]);
}

#[test]
fn msg_23() {
    eval_tests(&[
        "ap pwr2 0   =   ap ap ap s ap ap c ap eq 0 1 ap ap b ap mul 2 ap ap b pwr2 ap add -1 0",
        "ap pwr2 0   =   ap ap ap ap c ap eq 0 1 0 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 0",
        "ap pwr2 0   =   ap ap ap ap eq 0 0 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 0",
        "ap pwr2 0   =   ap ap t 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 0",
        "ap pwr2 0   =   1",
        "ap pwr2 1   =   ap ap ap s ap ap c ap eq 0 1 ap ap b ap mul 2 ap ap b pwr2 ap add -1 1",
        "ap pwr2 1   =   ap ap ap ap c ap eq 0 1 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 1",
        "ap pwr2 1   =   ap ap ap ap eq 0 1 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 1",
        "ap pwr2 1   =   ap ap f 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 1",
        "ap pwr2 1   =   ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap ap ap b pwr2 ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap pwr2 ap ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap ap ap s ap ap c ap eq 0 1 ap ap b ap mul 2 ap ap b pwr2 ap add -1 ap ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap ap ap ap c ap eq 0 1 ap ap add -1 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 ap ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap ap ap ap eq 0 ap ap add -1 1 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 ap ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap ap ap ap eq 0 0 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 ap ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 ap ap t 1 ap ap ap b ap mul 2 ap ap b pwr2 ap add -1 ap ap add -1 1",
        "ap pwr2 1   =   ap ap mul 2 1",
        "ap pwr2 1   =   2",
        "ap pwr2 2   =   ap ap ap s ap ap c ap eq 0 1 ap ap b ap mul 2 ap ap b pwr2 ap add -1 2",

        "ap pwr2 2   =   4",
        "ap pwr2 3   =   8",
        "ap pwr2 4   =   16",
        "ap pwr2 5   =   32",
        "ap pwr2 6   =   64",
        "ap pwr2 7   =   128",
        "ap pwr2 8   =   256",
    ])
}

#[test]
fn msg_24() {
    eval_tests(&[
        "ap i x0   =   x0",
        "ap i 1   =   1",
        "ap i i   =   i",
        "ap i add   =   add",
        "ap i ap add 1   =   ap add 1",
    ])
}

#[test]
fn msg_25() {
    eval_tests(&[
        "ap ap ap cons x0 x1 x2   =   ap ap x2 x0 x1"
    ])
}

#[test]
fn msg_26() {
    eval_tests(&[
        "ap car ap ap cons x0 x1   =   x0",
        "ap car x2   =   ap x2 t",
    ])
}

#[test]
fn msg_27() {
    eval_tests(&[
        "ap cdr ap ap cons x0 x1   =   x1",
        "ap cdr x2   =   ap x2 f",
    ])
}

#[test]
fn msg_28() {
    eval_tests(&[
        "ap nil x0   =   t"
    ])
}

#[test]
fn msg_29() {
    eval_tests(&[
        "ap isnil nil   =   t",
        "ap isnil ap ap cons x0 x1   =   f",
    ])
}

#[test]
fn msg_30() {
    eval_tests(&[
        "( )   =   nil",
        "( x0 )   =   ap ap cons x0 nil",
        "( x0 , x1 )   =   ap ap cons x0 ap ap cons x1 nil",
        "( x0 , x1 , x2 )   =   ap ap cons x0 ap ap cons x1 ap ap cons x2 nil",
        "( x0 , x1 , x2 , x5 )   =   ap ap cons x0 ap ap cons x1 ap ap cons x2 ap ap cons x5 nil",
    ])
}

#[test]
fn msg_35() {
    eval_tests(&[
        "ap mod nil   =   [00]",
        "ap mod ap ap cons nil nil   =   [110000]",
        "ap mod ap ap cons 0 nil   =   [1101000]",
        "ap mod ap ap cons 1 2   =   [110110000101100010]",
        "ap mod ap ap cons 1 ap ap cons 2 nil   =   [1101100001110110001000]",
        "ap mod ( 1 , 2 )   =   [1101100001110110001000]",
        "ap mod ( 1 , ( 2 , 3 ) , 4 )   =   [1101100001111101100010110110001100110110010000]",
    ]);

    eval_tests(&[
        "nil   = ap dem  [00]",
        "ap ap cons nil nil   = ap dem  [110000]",
        "ap ap cons 0 nil   = ap dem  [1101000]",
        "ap ap cons 1 2   = ap dem  [110110000101100010]",
        "ap ap cons 1 ap ap cons 2 nil   = ap dem  [1101100001110110001000]",
        "( 1 , 2 )   = ap dem  [1101100001110110001000]",
        "( 1 , ( 2 , 3 ) , 4 )   = ap dem  [1101100001111101100010110110001100110110010000]",
    ]);
}
