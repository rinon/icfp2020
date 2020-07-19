/// Cyborg Cats Riding Velociraptor Dragons - ICFP Contest 2020
mod modulate;
mod parse;

use nom::error::VerboseError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::rc::Rc;
use std::str::FromStr;

#[derive(Debug)]
pub struct Environment {
    symbols: HashMap<u64, Rc<Expr>>,
    galaxy: Rc<Expr>,
    state: Rc<Expr>,
    loc: (i64, i64),
}

impl Default for Environment {
    fn default() -> Environment {
        Environment {
            symbols: HashMap::new(),
            galaxy: Expr::new(Value::Nil),
            state: Expr::new(Value::Nil),
            loc: (0, 0),
        }
    }
}

impl Environment {
    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let exprs = fs::read_to_string(path.as_ref())?
            .lines()
            .into_iter()
            .map(parse::parse_line)
            .collect::<Result<Vec<_>, nom::Err<VerboseError<&str>>>>()
            .unwrap_or_else(|e| {
                panic!(
                    "Could not parse input file: {}\n{}",
                    path.as_ref().display(),
                    e
                )
            });
        eprintln!("Finished parsing");
        let mut env = Environment::default();
        for (lhs, rhs) in exprs.into_iter() {
            match lhs.kind {
                Value::Symbol(sym) => {
                    env.symbols.insert(sym, rhs);
                }
                Value::Galaxy => {
                    env.galaxy = rhs;
                }
                _ => panic!("Unexpected lhs in file: {:?}", lhs),
            }
        }
        Ok(env)
    }

    pub fn run(&mut self) {
        loop {
            let (x, y): (Value, Value) = (self.loc.0.into(), self.loc.1.into());
            let click = cons(x, y);
            let images = self.interact(click);
            dbg!(images);
            // TODO: get click from user
            break;
        }
    }

    pub fn interact(&mut self, _click: Rc<Expr>) -> Rc<Expr> {
        let expr = apply(self.galaxy.clone(), self.state.clone()).evaluate(self);
        dbg!(expr);
        // TODO: send interaction to proxy
        Expr::new(Value::Nil)
    }

    pub fn evaluate(&self, expr: Rc<Expr>) -> Rc<Expr> {
        use Value::*;

        if let Some(evaluated) = &*expr.evaluated.borrow() {
            return evaluated.clone();
        }

        dbg!(&expr);

        match &expr.kind {
            Symbol(sym) => {
                let function = self.symbols.get(&sym).expect("Missing function symbol");
                function.clone()
            }
            Ap(fun, x) => match &fun.evaluate(self).kind {
                Inc => Value::from(x.evaluate(self).num() + 1).into(),
                Dec => Value::from(x.evaluate(self).num() - 1).into(),
                Nil => Expr::new(T),
                IsNil => apply(x.clone(), apply(T, apply(T, F))),
                Car => apply(x.clone(), T),
                Cdr => apply(x.clone(), F),
                Mod => {
                    let modulated = modulate::modulate(&*x.evaluate(self), self)
                        .unwrap_or_else(|e| panic!("Couldn't modulate expression: {:?} ({:?})", x, e));
                    Linear(modulated).into()
                }
                Dem => {
                    if let Linear(modulated) = x.evaluate(self).value() {
                        modulate::demodulate(&modulated)
                            .expect("Could not demodulate")
                            .1
                    } else {
                        expr
                    }
                }
                Send => unimplemented!(),
                Neg => Num(-x.evaluate(self).num()).into(),
                Pwr2 => {
                    let x = x.evaluate(self).num();
                    if x == -1 {
                        Num(0).into()
                    } else {
                        Num(2i64.pow(x.try_into().unwrap())).into()
                    }
                }
                I => x.evaluate(self),
                Ap(fun2, y) => match &fun2.evaluate(self).kind {
                    T => y.clone(),
                    F => x.clone(),
                    Add => Value::from(x.evaluate(self).num() + y.evaluate(self).num()).into(),
                    Mul => Value::from(x.evaluate(self).num() * y.evaluate(self).num()).into(),
                    Div => Value::from(x.evaluate(self).num() / y.evaluate(self).num()).into(),
                    Lt => Value::from(x.evaluate(self).num() < y.evaluate(self).num()).into(),
                    Eq => Value::from(x.evaluate(self).num() == y.evaluate(self).num()).into(),
                    Cons => apply(apply(Cons, y.evaluate(self)), x.evaluate(self)),
                    Ap(fun3, z) => match fun3.evaluate(self).kind {
                        S => apply(apply(z.clone(), x.clone()), apply(y.clone(), x.clone())),
                        C => apply(apply(z.clone(), x.clone()), y.clone()),
                        B => apply(z.clone(), apply(y.clone(), x.clone())),
                        Cons => apply(apply(x.clone(), z.clone()), y.clone()),
                        _ => expr,
                    },
                    _ => expr,
                },
                _ => expr,
            },
            _ => expr,
        }
    }
}

pub struct Expr {
    kind: Value,
    evaluated: RefCell<Option<Rc<Expr>>>,
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.value(), f)
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl Eq for Expr {}

fn apply<A, B>(fun: A, arg: B) -> Rc<Expr>
where
    A: Into<Rc<Expr>>,
    B: Into<Rc<Expr>>,
{
    Expr::new(Value::Ap(fun.into(), arg.into()))
}

fn cons<A, B>(head: A, tail: B) -> Rc<Expr>
where
    A: Into<Rc<Expr>>,
    B: Into<Rc<Expr>>,
{
    apply(apply(Value::Cons, head), tail)
}

impl Expr {
    pub fn new(value: Value) -> Rc<Self> {
        Rc::new(Expr {
            kind: value,
            evaluated: RefCell::new(None),
        })
    }

    pub fn new_list(list: Vec<Rc<Expr>>) -> Rc<Expr> {
        list.into_iter().rfold(Expr::new(Value::Nil), |list, item| {
            apply(apply(Expr::new(Value::Cons), item), list)
        })
    }

    pub fn evaluate(self: &Rc<Expr>, env: &Environment) -> Rc<Expr> {
        if let Some(val) = &*self.evaluated.borrow() {
            return val.clone();
        }
        let mut expr = self.clone();
        loop {
            let res = env.evaluate(expr.clone());
            if expr == res {
                if *self != res {
                    *self.evaluated.borrow_mut() = Some(res.clone());
                }
                return res;
            }
            expr = res;
        }
    }

    fn num(&self) -> i64 {
        match self.kind {
            Value::Num(val) => val,
            _ => panic!("Could not interpret expr {:?} as a number", self),
        }
    }

    fn value(&self) -> Value {
        if let Some(evaluated) = &*self.evaluated.borrow() {
            evaluated.value()
        } else {
            self.kind.clone()
        }
    }

    pub fn cons_value(&self) -> Option<(Rc<Expr>, Rc<Expr>)> {
        if let Value::Ap(head, xs) = self.value() {
            if let Value::Ap(cons, x) = head.value() {
                if let Value::Cons = cons.value() {
                    return Some((x, xs));
                }
            }
        }
        None
    }
}

impl From<Value> for Rc<Expr> {
    fn from(value: Value) -> Rc<Expr> {
        Expr::new(value)
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
    Linear(Vec<u8>),
    Symbol(u64),
    Variable(u64),
    Ap(Rc<Expr>, Rc<Expr>),
    Galaxy,
    Eq,
    Inc,
    Dec,
    Add,
    Mul,
    Div,
    Lt,
    Mod,
    Dem,
    Send,
    Neg,
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
}

impl FromStr for Value {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Value, &'static str> {
        let value = match input {
            "galaxy" => Value::Galaxy,
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

impl Expr {}

#[cfg(test)]
fn eval_tests(cases: &[&str]) {
    let env = Environment::default();
    for case in cases.into_iter() {
        let parsed = parse::parse_line(case).unwrap();
        dbg!(&parsed);
        assert_eq!(parsed.0.evaluate(&env), parsed.1.evaluate(&env));
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
    eval_tests(&["ap ap ap cons x0 x1 x2   =   ap ap x2 x0 x1"])
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
    eval_tests(&["ap nil x0   =   t"])
}

#[test]
fn msg_29() {
    eval_tests(&["ap isnil nil   =   t", "ap isnil ap ap cons x0 x1   =   f"])
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

#[test]
fn read_galaxy() {
    let mut env = Environment::from_file(concat!(env!("CARGO_MANIFEST_DIR"), "/galaxy.txt"))
        .expect("Could not open galaxy.txt");
    assert_eq!(env.symbols.len(), 392);

    env.run();
}
