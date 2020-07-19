/// Cyborg Cats Riding Velociraptor Dragons - ICFP Contest 2020

mod modulate;
mod parse;

use std::cell::RefCell;
use std::convert::TryInto;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::str::FromStr;
use std::rc::Rc;
use std::collections::HashMap;

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
        let exprs = parse::parse_lines(fs::read_to_string(path.as_ref())?.lines())
            .unwrap_or_else(|e| panic!("Could not parse input file: {}\n{}", path.as_ref().display(), e));
        eprintln!("Finished parsing");
        let mut env = Environment::default();
        for expr in exprs.into_iter() {
            if let Some(Value::Eq) = expr.oper() {
                let args = expr.args();
                match args[0].value() {
                    Some(Value::Symbol(sym)) => {
                        env.symbols.insert(sym, args[1].clone());
                    }
                    Some(Value::Galaxy) => {
                        env.galaxy = args[1].clone();
                    }
                    _ => {}
                }
            }
        }
        Ok(env)
    }

    pub fn run(&mut self) {
        loop {
            let (x, y): (Value, Value) = (self.loc.0.into(), self.loc.1.into());
            let click = Expr::cons(x.into(), y.into());
            let images = self.interact(click);
            dbg!(images);
            // TODO: get click from user
            break;
        }
    }

    pub fn interact(&mut self, _click: Rc<Expr>) -> Rc<Expr> {
        let expr = Expr::new_apply(self.galaxy.clone(), self.state.clone());
        expr.evaluate(self);
        dbg!(expr);
        // TODO: send interaction to proxy
        Expr::new(Value::Nil)
    }

    pub fn evaluate(&self, args: &Vec<Rc<Expr>>) -> Option<ExprKind> {
        let op = args[0].value()?;
        //dbg!(&op, &args[1..]);
        match op {
            Value::Symbol(sym) => {
                let function = self.symbols.get(&sym).expect("Missing function symbol");
                Some(function.0.borrow().clone())
            }
            op if args.len() == 1 => {
                Some(ExprKind::Evaluated(op))
            }
            Value::Variable(..) => None,
            Value::Eq => {
                let arg0 = args.get(1)?;
                arg0.evaluate(self);
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
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
                arg0.evaluate(self);
                let modulated = modulate::modulate(&*arg0, self);
                Some(ExprKind::Evaluated(Value::Linear(modulated.ok()?)))
            }
            Value::Dem => {
                if let Value::Linear(modulated) = args.get(1)?.value()? {
                    let expr = modulate::demodulate(&modulated)
                        .expect("Could not demodulate")
                        .1;
                    let kind = Some(expr.0.borrow().clone());
                    kind
                } else {
                    None
                }
            }
            Value::Send => unimplemented!(),
            Value::Neg => Some(ExprKind::Evaluated((-args.get(1)?.value()?.num()).into())),
            Value::Ap(fun, arg) => {
                let fun = fun.evaluate(self);
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
                Some(arg0.apply(arg1))
            }
            Value::S => {
                let arg0 = args.get(1)?;
                arg0.evaluate(self);
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
                let arg2 = args.get(3)?;
                arg2.evaluate(self);
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
                arg0.evaluate(self);
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
                let arg2 = args.get(3)?;
                arg2.evaluate(self);
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
                arg0.evaluate(self);
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
                let arg2 = args.get(3)?;
                arg2.evaluate(self);
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
                arg0.evaluate(self);
                args.get(2)?;
                Some(arg0.0.borrow().clone())
            }
            Value::F => {
                args.get(1)?;
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
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
                arg0.evaluate(self);
                Some(arg0.0.borrow().clone())
            }
            Value::Cons => {
                let arg0 = args.get(1)?;
                arg0.evaluate(self);
                let arg1 = args.get(2)?;
                arg1.evaluate(self);
                let arg2 = args.get(3)?;
                arg2.evaluate(self);
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
                arg0.evaluate(self);
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
                arg0.evaluate(self);
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
}

#[derive(Debug, Eq, PartialEq)]
pub struct Expr {
    kind: Value,
    evaluated: RefCell<Option<Value>>,
}

// impl fmt::Debug for Expr {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match &*self.0.borrow() {
//             ExprKind::Evaluated(val) => write!(f, "{:?}", val),
//             ExprKind::Unevaluated{args} => {
//                 f.debug_list().entries(args.iter()).finish()
//             }
//         }
//     }
// }

// #[derive(Clone, Debug, Eq, PartialEq)]
// pub enum ExprKind {
//     Evaluated(Value),
//     Unevaluated {
//         args: Vec<Rc<Expr>>,
//     }
// }

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

    pub fn cons(arg0: Rc<Expr>, arg1: Rc<Expr>) -> Rc<Expr> {
        Self::new_fn(&[
            Self::new(Value::Cons),
            arg0,
            arg1,
        ])
    }

    pub fn evaluate(&self, env: &Environment) -> Option<Value> {
        match &*self.0.borrow() {
            ExprKind::Evaluated(Value::Symbol(s)) => {
                ExprKind::Unevaluated {
                    args: vec![Expr::new(Value::Symbol(*s))],
                }
            }
            ExprKind::Evaluated(val) => return Some(val.clone()),
            ExprKind::Unevaluated { args } => {
                if let Some(res) = env.evaluate(&args) {
                    res
                } else {
                    return None;
                }
            }
        };
        *self.0.borrow_mut() = replacement;
        self.evaluate(env)
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
        match &*self.0.borrow() {
            ExprKind::Evaluated(val) => Some(val.clone()),
            ExprKind::Unevaluated{..} => None,
        }
    }

    fn oper(&self) -> Option<Value> {
        match &*self.0.borrow() {
            ExprKind::Evaluated(val) => Some(val.clone()),
            ExprKind::Unevaluated{args} => args[0].value(),
        }
    }

    fn args(&self) -> Vec<Rc<Expr>> {
        match &*self.0.borrow() {
            ExprKind::Evaluated(_) => vec![],
            ExprKind::Unevaluated{args} => args[1..].to_vec(),
        }
    }

    pub fn eval_cons(&self, env: &Environment) -> Option<(Rc<Expr>, Rc<Expr>)> {
        self.evaluate(env);
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
        assert_eq!(parse::parse(case).unwrap().evaluate(&Environment::default()), Some(Value::T));
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

#[test]
fn read_galaxy() {
    let mut env = Environment::from_file(concat!(env!("CARGO_MANIFEST_DIR"), "/galaxy.txt"))
        .expect("Could not open galaxy.txt");
    assert_eq!(env.symbols.len(), 392);

    env.run();
}
