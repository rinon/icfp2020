#![allow(unused)]
/// Cyborg Cats Riding Velociraptor Dragons - ICFP Contest 2020
mod modulate;
mod parse;

use isahc::prelude::*;
use log::{debug, info, warn};
use nom::error::VerboseError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::rc::Rc;
use std::str::FromStr;
use std::thread;

const API_KEY: &str = "fbfebaf26f4e4f92838c0a3ba5f5d815";

pub enum State {
    Initial,
    Menu,
    Multiplayer,

    // Contains playerKey
    Join(i64),
}

type GalaxyImage = Vec<(i32, i32)>;

impl Environment {
    pub fn set_state(&mut self, state: State) {
        let state_str = match state {
            State::Initial => "nil".to_string(),
            State::Menu => "( 1 , ( 11 ) , 0 )".to_string(),
            State::Multiplayer => {
                "( 5, ( 2 , 0 , nil , nil, nil, nil, nil, t ) , 9, nil )".to_string()
            }
            State::Join(player_key) => {
                // ( ? , ( step, playerKey, nil, nil, nil, nil, nil, t ), ?, nil )
                //
                // failed join:
                // ( ? , ( 3, playerKey, nil, nil, nil, nil, ap ap cons 36 0, t ), ?, nil )
                format!(
                    "( 5, ( 4 , {} , nil , nil, nil, nil, ap ap cons 36 0, t ) , 9, nil )",
                    player_key
                )
            }
        };
        self.state = parse::parse(&state_str).unwrap();
    }

    pub fn start_tutorial(&mut self, number: u32) -> Vec<GalaxyImage> {
        self.state = p(&format!("ap ap cons 6 ap ap cons ap ap cons {} ap ap cons 5 ap ap cons 0 ap ap cons 1 ap ap cons 0 ap ap cons 0 ap ap cons nil ap ap cons nil ap ap cons 4 ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons nil nil ap ap cons 9 ap ap cons nil nil", number));
        let event = self.send(&*p(&format!("( 1, {} )", number)));
        self.interact(event)
    }

    // pub fn join_new_game(&mut self) {
    //     info!("Creating game");
    //     let response = self.send(&*p("( 1 , 0 )"));
    //     let (code, xs) = response.cons_value().unwrap();
    //     assert_eq!(code.num(), 1);
    //     let (players, _nil) = xs.cons_value().unwrap();
    //     let (player1, player2) = players.cons_value().unwrap();
    //     let (player2, _nil) = player2.cons_value().unwrap();

    //     let (id, player1) = player1.cons_value().unwrap();
    //     assert_eq!(id.num(), 0);
    //     let (player1_key, _nil) = player1.cons_value().unwrap();

    //     let (id, player2) = player2.cons_value().unwrap();
    //     assert_eq!(id.num(), 1);
    //     let (player2_key, _nil) = player2.cons_value().unwrap();

    //     self.player_key = Some(player2_key.num());

    //     let join_msg = modulate::modulate(&*p(&format!("( 2 , {} , nil )", player1_key.num())))
    //         .expect("Couldn't modulate message");
    //     let url = self.server_url.clone();
    //     self.set_state(State::Join(self.player_key.unwrap()));
    // }

    pub fn join_game(&mut self) -> Rc<Expr> {
        self.send(&*p(&format!("( 2 , {} , nil )", self.player_key.unwrap())))
    }

    pub fn start_game(&mut self, fuel: i64, gun_heat: i64, cooling: i64, unknown: i64) -> Rc<Expr> {
        self.send(&*p(&format!(
            "( 3 , {} , ( {} , {} , {} , {} ) )",
            self.player_key.unwrap(),
            fuel,
            gun_heat,
            cooling,
            unknown
        )))
    }
}

#[derive(Debug)]
pub enum GameStage {
    NotStarted,
    Started,
    Finished,
}

impl TryFrom<i64> for GameStage {
    type Error = ();
    fn try_from(num: i64) -> Result<GameStage, ()> {
        match num {
            0 => Ok(GameStage::NotStarted),
            1 => Ok(GameStage::Started),
            2 => Ok(GameStage::Finished),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct GameResponse {
    success: bool,
    stage: GameStage,
    static_info: Rc<Expr>,
    state: GameState,
}

#[derive(Debug)]
pub struct GameState {
    tick: u64,
    x1: Rc<Expr>,
    ships: Vec<(Ship, Vec<Command>)>,
}

#[derive(Debug)]
pub enum ShipRole {
    Attacker,
    Defender,
}

impl TryFrom<i64> for ShipRole {
    type Error = ();
    fn try_from(num: i64) -> Result<ShipRole, ()> {
        match num {
            1 => Ok(ShipRole::Attacker),
            2 => Ok(ShipRole::Defender),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Ship {
    role: ShipRole,
    id: u64,
    position: (i32, i32),
    velocity: (i32, i32),
    fuel: i64,
    gun_heat: i64,
    cooling: i64,
    unknown_supply: i64,
    temp: i64,
    x6: Rc<Expr>,
    x7: Rc<Expr>,
}

#[derive(Debug)]
pub enum Command {
    Accelerate {
        ship_id: u64,
        vector: (i32, i32),
    },
    Detonate {
        ship_id: u64,
    },
    Shoot {
        ship_id: u64,
        target: (i32, i32),
        x3: Rc<Expr>,
    },
}

impl Command {
    fn parse(xs: &Rc<Expr>) -> Option<Self> {
        let (id, xs) = xs.cons_value()?;
        let (ship_id, xs) = xs.cons_value()?;
        let ship_id = ship_id.num() as u64;
        match id.num() {
            0 => {
                let (vector, xs) = xs.cons_value()?;
                let (x, y) = vector.cons_value()?;
                Some(Command::Accelerate {
                    ship_id,
                    vector: (x.num() as i32, y.num() as i32),
                })
            }
            1 => Some(Command::Detonate { ship_id }),
            2 => {
                let (vector, x3) = xs.cons_value()?;
                let (x, y) = vector.cons_value()?;
                Some(Command::Shoot {
                    ship_id,
                    target: (x.num() as i32, y.num() as i32),
                    x3,
                })
            }
            _ => None,
        }
    }
}

impl GameResponse {
    pub fn parse(expr: &Rc<Expr>) -> Option<Self> {
        let (success, stage, static_info, game_state) = expr.list_value()?;
        Some(GameResponse {
            success: success.num() == 1,
            stage: stage.num().try_into().ok()?,
            static_info,
            state: GameState::parse(&game_state)?,
        })
    }
}

impl GameState {
    fn parse(expr: &Rc<Expr>) -> Option<Self> {
        let (tick, x1, ships_and_commands) = expr.list_value()?;
        let mut expr = ships_and_commands;
        let mut ships = vec![];
        while expr.value() != Value::Nil {
            let (x, xs) = expr.cons_value().unwrap();
            expr = xs;
            let (ship, mut command_expr) = x.list_value().unwrap();
            let mut commands = vec![];
            while command_expr.value() != Value::Nil {
                let (x, xs) = command_expr.cons_value()?;
                commands.push(Command::parse(&x)?);
                command_expr = xs;
            }
            ships.push((Ship::parse(&ship)?, commands));
        }
        Some(GameState {
            tick: tick.num() as u64,
            x1,
            ships,
        })
    }
}

impl Ship {
    fn parse(expr: &Rc<Expr>) -> Option<Self> {
        let (role, id, position, velocity, supply, temp, x6, x7) = expr.list_value()?;
        let (fuel, gun_heat, cooling, unknown_supply) = supply.list_value()?;
        let position = position.cons_value()?;
        let position = (position.0.num() as i32, position.1.num() as i32);
        let velocity = velocity.cons_value()?;
        let velocity = (velocity.0.num() as i32, velocity.1.num() as i32);
        Some(Ship {
            role: role.num().try_into().ok()?,
            id: id.num() as u64,
            position,
            velocity,
            temp: temp.num(),
            x6,
            x7,
            fuel: fuel.num(),
            gun_heat: gun_heat.num(),
            cooling: cooling.num(),
            unknown_supply: unknown_supply.num(),
        })
    }
}

fn p(s: &str) -> Rc<Expr> {
    parse::parse(s).unwrap()
}

#[derive(Debug)]
pub struct Environment {
    symbols: HashMap<u64, Rc<Expr>>,
    galaxy: Rc<Expr>,
    state: Rc<Expr>,
    loc: (i32, i32),

    server_url: String,
    player_key: Option<i64>,
    debug: bool,
}

impl Default for Environment {
    fn default() -> Environment {
        Environment {
            symbols: HashMap::new(),
            galaxy: Expr::new(Value::Nil),
            state: Expr::new(Value::Nil),
            loc: (0, 0),

            server_url: format!(
                "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey={}",
                API_KEY
            ),
            player_key: None,
            debug: false,
        }
    }
}

impl Environment {
    pub fn from_galaxy() -> io::Result<Self> {
        Environment::from_file(concat!(env!("CARGO_MANIFEST_DIR"), "/galaxy.txt"))
    }

    fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
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

    pub fn make_submission(&mut self, server_url: &str, player_key: i64) {
        self.server_url = format!("{}/aliens/send?playerKey={}", server_url, player_key);
        self.player_key = Some(player_key);
    }

    pub fn dump_state(&self) {
        eprintln!("{:?}", self.state);
    }

    pub fn print_state(&self) {
        eprintln!("{}", self.state);
    }

    pub fn toggle_debug(&mut self) {
        self.debug = !self.debug;
    }

    pub fn increment(&mut self) -> Vec<Vec<(i32, i32)>> {
        let expr = apply(
            apply(self.galaxy.clone(), self.state.clone()),
            cons(Value::Num(0), Value::Num(0)),
        )
        .evaluate(self);
        let (flag, expr) = expr.cons_value().expect("Did not find flag");
        let (newstate, data) = expr.cons_value().expect("Did not find new state");
        let value = apply(
            parse::parse("ap ap b inc ap ap b car ap ap b car cdr").unwrap(),
            newstate.clone(),
        );
        let value = value.evaluate(self);
        eprintln!("{:?}", value);
        // Note: setting this to 3 seems to get me into a new mode
        self.state = cons(
            Value::Num(1),
            cons(
                cons(value, Value::Nil),
                cons(Value::Num(0), cons(Value::Nil, Value::Nil)),
            ),
        );
        if flag.num() != 0 {
            eprintln!("Flag is non-zero!");
        }
        multirender(data)
    }

    pub fn click(&mut self, (x, y): (i32, i32)) -> Vec<Vec<(i32, i32)>> {
        let click = cons(Value::Num(x as i64), Value::Num(y as i64));
        self.interact(click)
    }
    pub fn interact(&mut self, event: Rc<Expr>) -> Vec<Vec<(i32, i32)>> {
        let expr = apply(apply(self.galaxy.clone(), self.state.clone()), event).evaluate(self);
        let (flag, expr) = expr.cons_value().expect("Did not find flag");
        let (newstate, data) = expr.cons_value().expect("Did not find new state");
        let (data, _nil) = data.cons_value().expect("Did not find data");
        self.state = newstate;
        //info!("{}", self.state);
        if flag.num() == 0 {
            multirender(data)
        } else {
            let event = self.send(&*data);
            let response = GameResponse::parse(&event);
            dbg!(response);
            self.interact(event)
        }
    }

    pub fn evaluate(&self, expr: Rc<Expr>) -> Rc<Expr> {
        use Value::*;

        if let Some(evaluated) = &*expr.evaluated.borrow() {
            return evaluated.clone();
        }

        if self.debug {
            expr.dump();
        }

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
                    let modulated = modulate::modulate(&*x.evaluate(self)).unwrap_or_else(|e| {
                        panic!("Couldn't modulate expression: {:?} ({:?})", x, e)
                    });
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
                Neg => Num(-x.evaluate(self).num()).into(),
                Pwr2 => {
                    let x = x.evaluate(self).num();
                    if x == -1 {
                        Num(0).into()
                    } else {
                        Num(2i64.pow(x.try_into().unwrap())).into()
                    }
                }
                I => x.clone(),
                Ap(fun2, y) => match &fun2.evaluate(self).kind {
                    T => y.clone(),
                    F => x.clone(),
                    Add => Value::from(x.evaluate(self).num() + y.evaluate(self).num()).into(),
                    Mul => Value::from(x.evaluate(self).num() * y.evaluate(self).num()).into(),
                    Div => Value::from(y.evaluate(self).num() / x.evaluate(self).num()).into(),
                    Lt => Value::from(y.evaluate(self).num() < x.evaluate(self).num()).into(),
                    Eq => Value::from(x.evaluate(self).num() == y.evaluate(self).num()).into(),
                    Cons => {
                        let res = apply(apply(Cons, y.evaluate(self)), x.evaluate(self));
                        *res.evaluated.borrow_mut() = Some(res.clone());
                        res
                    }
                    Ap(fun3, z) => match fun3.evaluate(self).kind {
                        S => apply(apply(z.clone(), x.clone()), apply(y.clone(), x.clone())),
                        C => apply(apply(z.clone(), x.clone()), y.clone()),
                        B => apply(z.clone(), apply(y.clone(), x.clone())),
                        Cons => apply(apply(x.clone(), z.clone()), y.clone()),
                        _ => expr,
                    },
                    _ => expr,
                },
                _ => {
                    // warn!("Could not evaluate function: {:?} {:?}", fun, x);
                    expr
                }
            },
            _ => expr,
        }
    }

    fn send(&self, data: &Expr) -> Rc<Expr> {
        info!("Sending {}", data);
        let data = modulate::modulate(data).expect("Couldn't modulate message");
        let mut response = isahc::post(&self.server_url, data).expect("Could not send to server");
        let response = response
            .text()
            .expect("Did not receive proper server response");
        debug!("Response {}", response);

        let response = modulate::demodulate(&response)
            .expect("Could not demodulate server response")
            .1;
        info!("Response {}", response);
        response
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value(), f)
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

    fn dump(self: &Rc<Expr>) {
        let mut depth = 0;
        let mut stack = vec![self.clone()];
        while let Some(expr) = stack.pop() {
            match expr.value() {
                Value::Ap(fun, arg) => {
                    depth += 1;
                    eprint!("ap ");
                    stack.push(arg.clone());
                    stack.push(fun.clone());
                }
                v => eprint!("{:?} ", v),
            }
            if depth > 200 {
                eprintln!("...");
                return;
            }
        }
        eprintln!();
    }

    pub fn evaluate(self: &Rc<Expr>, env: &Environment) -> Rc<Expr> {
        if let Some(val) = &*self.evaluated.borrow() {
            return val.clone();
        }
        let mut expr = self.clone();
        loop {
            let res = env.evaluate(expr.clone());
            if Rc::ptr_eq(&expr, &res) {
                *self.evaluated.borrow_mut() = Some(res.clone());
                return res;
            }
            expr = res;
        }
    }

    fn num(&self) -> i64 {
        match self.kind {
            Value::Num(val) => val,
            Value::T => 1,
            Value::F => 0,
            _ => panic!("Could not interpret expr {:?} as a number", self),
        }
    }

    fn value(&self) -> Value {
        if let Some(evaluated) = &*self.evaluated.borrow() {
            evaluated.kind.clone()
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

trait ListValue<T = Value> {
    fn list_value(&self) -> Option<T>;
}

macro_rules! impl_list_value {
    ($($var:ident: $tup_tys:ty),*) => {
        impl ListValue<($($tup_tys),*)> for Rc<Expr> {
            fn list_value(&self) -> Option<($($tup_tys),*)> {
                let xs = self.clone();
                $(
                    let ($var, xs) = xs.cons_value()?;
                )*
                if xs.value() == Value::Nil {
                    Some(($($var),*))
                } else {
                    None
                }
            }
        }
    };
}

impl_list_value!(a: Rc<Expr>);
impl_list_value!(a: Rc<Expr>, b: Rc<Expr>);
impl_list_value!(a: Rc<Expr>, b: Rc<Expr>, c: Rc<Expr>);
impl_list_value!(a: Rc<Expr>, b: Rc<Expr>, c: Rc<Expr>, d: Rc<Expr>);
impl_list_value!(
    a: Rc<Expr>,
    b: Rc<Expr>,
    c: Rc<Expr>,
    d: Rc<Expr>,
    e: Rc<Expr>
);
impl_list_value!(
    a: Rc<Expr>,
    b: Rc<Expr>,
    c: Rc<Expr>,
    d: Rc<Expr>,
    e: Rc<Expr>,
    f: Rc<Expr>
);
impl_list_value!(
    a: Rc<Expr>,
    b: Rc<Expr>,
    c: Rc<Expr>,
    d: Rc<Expr>,
    e: Rc<Expr>,
    f: Rc<Expr>,
    g: Rc<Expr>
);
impl_list_value!(
    a: Rc<Expr>,
    b: Rc<Expr>,
    c: Rc<Expr>,
    d: Rc<Expr>,
    e: Rc<Expr>,
    f: Rc<Expr>,
    g: Rc<Expr>,
    h: Rc<Expr>
);

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
    Linear(String),
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
    // Draw,
    // If0,
    // Interact,
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
            _ => return Err("Could not match value string"),
        };
        Ok(value)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Num(num) => write!(f, "{}", num),
            Linear(s) => write!(f, "[{}]", s),
            Symbol(sym) => write!(f, ":{}", sym),
            Variable(x) => write!(f, "x{}", x),
            Ap(fun, arg) => write!(f, "ap {} {}", fun, arg),
            Galaxy => write!(f, "galaxy"),
            Eq => write!(f, "eq"),
            Inc => write!(f, "inc"),
            Dec => write!(f, "dec"),
            Add => write!(f, "add"),
            Mul => write!(f, "mul"),
            Div => write!(f, "div"),
            Lt => write!(f, "lt"),
            Mod => write!(f, "mod"),
            Dem => write!(f, "dem"),
            Neg => write!(f, "neg"),
            S => write!(f, "s"),
            C => write!(f, "c"),
            B => write!(f, "b"),
            T => write!(f, "t"),
            F => write!(f, "f"),
            Pwr2 => write!(f, "pwr2"),
            I => write!(f, "i"),
            Cons => write!(f, "cons"),
            Car => write!(f, "car"),
            Cdr => write!(f, "cdr"),
            Nil => write!(f, "nil"),
            IsNil => write!(f, "isnil"),
        }
    }
}

fn multirender(mut expr: Rc<Expr>) -> Vec<Vec<(i32, i32)>> {
    let mut images = vec![];
    while let Some((image, xs)) = expr.cons_value() {
        images.push(render(image));
        expr = xs;
    }
    images
}

fn render(mut expr: Rc<Expr>) -> Vec<(i32, i32)> {
    let mut image = vec![];
    while let Some((pixel, xs)) = expr.cons_value() {
        expr = xs;
        let (x, y) = pixel.cons_value().unwrap();
        image.push((x.num() as i32, y.num() as i32));
    }
    image
}

#[cfg(test)]
fn eval_tests(cases: &[&str]) {
    let _ = env_logger::builder().is_test(true).try_init();
    let env = Environment::default();
    for case in cases.into_iter() {
        dbg!(&case);
        let parsed = parse::parse_line(case).unwrap();
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
    let _ = env_logger::builder().is_test(true).try_init();
    let mut env = Environment::from_file(concat!(env!("CARGO_MANIFEST_DIR"), "/galaxy.txt"))
        .expect("Could not open galaxy.txt");
    assert_eq!(env.symbols.len(), 392);

    env.click((0, 0));
}

#[test]
fn scratch() {
    dbg!(parse::parse(
        "( ap ap vec 1 1 , ap ap vec 3 1 , ap ap vec 4 4 )"
    ));

    let _ = env_logger::builder().is_test(true).try_init();
    let mut env = Environment::from_galaxy().expect("Could not open galaxy.txt");

    let expr = parse::parse("ap ap ap c :1141 7 ap ap cons 2 ap ap cons 0 ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons nil ap ap cons t nil")
        .unwrap();
    eprintln!("{:?}", expr.evaluate(&env));
}
