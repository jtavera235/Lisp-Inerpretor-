use std::collections::HashMap;
use core::num::ParseFloatError;
use std::fmt;
use std::io::{self, BufRead};
use std::ops::Mul;


fn main() {
    let env = &mut default_env();
    loop {
      println!(">>");
      let expr = slurp_expr();
      match parse_eval(expr, env) {
        Ok(res) => println!("{}", res),
        Err(e) => match e {
          RispErr::Reason(msg) => println!("Error {}", msg),
        },
      }
    }
  }

  #[macro_use]
  macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
      |args: &[RispExp]| -> Result<RispExp, RispErr> {
        let floats = parse_list_of_floats(args)?;
        let first = floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
        let rest = &floats[1..];
        fn f (prev: &f64, xs: &[f64]) -> bool {
          match xs.first() {
            Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
            None => true,
          }
        };
        Ok(RispExp::Bool(f(first, rest)))
      }
    }};
  }


#[derive(Clone)]
enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>), // bam
}


#[derive(Debug)]
enum RispErr {
    Reason(String),
}

#[derive(Clone)]
struct RispEnv {
    data: HashMap<String, RispExp>,
}


// tokenize: String -> Vector of Strings
// converts the passed expression into tokens
fn tokenize(expr: String) -> Vec<String> {
    expr
      .replace("(", " ( ")
      .replace(")", " ) ")
      .split_whitespace()
      .map(|x| x.to_string())
      .collect()
  }

// parse: String -> Result
fn parse<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
    let (token, rest) = tokens.split_first()
      .ok_or(
        RispErr::Reason("could not get token".to_string())
      )?;
    match &token[..] {
      "(" => read_seq(rest),
      ")" => Err(RispErr::Reason("unexpected `)`".to_string())),
      _ => Ok((parse_atom(token), rest)),
    }
  }

  fn read_seq<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
      let mut res: Vec<RispExp> = vec![];
      let mut xs = tokens;
      loop {
          let (next_token, rest) = xs.split_first().ok_or(RispErr::Reason("could not find a closing `)`".to_string()))?;

          if next_token == ")" {
              return Ok((RispExp::List(res), rest))
          }
          let (exp, new_xs) = parse(&xs)?;
          res.push(exp);
          xs = new_xs;
      }
  }

  fn parse_atom(token: &str) -> RispExp {
      match token.as_ref() {
          "true" => RispExp::Bool(true),
          "false" => RispExp::Bool(false),
          _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
              Ok(v) => RispExp::Number(v),
              Err(_) => RispExp::Symbol(token.to_string().clone())
            }
          }
      }      
  }

  #[macro_use]
  fn default_env() -> RispEnv {
      let mut data: HashMap<String, RispExp> = HashMap::new();
      data.insert(
          "+".to_string(),
          RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RispExp::Number(first + sum_of_rest))
              }
          )
      );
      data.insert(
          "-".to_string(),
          RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                  let floats = parse_list_of_floats(args)?;
                  let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                  let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                  Ok(RispExp::Number(first - sum_of_rest))
              }
          )
      );
      data.insert(
          "*".to_string(),
          RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                  let floats = parse_list_of_floats(args)?;
                  let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                  let product = floats[1..].iter().fold(1.0, |product, a| product * a);
                  Ok(RispExp::Number(first * product))
                }
            )
        );
      data.insert(
          "/".to_string(),
        RispExp::Func(
            |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                let quotient = floats[1..].iter().fold(1.0, |q, a| q * a);
                Ok(RispExp::Number(first / quotient))
              }
        )
    );
    data.insert(
        "=".to_string(),
         RispExp::Func(
             |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                let equality = floats[1..].iter().all(|a| a == &first);
                Ok(RispExp::Bool(equality))
             }
         )
        );
        data.insert(
            ">".to_string(), 
            RispExp::Func(ensure_tonicity!(|a, b| a > b))
          );
          data.insert(
            ">=".to_string(), 
            RispExp::Func(ensure_tonicity!(|a, b| a >= b))
          );
          data.insert(
            "<".to_string(), 
            RispExp::Func(ensure_tonicity!(|a, b| a < b))
          );
          data.insert(
            "<=".to_string(), 
            RispExp::Func(ensure_tonicity!(|a, b| a <= b))
          );
      RispEnv {data}
  }

  fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
      args.iter().map(|x| parse_single_float(x)).collect()
  }

  fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
      match exp {
          RispExp::Number(num) => Ok(*num),
          _ => Err(RispErr::Reason("expected a number".to_string())),
      }
  }


  fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
      match exp {
        RispExp::Symbol(k) =>
        env.data.get(k)
        .ok_or(
          RispErr::Reason(
            format!("unexpected symbol k='{}'", k)
          )
        )
        .map(|x| x.clone())
    ,
    RispExp::Number(_a) => Ok(exp.clone()),
    RispExp::Bool(_a) => Ok(exp.clone()),
    RispExp::List(list) => {
      let first_form = list
        .first()
        .ok_or(RispErr::Reason("expected a non-empty list".to_string()))?;
      let arg_forms = &list[1..];
      let first_eval = eval(first_form, env)?;
      match first_eval {
        RispExp::Func(f) => {
          let args_eval = arg_forms
            .iter()
            .map(|x| eval(x, env))
            .collect::<Result<Vec<RispExp>, RispErr>>();
          f(&args_eval?)
        },
        _ => Err(
          RispErr::Reason("expected an opening function call".to_string())
        ),
      }
    },
    RispExp::Func(_) => Err(
      RispErr::Reason("unexpected form".to_string())
    ),
  }
}

  impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      let str = match self {
        RispExp::Symbol(s) => s.clone(),
        RispExp::Number(n) => n.to_string(),
        RispExp::Bool(b) => b.to_string(),
        RispExp::List(list) => {
          let xs: Vec<String> = list
            .iter()
            .map(|x| x.to_string())
            .collect();
          format!("({})", xs.join(","))
        },
        RispExp::Func(_) => "Function {}".to_string(),
      };
      
      write!(f, "{}", str)
    }
  }

  fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
      let (parsed_exp, _) = parse(&tokenize(expr))?;
      let evaled_exp = eval(&parsed_exp, env)?;

      Ok(evaled_exp)
  }

  fn slurp_expr() -> String {
      let mut expr = String::new();
      io::stdin().read_line(&mut expr).expect("Failed to read line");

      expr
  }

  