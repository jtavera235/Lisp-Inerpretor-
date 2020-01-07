use crate::data;
use crate::env;

use data::RispEnv;
use data::RispErr;
use data::RispExp;

extern crate rand;
use core::num::ParseFloatError;
use std::rc::Rc;

pub fn parse<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
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

  pub fn read_seq<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
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

pub fn parse_atom(token: &str) -> RispExp {
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

pub fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args.iter().map(|x| parse_single_float(x)).collect()
}

pub fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispErr::Reason("expected a number".to_string())),
    }
}

pub fn parse_list_of_bools(args: &[RispExp]) -> Result<Vec<bool>, RispErr> {
  args.iter().map(|x| parse_single_bool(x)).collect()
}

pub fn parse_single_bool(exp: &RispExp) -> Result<bool, RispErr> {
  match exp {
    RispExp::Bool(b) => Ok(*b),
    _ => Err(RispErr::Reason("expected a boolean".to_string())),
  }
}

pub fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = env::eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}


// tokenize: String -> Vector of Strings
// converts the passed expression into tokens
pub fn tokenize(expr: String) -> Vec<String> {
  expr
    .replace("(", " ( ")
    .replace(")", " ) ")
    .split_whitespace()
    .map(|x| x.to_string())
    .collect()
}



pub fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>, RispErr> {
  let list = match form.as_ref() {
    RispExp::List(s) => Ok(s.clone()),
    _ => Err(RispErr::Reason(
      "expected args form to be a list".to_string(),
    ))
  }?;
  list
    .iter()
    .map(
      |x| {
        match x {
          RispExp::Symbol(s) => Ok(s.clone()),
          _ => Err(RispErr::Reason(
            "expected symbols in the argument list".to_string(),
          ))
        }   
      }
    ).collect()
}




