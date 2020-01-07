
use crate::data;
use data::RispEnv;
use data::RispErr;
use data::RispExp;
use data::RispLambda;

use crate::parse;


extern crate rand;
use std::collections::HashMap;
use rand::Rng;
use std::rc::Rc;

#[macro_use]
macro_rules! ensure_tonicity {
  ($check_fn:expr) => {{
    |args: &[RispExp]| -> Result<RispExp, RispErr> {
      let floats = parse::parse_list_of_floats(args)?;
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

pub fn default_env<'a>() -> RispEnv<'a> {
  let mut data: HashMap<String, RispExp> = HashMap::new();
  data.insert(
      "+".to_string(),
      RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
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
              let floats = parse::parse_list_of_floats(args)?;
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
              let floats = parse::parse_list_of_floats(args)?;
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
            let floats = parse::parse_list_of_floats(args)?;
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
            let floats = parse::parse_list_of_floats(args)?;
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
      data.insert(
        "integer?".to_string(),
         RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let integer_check = floats.iter().all(|x| x >= &0.0 || x < &0.0);
            Ok(RispExp::Bool(integer_check))
          }
         )
        );
        data.insert("positive?".to_string(),
         RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let integer_check = floats.iter().all(|x| x > &0.0);
            Ok(RispExp::Bool(integer_check))
          }
         ));
         data.insert("negative?".to_string(),
         RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let integer_check = floats.iter().all(|x| x < &0.0);
            Ok(RispExp::Bool(integer_check))
          }
         ));
         data.insert("zero?".to_string(),
         RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let integer_check = floats.iter().all(|x| x == &0.0);
            Ok(RispExp::Bool(integer_check))
          }
         ));
         data.insert("max".to_string(),
         RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let mut max = first;
            for int in floats.iter() {
              if int >= &max {
                max = *int;
              } 
            }
            Ok(RispExp::Number(max))
          }));
          data.insert("min".to_string(),
          RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let mut min = first;
            for int in floats.iter() {
              if int <= &min {
                min = *int;
              } 
            }
            Ok(RispExp::Number(min))
          }));
          data.insert("even?".to_string(),
          RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let all_even = floats.iter().all(|x| x % 2.0 == 0.0);
            Ok(RispExp::Bool(all_even))
          }));
          data.insert("odd?".to_string(),
          RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let all_even = floats.iter().all(|x| x % 2.0 == 1.0);
            Ok(RispExp::Bool(all_even))
          }));
          data.insert("sqr".to_string(),
          RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let sqr_number: f64;
            if args.len() > 1 {
              return Err(RispErr::Reason("sqr can only have one argument, found more.".to_string()))
            }
            sqr_number = floats[0] * floats[0];
            Ok(RispExp::Number(sqr_number))
          }));
          data.insert("sqrt".to_string(),
          RispExp::Func(
          |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse::parse_list_of_floats(args)?;
            let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
            let sqrt_number: f64;
            if args.len() > 1 {
              return Err(RispErr::Reason("sqrt can only have one argument, found more.".to_string()))
            }
            sqrt_number = floats[0].sqrt();
            Ok(RispExp::Number(sqrt_number))
          }));
          data.insert(
            "%".to_string(),
            RispExp::Func(
                |args: &[RispExp]| -> Result<RispExp, RispErr> {
                    let floats = parse::parse_list_of_floats(args)?;
                    let _first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                    if args.len() != 2 {
                      return Err(RispErr::Reason("% needs two arguments.".to_string()))
                    }
                    let modulo = floats[0] % floats[1];
                    Ok(RispExp::Number(modulo))
                  }
              )
          );
          data.insert(
            "random".to_string(),
            RispExp::Func(
                |args: &[RispExp]| -> Result<RispExp, RispErr> {
                    let floats = parse::parse_list_of_floats(args)?;
                    let first = *floats.first().ok_or(RispErr::Reason("expected at least one number".to_string()))?;
                    if args.len() != 1 {
                      return Err(RispErr::Reason("random needs one argument.".to_string()))
                    }
                    let mut rng = rand::thread_rng();
                    let random_number = rng.gen_range(0.0, first).round();
                    Ok(RispExp::Number(random_number))
                  }
              )
          );
          data.insert(
            "boolean?".to_string(),
            RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let booleans = parse::parse_list_of_bools(args)?;
                let _first = *booleans.first().ok_or(RispErr::Reason("expected at least one boolean".to_string()))?;
                let bool_equality = booleans.iter().all(|x| x == &true || x == &false);
                Ok(RispExp::Bool(bool_equality))
              }
            )
          );
          data.insert(
            "boolean=?".to_string(),
            RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let booleans = parse::parse_list_of_bools(args)?;
                let first = *booleans.first().ok_or(RispErr::Reason("expected at least one boolean".to_string()))?;
                let bool_equality = booleans.iter().all(|x| x == &first);
                Ok(RispExp::Bool(bool_equality))
              }
            )
          );
          data.insert(
            "false?".to_string(),
            RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let booleans = parse::parse_list_of_bools(args)?;
                let _first = *booleans.first().ok_or(RispErr::Reason("expected at least one boolean".to_string()))?;
                let bool_equality = booleans.iter().all(|x| x == &false);
                Ok(RispExp::Bool(bool_equality))
              }
            )
          );
          data.insert(
            "true?".to_string(),
            RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let booleans = parse::parse_list_of_bools(args)?;
                let _first = *booleans.first().ok_or(RispErr::Reason("expected at least one boolean".to_string()))?;
                let bool_equality = booleans.iter().all(|x| x == &true );
                Ok(RispExp::Bool(bool_equality))
              }
            )
          );
          data.insert(
            "not".to_string(),
            RispExp::Func(
              |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let booleans = parse::parse_list_of_bools(args)?;
                let first = *booleans.first().ok_or(RispErr::Reason("expected at least one boolean".to_string()))?;
                if args.len() != 1 {
                  return Err(RispErr::Reason("not needs one argument.".to_string()))
                }
                let bool_opp = !&first;
                Ok(RispExp::Bool(bool_opp))
              }
            )
          );
  RispEnv {data, outer: None}
}



pub fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
  match env.data.get(k) {
    Some(exp) => Some(exp.clone()),
    None => {
      match &env.outer {
        Some(outer_env) => env_get(k, &outer_env),
        None => None
      }
    }
  }
}


pub fn env_for_lambda<'a>(
  params: Rc<RispExp>, 
  arg_forms: &[RispExp],
  outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>, RispErr> {
  let ks = parse::parse_list_of_symbol_strings(params)?;
  if ks.len() != arg_forms.len() {
    return Err(
      RispErr::Reason(
        format!("expected {} arguments, got {}", ks.len(), arg_forms.len())
      )
    );
  }
  let vs = eval_forms(arg_forms, outer_env)?;
  let mut data: HashMap<String, RispExp> = HashMap::new();
  for (k, v) in ks.iter().zip(vs.iter()) {
    data.insert(k.clone(), v.clone());
  }
  Ok(
    RispEnv {
      data,
      outer: Some(outer_env),
    }
  )
}


pub fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
  arg_forms
    .iter()
    .map(|x| eval(x, env))
    .collect()
}

pub fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
  match exp {
    RispExp::Symbol(k) =>
      env_get(k, env)
      .ok_or(
        RispErr::Reason(
          format!("unexpected symbol k='{}'", k)
        )
      )
    ,
    RispExp::Bool(_a) => Ok(exp.clone()),
    RispExp::Number(_a) => Ok(exp.clone()),

    RispExp::List(list) => {
      let first_form = list
        .first()
        .ok_or(RispErr::Reason("expected a non-empty list".to_string()))?;
      let arg_forms = &list[1..];
      match evaluate_built_in(first_form, arg_forms, env) {
        Some(res) => res,
        None => {
          let first_eval = eval(first_form, env)?;
          match first_eval {
            RispExp::Func(f) => {
              f(&eval_forms(arg_forms, env)?)
            },
            RispExp::Lambda(lambda) => {
              let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
              eval(&lambda.body_exp, new_env)
            },
            _ => Err(
              RispErr::Reason("first form must be a function".to_string())
            ),
          }
        }
      }
    },
    RispExp::Func(_) => Err(RispErr::Reason("unexpected form".to_string())),
    RispExp::Lambda(_) => Err(RispErr::Reason("unexpected form".to_string())),
  }
}

pub fn evaluate_built_in(exp: &RispExp, arg_forms: &[RispExp], env: &mut RispEnv) -> Option<Result<RispExp, RispErr>> {
  match exp {
      RispExp::Symbol(s) => 
          match s.as_ref() {
              "if" => Some(eval_if_args(arg_forms, env)),
              "def" => Some(eval_def_args(arg_forms, env)),
              "lam" => Some(eval_lambda_args(arg_forms)),
              _ => None,
          },
          _ => None,
  }
}

pub fn eval_if_args(args_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
  let test_form = args_forms.first().ok_or(
      RispErr::Reason(
          "expected test form".to_string(),
      )
  )?;
  let test_eval = eval(test_form, env)?;
  match test_eval {
      RispExp::Bool(b) => {
          let form_idx = if b { 1 } else { 2 };
          let res_form = args_forms.get(form_idx).ok_or(RispErr::Reason(
              format!("expected form idx={}", form_idx)
          ))?;
          let res_eval = eval(res_form, env);

          res_eval
      },
      _ => Err(
        RispErr::Reason(format!("unexpected test form='{}'", test_form.to_string()))
      )
  }
}

pub fn eval_def_args(args_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
let first_form = args_forms.first().ok_or(
  RispErr::Reason("Expected first form".to_string())
)?;
let first_str = match first_form {
  RispExp::Symbol(s) => Ok(s.clone()),
  _ => Err(RispErr::Reason("Expected a symbol to be first argument".to_string()))
}?;
let second_form = args_forms.get(1).ok_or(
  RispErr::Reason("Expected the second form".to_string())
)?;
if args_forms.len() > 2 {
  return Err(RispErr::Reason("def can only have one argument found more.".to_string()))
}
let second_eval = eval(second_form, env)?;
env.data.insert(first_str, second_eval);
Ok(first_form.clone())
}

pub fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp, RispErr> {
let params_exp = arg_forms.first().ok_or(
  RispErr::Reason(
    "expected args form".to_string(),
  )
)?;
let body_exp = arg_forms.get(1).ok_or(
  RispErr::Reason(
    "expected second form".to_string(),
  )
)?;
if arg_forms.len() > 2 {
  return Err(
    RispErr::Reason(
      "lam definition can only have two forms ".to_string(),
    )
  )
}

Ok(
  RispExp::Lambda(
    RispLambda {
      body_exp: Rc::new(body_exp.clone()),
      params_exp: Rc::new(params_exp.clone()),
    }
  )
)
}
