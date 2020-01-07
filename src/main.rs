mod parse;
mod env;
mod data;

use std::io::{self};

use data::RispErr;


fn main() {
    let env = &mut env::default_env();
    loop {
      println!(">>");
      let expr = slurp_expr();
      match parse::parse_eval(expr, env) {
        Ok(res) => println!("{}", res),
        Err(e) => match e {
          RispErr::Reason(msg) => println!("Error {}", msg),
        },
      }
    }
  }



  fn slurp_expr() -> String {
      let mut expr = String::new();
      io::stdin().read_line(&mut expr).expect("Failed to read line");

      expr
  }

  