use std::collections::HashMap;
use std::rc::Rc;
use std::fmt;

#[derive(Clone)]
pub enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>), 
    Lambda(RispLambda), 
}


#[derive(Debug)]
pub enum RispErr {
    Reason(String),
}

#[derive(Clone)]
pub struct RispEnv<'a> {
    pub data: HashMap<String, RispExp>,
    pub outer: Option<&'a RispEnv<'a>>,
}

#[derive(Clone)]
pub struct RispLambda {
    pub params_exp: Rc<RispExp>,
    pub body_exp: Rc<RispExp>,
}


impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      let str = match self {
        RispExp::Symbol(s) => s.clone(),
        RispExp::Number(n) => n.to_string(),
        RispExp::Bool(b) => b.to_string(),
        RispExp::Lambda(_) => "Lambda {}".to_string(),
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


