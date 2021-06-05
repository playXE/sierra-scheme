use crate::{gc::*, *};
pub struct SynPass {
    environments: Gc<ScmVector>,
    source: Gc<ScmVector>,
    functions: Gc<ScmHashtable>,
    lambdas: Gc<ScmVector>,
}

impl SynPass {
    pub fn new(source: Gc<ScmVector>) -> Self {
        Self {
            environments: ScmVector::new(2),
            source,
            functions: ScmHashtable::new(8, None, None),
            lambdas: ScmVector::new(8),
        }
    }

    pub fn transform_all(&mut self) -> Result<Gc<ScmVector>, String> {
        let mut transformed = ScmVector::new(self.source.size);
        for i in 0..self.source.size {
            let atom = self.source[i];
            transformed.push(self.transform(atom)?);
        }
        Ok(transformed)
    }
    pub fn transform(&mut self, atom: Value) -> Result<Value, String> {
        if atom.consp() {
            let cons = atom.cons();
            let car = cons.car;
            let cdr = cons.cdr;

            if car == mk_symbol("define") {
                return self.transform_define(atom, cdr);
            } else if car == mk_symbol("lambda") {
                return self.transform_lambda(atom, cdr);
            } else if car == mk_symbol("defun") {
                return self.transform_defun(atom, cdr);
            }
            return Ok(atom);
        } else {
            Ok(atom)
        }
    }

    fn transform_lambda(&mut self, define: Value, data: Value) -> Result<Value, String> {
        if !data.consp() {
            return Err(format!(
                "expected (lambda <formals> <body>) but found '{:?}'",
                define
            ));
        }
        let data = data.cons();
        if !data.cdr.consp() {
            return Err(format!(
                "expected (lambda <formals> <body>) but found '{:?}'",
                define
            ));
        }
        self.pass_formals(data.car)?;
        if !data.cdr.consp() || data.cdr.cdr() != Value::make_nil() {
            return Err(format!(
                "expected (lambda <formals> <body>) but found '{:?}'",
                define
            ));
        }

        /*Ok(Value::make_cons(
            mk_symbol("lambda"),
            Value::make_cons(data.car, Value::make_cons(body, Value::make_nil())),
        ))*/

        data.cdr.cons().car = self.transform(data.cdr.car())?;
        self.lambdas.push(define);
        Ok(define)
    }

    fn transform_defun(&mut self, defun: Value, data: Value) -> Result<Value, String> {
        if !data.consp() {
            return Err(format!(
                "Expected (defun <variable> <formals> <body>) but found '{:?}'",
                defun
            ));
        }

        let first = data.cons();
        if !first.car.symbolp() {
            return Err(format!("Expected <variable> but found '{:?}'", first.car));
        }
        if !first.cdr.consp() {
            return Err(format!(
                "Expected (defun <variable> <formals> <body>) but found '{:?}'",
                defun
            ));
        }
        let second = first.cdr.cons();
        self.pass_formals(second.car)?;
        if !second.cdr.consp() || second.cdr.cdr() != Value::make_nil() {
            return Err(format!(
                "Expected (defun <variable> <formals> <body>) but found '{:?}'",
                defun
            ));
        }

        second.cdr.cons().car = self.transform(second.cdr.car())?;
        self.functions.insert(first.car, defun);
        Ok(defun)
    }

    fn pass_formals(&mut self, mut formals: Value) -> Result<(), String> {
        while formals.consp() {
            if !formals.car().symbolp() {
                return Err(format!("Expected symbol but found '{:?}'", formals.car()));
            }
            formals = formals.cdr();
        }
        if formals != Value::make_nil() {
            if !formals.symbolp() {
                return Err(format!("Expected symbol but found '{:?}'", formals.car()));
            }
        }
        Ok(())
    }
    fn transform_define(&mut self, define: Value, data: Value) -> Result<Value, String> {
        if !data.consp() {
            return Err(format!(
                "expected (define <variable> <expr>), found '{:?}'",
                data
            ));
        }

        let data = data.cons();
        if !data.car.symbolp() {
            return Err(format!("Expected <variable> but found '{:?}'", data.car));
        }
        if !data.cdr.consp() || data.cdr.cons().cdr != Value::make_nil() {
            return Err(format!(
                "Expected (define <variable> <expression> but found '{:?}'",
                define
            ));
        }
        return Ok(define);
    }
}
