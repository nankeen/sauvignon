use super::{Environment, EvalError, Evaluator};
use crate::parser::Program;
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Module {
    ImportedModule(Rc<RefCell<Environment>>),
}

impl Module {
    pub fn from_source(evaluator: &mut Evaluator, filename: &str) -> Result<Module, EvalError> {
        let module_path = Path::new(filename).with_extension("sg");
        let source = std::fs::read_to_string(module_path)?;
        let program: Program = source.parse().map_err(EvalError::ModuleParseFailure)?;
        evaluator.eval_program(&program)?;

        Ok(Module::ImportedModule(Rc::clone(&evaluator.env)))
    }
}
