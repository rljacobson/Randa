#![allow(ambiguous_associated_items)]
#![allow(dead_code)]
#![feature(pattern)]

mod bytecode_parser;
mod compiler;
mod constants;
mod data;
mod errors;
mod options;
mod vm;

use crate::errors::{emit_error, BytecodeError};
use crate::vm::VM;
use std::process::ExitCode;

fn run_startup(vm: &mut VM) -> Result<(), BytecodeError> {
    vm.announce();
    println!("\n{}", vm.options);
    vm.run_startup()
}

fn startup_exit_code(result: Result<(), BytecodeError>) -> ExitCode {
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            emit_error(&err);
            ExitCode::from(1)
        }
    }
}

fn main() -> ExitCode {
    let mut vm: VM = VM::new();
    startup_exit_code(run_startup(&mut vm))
}

#[cfg(test)]
mod tests {
    use super::startup_exit_code;
    use crate::errors::BytecodeError;
    use std::process::ExitCode;

    #[test]
    fn startup_exit_code_reports_error_as_failure() {
        println!("The following error message is expected for a passing test.");
        let code = startup_exit_code(Err(BytecodeError::MissingSourceFile {
            path: "missing.m".to_string(),
        }));

        assert_eq!(code, ExitCode::from(1));
    }
}
