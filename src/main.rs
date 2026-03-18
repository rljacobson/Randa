#![allow(ambiguous_associated_items)]
#![allow(dead_code)]

mod big_num;
mod bytecode_parser;
mod compiler;
mod constants;
mod data;
mod errors;
mod options;
mod vm;

use crate::errors::{emit_error, StartupLoadError};
use crate::vm::VM;
use std::process::ExitCode;

fn run_startup(vm: &mut VM) -> Result<(), StartupLoadError> {
    vm.announce();
    println!("\n{}", vm.options);
    vm.run_startup()
}

fn startup_exit_code(result: Result<(), StartupLoadError>) -> ExitCode {
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
    use crate::errors::{LoadFileError, SourceInputError, StartupLoadError};
    use std::process::ExitCode;

    #[test]
    fn startup_exit_code_reports_error_as_failure() {
        println!("The following error message is expected for a passing test.");
        let code = startup_exit_code(Err(StartupLoadError::LoadFile(LoadFileError::SourceInput(
            SourceInputError::MissingFile {
                path: "missing.m".to_string(),
            },
        ))));

        assert_eq!(code, ExitCode::from(1));
    }
}
