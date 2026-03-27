use randa::errors::{emit_error, StartupLoadError};
use randa::vm::VM;
use std::process::ExitCode;

fn run_startup(vm: &mut VM) -> Result<(), StartupLoadError> {
    vm.announce();
    println!("\n{}", vm.options());
    vm.run_startup()
}

fn main() -> ExitCode {
    let mut vm: VM = VM::from_command_line();
    let result = run_startup(&mut vm);
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            emit_error(&err);
            ExitCode::from(1)
        }
    }
}

#[cfg(test)]
mod tests {
    use randa::errors::{emit_error, LoadFileError, SourceInputError, StartupLoadError};
    use std::process::ExitCode;

    #[test]
    fn startup_exit_code_reports_error_as_failure() {
        println!("The following error message is expected for a passing test.");
        let result = Err(StartupLoadError::LoadFile(LoadFileError::SourceInput(
            SourceInputError::MissingFile {
                path: "missing.m".to_string(),
            },
        )));
        let code = match result {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => {
                emit_error(&err);
                ExitCode::from(1)
            }
        };

        assert_eq!(code, ExitCode::from(1));
    }
}
