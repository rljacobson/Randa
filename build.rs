use std::path::Path;
use std::process::Command;


use rust_bison_skeleton::{process_bison_file, BisonErr};

use chrono::Local;


static SPEC_FILE_PATH: &'static str = "src/compiler/parser.y";

fn main() {
    // region Bison parser generation
    // Bison is used to generate `parser.rs` from `parser.y`.

    match process_bison_file(Path::new(SPEC_FILE_PATH)) {
        Ok(_) => {}
        Err(BisonErr { message, .. }) => {
            eprintln!("Bison error:\n{}\nexiting with 1", message);
            std::process::exit(1);
        }
    }

    println!("cargo:rerun-if-changed={}", SPEC_FILE_PATH);
    // endregion

    // region Compile-time Constants
    // Constants generated as compile time. These are used in `Options.rs` and reported via `/v`, `/V`, `-v`,
    // and `-V`.

    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = std::env::var_os("OUT_DIR").unwrap();
    let path = Path::new(&out_dir).join("constants.rs");


    // Don't be confused about the types here. These `String` values are written to a Rust file as `&'static str`
    // values.
    let compiler_platform = {
        let output = Command::new("uname")
            .arg("-sr")
            .output()
            .expect("failed to run uname to get the host platform");
        std::str::from_utf8(&output.stdout)
            .expect("uname returned invalid UTF-8 encoding.").trim().to_string()
    };
    let compiler_arch = {
        let output = Command::new("uname")
            .arg("-m")
            .output()
            .expect("failed to run uname to get the host architecture");
        std::str::from_utf8(&output.stdout)
            .expect("uname returned invalid UTF-8 encoding.").trim().to_string()
    };


    std::fs::write(&path, format!(
        r#"
        // Constants generated as compile time. These are used in `Options.rs` and reported via `/v`, `/V`, `-v`,
        // and `-V`.

        /// Encodes the platform and architecture on which Randa was compiled: "x86_64 Darwin 22.1.0"
        pub static COMPILER_HOST_TARGET: &'static str = "{} {}";
        /// The date on which Randa was compiled: "Dec 11 2022"
        pub static BUILD_DATE: &'static str = "{}";
        "#,
        compiler_arch, compiler_platform,
        Local::now().format("%b %d %Y")
    )).unwrap();
    // endregion
}
