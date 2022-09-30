use rust_bison_skeleton::{process_bison_file, BisonErr};
use std::path::Path;

static SPEC_FILE_PATH: &'static str = "src/compiler/parser.y";

fn main() {
    match process_bison_file(Path::new(SPEC_FILE_PATH)) {
        Ok(_) => {}
        Err(BisonErr { message, .. }) => {
            eprintln!("Bison error:\n{}\nexiting with 1", message);
            std::process::exit(1);
        }
    }


    println!("cargo:rerun-if-changed={}", SPEC_FILE_PATH);
}
