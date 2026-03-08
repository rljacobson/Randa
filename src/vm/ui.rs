use super::*;

impl VM {
    pub fn announce(&mut self) {
        if let Some(term) = &mut self.terminal {
            term.set_title(format!(
                "Randa {}",
                make_version_string(self.options.version)
            ));

            println_centered(
                &self.terminal,
                format!(
                    "{}   {}   {}",
                    style("T h e").blue(),
                    style("R a n d a").red(),
                    style("S y s t e m").blue()
                )
                .as_str(),
            );
            print!("

"); // two lines
            println_centered(
                &self.terminal,
                format!(
                    "Version {} last revised {}",
                    make_version_string(self.options.version),
                    self.options.build_date
                )
                .as_str(),
            );
            print!("

"); // two lines
            println_centered(
                &self.terminal,
                format!(
                    "{} Copyright 2022-2026 Robert Jacobson, BSD License",
                    style("Randa").red()
                )
                .as_str(),
            );
            println_centered(
                &self.terminal,
                "The Original Miranda System Copyright Research Software Ltd 1985-2020",
            );
            println_centered(&self.terminal, "Original Miranda System: http://miranda.org.uk");
            print!("


"); // three lines

            if self.options.space_limit != DEFAULT_SPACE {
                println!("({} cells)
", self.options.space_limit);
            }
            if !self.options.strict_if {
                println!("(-nostrictif : deprecated!)
");
            }
        }
    }
}

fn println_centered(terminal: &Option<Term>, text: &str) {
    if let Some(term) = terminal {
        let (_terminal_height, terminal_width) = term.size();
        // println!("width: {}", terminal_width);
        let centered = pad_str(text, terminal_width as usize, Alignment::Center, Some("…"));

        term.write_line(&*centered).ok();
    }
}
