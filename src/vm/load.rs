use super::diagnostics::{alfasort, printlist, source_update_check};
use super::*;

impl VM {
    /// Loads the object file for `source_path` if it exists and is modified after `source_path`. Otherwise calls
    /// `load_file` for `source_path`.
    pub(super) fn undump(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        // This does not guarantee that the path ends in ".m" after the if block.
        if !source_path.ends_with(".m") && !self.initializing {
            // Except for prelude, only .m files have dumps.
            // Todo: Then should not there not be a not on initializing?
            return self.load_file(source_path);
        }

        // Change "source.m" into "source.x".
        // The if statement above does not guarantee that the path ends in ".m".
        // Include the dot before the extension to avoid a case like "them" --> "thex".
        let binary_path: String = format!(
            "{}{}",
            source_path.strip_suffix(".m").unwrap_or(source_path),
            ".x"
        );

        // region Check Failure to Load Bytecode
        // If we cannot load the binary file for the source file, then we have to do the entire `loadfile(..)`.
        if let Ok(source_metadata) = std::fs::metadata(source_path) {
            // Source file exists.
            let source_modified_time: SystemTime = source_metadata.modified().unwrap_or_else(
                // If we can't determine the time the source was modified, be conservative and assume it
                // was just modified.
                |_| SystemTime::now(),
            );

            if let Ok(binary_metadata) = std::fs::metadata(source_path) {
                // binary file exists.
                let binary_modified_time = binary_metadata
                    .modified()
                    // If we can't determine the time the binary was modified, be conservative and assume it
                    // was in the 1970s.
                    .unwrap_or(SystemTime::UNIX_EPOCH);

                if binary_modified_time < source_modified_time {
                    // Can't use the binary.
                    return self.load_file(source_path);
                }
            }
        } else {
            // Source file does not exist. Delete binary if it exists.
            std::fs::remove_file(binary_path).ok();
            // Can't use the binary.
            return self.load_file(source_path);
        }

        // Lastly, if we cannot open the binary, we have to do the full `load_file`.
        let in_file: File = match File::open(&binary_path) {
            Ok(f) => f,
            Err(..) => {
                return self.load_file(source_path);
            }
        };

        // endregion

        // You have to `unload` before you can `load`.
        // Todo: Implement unload
        self.unload();

        #[cfg(feature = "debug")]
        if !self.initialising {
            println!("undumping from {}", binary_path);
        }

        // If this is the main script…
        let is_main_script: bool = !self.initializing && !self.making;
        if is_main_script {
            // Todo: Figure out how these signal handlers work.
            //       Probably use https://docs.rs/crossbeam-channel/latest/crossbeam_channel/
            // Can't take interrupt during load_script, so disable them.
            // sigflag = 0,
            // oldsig = signal(SIGINT, (sighandler) sigdefer);
        }

        let load_result = self.load_script(
            &in_file,
            source_path.to_string(),
            ConsList::EMPTY,
            NIL,
            is_main_script,
        );
        drop(in_file); // Close the file.

        // Restore interrupt handler
        if is_main_script {
            // Restore interrupt handler
            // Todo: Figure out how these signal handlers work.
            // (void) signal(SIGINT, oldsig);
        }
        // Todo: Once signal handlers are implemented, see if we actually need sigflag.
        /*
        if sigflag {
          // If a signal fired during loading, we handle it now.
          sigflag = 0;
          (*oldsig)(); // Take deferred interrupt.
        }
        */

        // Handle errors
        let dump_error_encountered: bool = // the result of the following match
    match load_result {

      Ok(files) => { self.files = files; false }

      Err(BytecodeError::ArchitectureMismatch)
      | Err(BytecodeError::WrongBytecodeVersion)
        => {
        std::fs::remove_file(&binary_path).ok();
        self.unload();
        true
      }

      Err(BytecodeError::NameClash) => {
        if self.include_depth == 0 {
          let sorted = alfasort(&mut self.heap, self.clashes);
          println!("Cannot load {} due to name clashes: {}", binary_path, printlist(&self.heap, sorted));
        }
        self.unload();
        self.loading = false;
        return load_result.map(|_| ());
      }

      Err(_bytecode_error) => {
        std::fs::remove_file(&binary_path).ok();
        self.unload();
        // Since we store clashes in the error, there is no equivalent for us. This might change when we figure out GC.
        // CLASHES = self.nill;
        // stackp = dstack;
        true
      }

    }; // end match on load_result

        #[cfg(feature = "debug")]
        if !self.initialising {
            println!(
                "{} undumped, success={}",
                binary_path, dump_error_encountered
            );
        }

        // If any of the files were modified during the dump, reload them.
        if dump_error_encountered || source_update_check(&self.heap, self.files) {
            self.load_file(source_path)?;
        } else if self.initializing {
            // Nested if causes us to break out of outer if block
            if !self.undefined_names.is_empty() || self.files.is_empty() {
                // There is an error in the dump of the prelude.
                // Todo: These errors should be propagated from their source instead of divined from the tea leaves of the flags.
                fatal_error(format!("panic: {} contains errors\n", &binary_path).as_str());
            }
        } else if self.options.verbose
            || self.options.magic
            || !self.options.make_exports.is_empty()
        {
            if self.files.is_empty() {
                println!("{} contains syntax error", source_path);
            } else if !self.undefined_names.is_empty() {
                println!("{} contains undefined names or type errors", source_path);
            } else if !self.making && !self.options.magic {
                println!("{}", source_path);
            }
        }

        if !self.files.is_empty() && !self.making && !self.initializing {
            self.unfix_exports_partial();
        }
        self.loading = false;

        Ok(())
    }

    pub(super) fn load_file(&mut self, source_path: &str) -> Result<(), BytecodeError> {
        #[cfg(test)]
        self.last_load_phase_trace.clear();
        self.record_load_phase("begin");

        // Keep the previous source graph available while replacing current load state.
        self.old_files = self.files;
        self.unload();
        self.reset_load_phase_state();
        self.loading = true;

        // Ensure `loading` is reset on every return path.
        let result = (|| {
            let source_path = normalize_source_path_for_load(self.initializing, source_path);

            let metadata = match std::fs::metadata(&source_path) {
                Ok(metadata) => metadata,
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
                    if self.initializing || self.making {
                        self.record_load_phase("missing-source-error");
                        return Err(BytecodeError::MissingSourceFile {
                            path: source_path.clone(),
                        });
                    }

                    self.record_load_phase("missing-source-allowed");
                    self.files = self.empty_environment_for_source(&source_path, UNIX_EPOCH);
                    self.old_files = self.files;
                    return Ok(());
                }
                Err(err) => {
                    self.record_load_phase("source-metadata-error");
                    return Err(BytecodeError::UnreadableSourceFile {
                        path: source_path.clone(),
                        source: err,
                    });
                }
            };

            let source_file =
                File::open(&source_path).map_err(|source| BytecodeError::UnreadableSourceFile {
                    path: source_path.clone(),
                    source,
                })?;

            // Keep the most recently targeted source graph available if compilation fails.
            let modified_time = metadata.modified().unwrap_or_else(|_| SystemTime::now());
            self.old_files = self.empty_environment_for_source(&source_path, modified_time);

            let is_main_script = !self.initializing && !self.making;
            self.record_load_phase("parse");
            let parse_outcome =
                self.parse_source_script(&source_file, &source_path, is_main_script)?;

            if parse_outcome.status == ParsePhaseStatus::SyntaxError {
                self.record_load_phase("syntax-fallback");
                self.files = parse_outcome.files;
                self.apply_syntax_error_fallback(&source_path)?;
                return Ok(());
            }

            self.files = parse_outcome.files;
            self.record_load_phase("exportfile-checks");
            self.validate_exportfile_bindings_partial()?;
            self.record_load_phase("include-expansion");
            self.run_mkincludes_phase()?;
            self.record_load_phase("typecheck");
            self.run_checktypes_phase()?;
            self.record_load_phase("export-closure");
            self.run_export_closure_phase_partial()?;
            self.record_load_phase("bereaved-warnings");
            self.emit_bereaved_warnings_partial();
            self.record_load_phase("unused-diagnostics");
            self.emit_unused_definition_diagnostics_partial();
            self.record_load_phase("codegen");
            self.run_codegen_phase()?;
            self.record_load_phase("dump-visibility");
            self.run_dump_visibility_phase(&source_path)?;

            self.record_load_phase("success-postlude");
            self.run_success_postlude_phase();
            Ok(())
        })();

        self.loading = false;
        result
    }

    // This is test-only instrumentation that captures load_file phase order so we can verify orchestration
    // branches and sequencing while parser/typecheck/codegen internals are still deferred.
    #[cfg(test)]
    pub(super) fn record_load_phase(&mut self, phase: &'static str) {
        self.last_load_phase_trace.push(phase);
    }

    #[cfg(not(test))]
    pub(super) fn record_load_phase(&mut self, _phase: &'static str) {}

    pub(super) fn reset_load_phase_state(&mut self) {
        // These parse/load accumulators are rebuilt by each load attempt.
        // Reset them eagerly to avoid carrying stale state across retries.
        self.embargoes = NIL;
        self.exportfiles = NIL;
        self.exports = NIL;
        self.eprodnts = NIL;
    }

    pub(super) fn empty_environment_for_source(
        &mut self,
        source_path: &str,
        modified_time: SystemTime,
    ) -> ConsList<FileRecord> {
        let source_record = FileRecord::new(
            &mut self.heap,
            source_path.to_string(),
            modified_time,
            false,
            ConsList::EMPTY,
        );

        ConsList::new(&mut self.heap, source_record)
    }

    pub(super) fn apply_syntax_error_fallback(
        &mut self,
        source_path: &str,
    ) -> Result<(), BytecodeError> {
        if self.initializing {
            return Err(BytecodeError::SyntaxErrorInSource {
                path: source_path.to_string(),
            });
        }

        self.old_files = self.files;
        self.record_load_phase("syntax-unload");
        self.unload();

        self.record_load_phase("syntax-dump-decision");
        if source_path.ends_with(".m") {
            self.maybe_write_syntax_dump(source_path)?;
        }

        self.record_load_phase("syntax-state-reset");
        self.reset_syntax_error_state_for_load();

        Ok(())
    }

    /// Applies success-branch finalization after all load phases succeed.
    ///
    /// C parity target: final success-branch housekeeping before returning from `loadfile`.
    /// This is concrete state update logic, not a deferred boundary.
    pub(super) fn run_success_postlude_phase(&mut self) {
        self.sorted = true;
        self.reset_syntax_error_state_for_load();
        self.old_files = self.files;
    }

    pub(super) fn reset_syntax_error_state_for_load(&mut self) {
        self.error_line = 0;
        self.errs.clear();
    }

    /// Validates `%export` file-id bindings against the currently tracked include set.
    ///
    /// C parity target: `loadfile` exportfiles/pathname checks in `steer.c`.
    /// Current concrete behavior:
    /// - ignores `PLUS` markers (the "include current script" sentinel),
    /// - requires each pathname entry to resolve to exactly one includee,
    /// - returns typed errors for malformed, missing, or ambiguous bindings.
    ///
    /// This implementation is intentionally partial: it covers binding-shape and
    /// include-membership constraints, while parser-driven export semantics remain
    /// deferred to parser/type integration.
    pub(super) fn validate_exportfile_bindings_partial(&mut self) -> Result<(), BytecodeError> {
        if self.exportfiles == NIL {
            return Ok(());
        }

        let mut exportfiles: ConsList<RawValue> = ConsList::from_ref(self.exportfiles.into());
        while let Some(entry) = exportfiles.pop_raw(&self.heap) {
            if entry == Combinator::Plus.into() {
                continue;
            }

            // In this list shape, non-PLUS entries are expected to be heap strings.
            let path = self
                .heap
                .resolve_string(entry.into())
                .map_err(|_| BytecodeError::MalformedExportFileList)?;
            let mut includee_matches = 0usize;
            let mut includees = self.includees;
            while let Some(includee) = includees.pop(&self.heap) {
                if includee.get_file_name(&self.heap) == path {
                    includee_matches += 1;
                }
            }

            if includee_matches == 0 {
                return Err(BytecodeError::ExportFileNotIncludedInScript { path });
            }

            if includee_matches > 1 {
                return Err(BytecodeError::ExportFileAmbiguous { path });
            }
        }

        Ok(())
    }

    /// Applies the include-expansion phase for the current load cycle.
    ///
    /// C parity target: `files=append1(files,mkincludes(includees)),includees=NIL; ld_stuff=NIL;`
    /// from `loadfile`.
    /// Current concrete behavior:
    /// - appends currently tracked `includees` into `files` in list order,
    /// - clears `includees` after append,
    /// - clears `mkinclude_files` bookkeeping for interrupted include loads.
    ///
    /// Real `%include` discovery/compilation remains deferred to parser/typecheck integration.
    pub(super) fn run_mkincludes_phase(&mut self) -> Result<(), BytecodeError> {
        if self.includees.is_empty() {
            self.mkinclude_files = ConsList::EMPTY;
            return Ok(());
        }

        let mut includees = self.includees;
        while let Some(includee) = includees.pop(&self.heap) {
            self.files.append(&mut self.heap, includee);
        }

        self.includees = ConsList::EMPTY;
        self.mkinclude_files = ConsList::EMPTY;
        Ok(())
    }

    /// Executes the typecheck gate for the current load cycle.
    ///
    /// C parity target: `if(!SYNERR) ... checktypes();`
    /// Current concrete behavior:
    /// - if undefined names are already present, fail the phase with a typed error,
    /// - otherwise succeed without invoking the real typechecker subsystem.
    pub(super) fn run_checktypes_phase(&mut self) -> Result<(), BytecodeError> {
        let undefined_count = self.undefined_names.len(&self.heap);
        if undefined_count > 0 {
            return Err(BytecodeError::TypecheckUndefinedNames {
                count: undefined_count,
            });
        }

        Ok(())
    }

    /// Applies export-closure gating after typecheck.
    ///
    /// C parity target: export-list checks gated by `ND`/undefined names.
    /// Current concrete behavior:
    /// - if there is no export list, this phase is a no-op,
    /// - if undefined names are present, drop exports and return a typed blocking error,
    /// - otherwise keep exports unchanged and continue.
    ///
    /// This implementation is intentionally partial: deeper closure/dependency
    /// analysis (for example `deps`-style traversal) remains deferred.
    pub(super) fn run_export_closure_phase_partial(&mut self) -> Result<(), BytecodeError> {
        if self.exports == NIL {
            return Ok(());
        }

        if !self.undefined_names.is_empty() {
            self.exports = NIL;
            return Err(BytecodeError::ExportClosureBlockedByUndefinedNames);
        }

        Ok(())
    }

    /// Emits post-export warnings for potentially bereaved/orphaned type information.
    ///
    /// C parity target: the `bereaved` warning block in `loadfile`.
    /// Current concrete behavior:
    /// - when exports are active and orphan risk was flagged (`unused_types`),
    ///   emit a warning in verbose/make contexts,
    /// - do not fail the load path for warning-only conditions.
    ///
    /// This implementation is intentionally partial: full bereaved analysis depends
    /// on deferred type/export internals.
    pub(super) fn emit_bereaved_warnings_partial(&mut self) {
        if self.exports != NIL && self.unused_types && (self.options.verbose || self.making) {
            println!("warning, export list may be incomplete - missing typenames");
        }
    }

    /// Emits diagnostics for parser-collected unused definitions/nonterminals.
    ///
    /// C parity target: the `detrop` warning block in `loadfile`.
    /// Current concrete behavior:
    /// - if deferred unused-definition diagnostics are present (`eprodnts != NIL`),
    ///   emit a warning in verbose/make contexts,
    /// - clear the deferred diagnostics marker once processed for this load cycle.
    ///
    /// This implementation is intentionally partial: full production of these
    /// diagnostics remains deferred to parser/type integration.
    pub(super) fn emit_unused_definition_diagnostics_partial(&mut self) {
        if self.eprodnts != NIL && (self.options.verbose || self.making) {
            println!("warning, script contains deferred unused-definition diagnostics");
        }

        self.eprodnts = NIL;
    }

    /// Executes the codegen gate across loaded file definitions.
    ///
    /// C parity target: codegen loop and `initialising && ND!=NIL` panic branch.
    /// Current concrete behavior:
    /// - fail if no files are available for code generation,
    /// - fail during initialization if unresolved names remain,
    /// - otherwise succeed without invoking the real codegen subsystem.
    pub(super) fn run_codegen_phase(&mut self) -> Result<(), BytecodeError> {
        if self.files.is_empty() {
            return Err(BytecodeError::CodegenWithoutLoadedFiles);
        }

        if self.initializing && !self.undefined_names.is_empty() {
            return Err(BytecodeError::InitializationLoadContainsErrors);
        }

        Ok(())
    }

    /// Applies dump-writing and export-visibility orchestration for the load cycle.
    ///
    /// C parity target: `fixexports(); makedump(); unfixexports();` on success paths.
    /// Current concrete behavior:
    /// - run phase only for initialization or normal `.m` sources,
    /// - execute `fixexports`/`makedump` F2 boundaries in order,
    /// - execute `unfix_exports_partial` as the current F6 unwind subset.
    ///
    /// This keeps orchestration concrete while deferring full dump serialization
    /// and deep export-visibility rewriting semantics.
    pub(super) fn run_dump_visibility_phase(
        &mut self,
        source_path: &str,
    ) -> Result<(), BytecodeError> {
        // C parity: initialization always writes a dump; otherwise only "normal" scripts
        // (ending in `.m`) run through fixexports/makedump/unfixexports.
        let should_run_dump_visibility = self.initializing || source_path.ends_with(".m");
        if !should_run_dump_visibility {
            return Ok(());
        }

        self.record_load_phase("dump-fixexports");
        self.run_fixexports_boundary();
        self.record_load_phase("dump-write");
        self.run_makedump_boundary(source_path)?;
        self.record_load_phase("dump-unfixexports");
        self.unfix_exports_partial();

        Ok(())
    }

    /// `fixexports` visibility boundary.
    ///
    /// C parity target: `fixexports();` before dump serialization.
    /// Current concrete behavior:
    /// - marks export-list processing mode when exports are present,
    /// - leaves real visibility rewriting (including full internal-name handling)
    ///   deferred.
    ///
    /// This is an F2 deferred boundary that should collapse into direct concrete
    /// visibility logic when export-visibility parity work lands.
    pub(super) fn run_fixexports_boundary(&mut self) {
        if self.exports != NIL {
            self.in_export_list = true;
        }
    }

    /// Deferred success/syntax dump-write boundary.
    ///
    /// C parity target: `makedump();` after visibility fixups.
    /// Current concrete behavior:
    /// - keeps dump write decision concrete in orchestration,
    /// - intentionally preserves `source_path` in the boundary contract for
    ///   future serializer output/diagnostic context,
    /// - defers actual dump serialization to later integration.
    pub(super) fn run_makedump_boundary(
        &mut self,
        _source_path: &str,
    ) -> Result<(), BytecodeError> {
        Ok(())
    }

    /// Captures syntax-dump boundary bookkeeping for syntax-error load exits.
    ///
    /// C parity target: `if(normal(t)&&SYNERR!=2)makedump();` on syntax fallback.
    /// Current concrete behavior:
    /// - only applies to normal `.m` sources,
    /// - ensures `old_files` carries at least one source anchor record so
    ///   syntax-error state remains attributable,
    /// - routes through the same `makedump` boundary used by success-path dump flow,
    ///   while leaving actual dump serialization deferred.
    pub(super) fn maybe_write_syntax_dump(
        &mut self,
        source_path: &str,
    ) -> Result<(), BytecodeError> {
        if !source_path.ends_with(".m") {
            return Ok(());
        }

        if self.old_files.is_empty() {
            self.old_files = self.empty_environment_for_source(source_path, UNIX_EPOCH);
        }

        self.record_load_phase("dump-write");
        self.run_makedump_boundary(source_path)?;

        Ok(())
    }

    /// Parser/openfile load boundary.
    ///
    /// C parity target: parser/openfile segment of `loadfile` (`yyparse`-driven source parse).
    /// Current concrete behavior:
    /// - reads source bytes and creates a placeholder single-file environment,
    /// - recognizes explicit test markers (`RANDA_PARSE_OK`, `RANDA_PARSE_SYNTAX_ERROR`)
    ///   to drive phase orchestration deterministically,
    /// - returns typed deferral error for all non-marker inputs.
    ///
    /// This is an F2 deferred boundary. Full parser invocation, semantic actions,
    /// and `%include`/`%export` side effects remain deferred to parser integration,
    /// where this boundary should be removed or replaced by direct parser wiring.
    pub(super) fn parse_source_script(
        &mut self,
        source_file: &File,
        source_path: &str,
        _is_main_script: bool,
    ) -> Result<ParsePhaseOutcome, BytecodeError> {
        let mut source_text = String::new();
        let mut reader = BufReader::new(source_file);
        reader.read_to_string(&mut source_text).map_err(|source| {
            BytecodeError::UnreadableSourceFile {
                path: source_path.to_string(),
                source,
            }
        })?;

        let placeholder_files = self.empty_environment_for_source(source_path, SystemTime::now());
        if source_text.contains("RANDA_PARSE_OK") {
            return Ok(ParsePhaseOutcome {
                status: ParsePhaseStatus::Parsed,
                files: placeholder_files,
            });
        }

        if source_text.contains("RANDA_PARSE_SYNTAX_ERROR") {
            return Ok(ParsePhaseOutcome {
                status: ParsePhaseStatus::SyntaxError,
                files: placeholder_files,
            });
        }

        Err(BytecodeError::ParserIntegrationDeferred {
            path: source_path.to_string(),
        })
    }

    /// `unfixexports` state restoration used by undump and load-tail paths.
    ///
    /// Current concrete behavior:
    /// - exits export-list processing mode,
    /// - clears deferred `internals` bookkeeping.
    ///
    /// This is intentionally not the full C `unfixexports` algorithm.
    /// Deeper name-restore/publicise semantics remain deferred to export-visibility
    /// parity work.
    pub(super) fn unfix_exports_partial(&mut self) {
        self.in_export_list = false;
        self.internals = ConsList::EMPTY;
    }
}

fn normalize_source_path_for_load(initializing: bool, source_path: &str) -> String {
    let mut normalized_path = source_path.to_string();

    if !normalized_path.ends_with(".m") && !initializing {
        if normalized_path.ends_with(".x") {
            normalized_path.truncate(normalized_path.len() - 2);
        }

        if normalized_path.ends_with('.') {
            normalized_path.pop();
        }

        normalized_path.push_str(".m");
    }

    normalized_path
}
