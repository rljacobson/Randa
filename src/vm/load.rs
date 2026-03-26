use super::diagnostics::{alfasort, printlist, source_update_check};
use super::*;
use crate::compiler::{
    parser::Parser, HereInfo, Lexer, ParserActivation, ParserConstructorPayload,
    ParserDeferredState, ParserDefinitionPayload, ParserEntryMode, ParserFreeBindingPayload,
    ParserIncludeBindingPayload, ParserIncludeDirectivePayload, ParserIncludeModifierPayload,
    ParserRunDiagnostics, ParserRunResult, ParserSessionState, ParserSpecificationPayload,
    ParserSupportError, ParserTopLevelDirectivePayload, ParserTopLevelScriptPayload,
    ParserTypeDeclarationPayload, ParserVmContext,
};
use crate::compiler::{token::ParserLookahead, Token};
use crate::data::{
    api::{
        AlgebraicConstructorFieldParts, AlgebraicConstructorFieldRef,
        AlgebraicConstructorMetadataParts, AlgebraicConstructorMetadataRef, ApNodeRef, ConsList,
        IdentifierValueTypeKind, TypeExprRef,
    },
    ATOM_LIMIT,
};
use std::io::Write;
use std::path::{Path, PathBuf};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) enum LoadScriptForm {
    Expression,
    TopLevelScript,
    OtherTopLevelForm,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(super) struct CommittedDirectiveState {
    pub(super) exported_identifiers: Value,
    pub(super) export_paths: Value,
    pub(super) export_embargoes: Value,
}

impl Default for CommittedDirectiveState {
    fn default() -> Self {
        Self {
            exported_identifiers: NIL,
            export_paths: NIL,
            export_embargoes: NIL,
        }
    }
}

impl VM {
    /// Loads the object file for `source_path` if it exists and is modified after `source_path`. Otherwise calls
    /// `load_file` for `source_path`.
    pub(super) fn undump(&mut self, source_path: &str) -> Result<(), StartupLoadError> {
        // This does not guarantee that the path ends in ".m" after the if block.
        if !source_path.ends_with(".m") && !self.initializing {
            // Except for prelude, only .m files have dumps.
            // Todo: Then should not there not be a not on initializing?
            return self.load_file(source_path).map_err(Into::into);
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
                    return self.load_file(source_path).map_err(Into::into);
                }
            }
        } else {
            // Source file does not exist. Delete binary if it exists.
            std::fs::remove_file(binary_path).ok();
            // Can't use the binary.
            return self.load_file(source_path).map_err(Into::into);
        }

        // Lastly, if we cannot open the binary, we have to do the full `load_file`.
        let in_file: File = match File::open(&binary_path) {
            Ok(f) => f,
            Err(..) => {
                return self.load_file(source_path).map_err(Into::into);
            }
        };

        // endregion

        // You have to `unload` before you can `load`.
        // Todo: Implement unload
        self.unload();

        #[cfg(feature = "debug")]
        if !self.initializing {
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

      Err(LoadScriptError::Decode(BytecodeDecodeError::ArchitectureMismatch))
      | Err(LoadScriptError::Decode(BytecodeDecodeError::WrongBytecodeVersion))
        => {
        std::fs::remove_file(&binary_path).ok();
        self.unload();
        true
      }

      Err(LoadScriptError::AliasInstall(AliasInstallError::DestinationNameClash)) => {
        if self.include_depth == 0 {
          let sorted = alfasort(&mut self.heap, self.clashes);
          println!("Cannot load {} due to name clashes: {}", binary_path, printlist(&self.heap, sorted));
        }
        self.unload();
        self.loading = false;
        return Err(LoadScriptError::AliasInstall(AliasInstallError::DestinationNameClash).into());
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
        if !self.initializing {
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
            self.unfix_exports();
        }
        self.loading = false;

        Ok(())
    }

    pub(super) fn load_file(&mut self, source_path: &str) -> Result<(), LoadFileError> {
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
                        return Err(SourceInputError::MissingFile {
                            path: source_path.clone(),
                        }
                        .into());
                    }

                    self.record_load_phase("missing-source-allowed");
                    self.files = self.empty_environment_for_source(&source_path, UNIX_EPOCH);
                    self.old_files = self.files;
                    return Ok(());
                }
                Err(err) => {
                    self.record_load_phase("source-metadata-error");
                    return Err(SourceInputError::UnreadableFile {
                        path: source_path.clone(),
                        source: err,
                    }
                    .into());
                }
            };

            let source_file =
                File::open(&source_path).map_err(|source| SourceInputError::UnreadableFile {
                    path: source_path.clone(),
                    source,
                })?;

            // Keep the most recently targeted source graph available if compilation fails.
            let modified_time = metadata.modified().unwrap_or_else(|_| SystemTime::now());
            self.old_files = self.empty_environment_for_source(&source_path, modified_time);

            let is_main_script = !self.initializing && !self.making;
            self.record_load_phase("parse");
            let parse_outcome = self.parse_source_script(
                &source_file,
                &source_path,
                modified_time,
                is_main_script,
            )?;

            if parse_outcome.status == ParsePhaseStatus::SyntaxError {
                self.record_load_phase("syntax-fallback");
                self.files = parse_outcome.files;
                self.apply_syntax_error_fallback(&source_path)?;
                return Ok(());
            }

            let directive_payload = parse_outcome
                .top_level_payload
                .as_ref()
                .map(|payload| &payload.directives);
            self.files = parse_outcome.files;
            self.record_load_phase("exportfile-checks");
            self.validate_exportfile_bindings_partial(directive_payload)?;
            self.record_load_phase("include-expansion");
            let materialized_includees = self.run_mkincludes_phase(directive_payload)?;
            self.record_load_phase("typecheck");
            self.run_checktypes_phase()?;
            self.record_load_phase("export-closure");
            let committed_directives =
                self.run_export_closure_phase_partial(directive_payload, materialized_includees)?;
            self.exported_identifiers = committed_directives.exported_identifiers;
            self.export_paths = committed_directives.export_paths;
            self.export_embargoes = committed_directives.export_embargoes;
            self.included_files = ConsList::EMPTY;
            self.include_rollback_files = ConsList::EMPTY;
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
        self.export_embargoes = NIL;
        self.export_paths = NIL;
        self.exported_identifiers = NIL;
        self.empty_production_nonterminals = NIL;
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
    ) -> Result<(), LoadFileError> {
        if self.initializing {
            return Err(SourceParseError::SyntaxErrorsPresent {
                path: source_path.to_string(),
            }
            .into());
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
        self.error_locations.clear();
        self.parser_diagnostics.clear();
    }

    /// Validates `%export` file-id bindings against the currently tracked include set.
    ///
    /// C parity target: `loadfile` export_paths/pathname checks in `steer.c`.
    /// Current concrete behavior:
    /// - ignores `PLUS` markers (the "include current script" sentinel),
    /// - requires each pathname entry to resolve to exactly one includee,
    /// - returns typed errors for malformed, missing, or ambiguous bindings.
    ///
    /// This implementation is intentionally partial: it covers binding-shape and
    /// include-membership constraints, while parser-driven export semantics remain
    /// deferred to parser/type integration.
    pub(super) fn validate_exportfile_bindings_partial(
        &mut self,
        directive_payload: Option<&ParserTopLevelDirectivePayload>,
    ) -> Result<(), ExportValidationError> {
        let Some(directive_payload) = directive_payload else {
            if self.export_paths == NIL {
                return Ok(());
            }

            let mut export_paths: ConsList<Value> = ConsList::from_ref(self.export_paths.into());
            while let Some(entry) = export_paths.pop_value(&self.heap) {
                if entry == Combinator::Plus.into() {
                    continue;
                }

                let path = self
                    .heap
                    .resolve_string(entry)
                    .map_err(|_| ExportValidationError::MalformedPathList)?;
                let mut includee_matches = 0usize;
                let mut included_files = self.included_files;
                while let Some(includee) = included_files.pop(&self.heap) {
                    if includee.get_file_name(&self.heap) == path {
                        includee_matches += 1;
                    }
                }

                if includee_matches == 0 {
                    return Err(ExportValidationError::PathNotIncludedInScript { path });
                }

                if includee_matches > 1 {
                    return Err(ExportValidationError::AmbiguousPathRequest { path });
                }
            }

            return Ok(());
        };

        let Some(export) = directive_payload.export.as_ref() else {
            return Ok(());
        };

        if export.pathname_requests == NIL.into() {
            return Ok(());
        }

        let mut export_paths: ConsList<Value> = ConsList::from_ref(export.pathname_requests);
        while let Some(entry) = export_paths.pop_value(&self.heap) {
            if entry == Combinator::Plus.into() {
                continue;
            }

            let path = self
                .heap
                .resolve_string(entry)
                .map_err(|_| ExportValidationError::MalformedPathList)?;
            let mut includee_matches = 0usize;
            for include_request in &directive_payload.include_requests {
                let include_path = self
                    .heap
                    .resolve_string(include_request.target_path.into())
                    .map_err(|_| ExportValidationError::MalformedPathList)?;
                if include_path == path {
                    includee_matches += 1;
                }
            }

            if includee_matches == 0 {
                return Err(ExportValidationError::PathNotIncludedInScript { path });
            }

            if includee_matches > 1 {
                return Err(ExportValidationError::AmbiguousPathRequest { path });
            }
        }

        Ok(())
    }

    /// Appends one staged include file graph to `target` in front-first order.
    /// This exists so recursive include compilation keeps one load-owned append seam instead of open-coding list walks at each nesting level.
    /// The invariant is that the relative order of the staged file graph is preserved in the destination list.
    fn append_file_graph(
        &mut self,
        target: &mut ConsList<FileRecord>,
        mut graph: ConsList<FileRecord>,
    ) {
        while let Some(file) = graph.pop(&self.heap) {
            target.append(&mut self.heap, file);
        }
    }

    /// Compiles one parser-fed include request into a staged file graph without authoritative append.
    /// This exists so recursive include compilation can reuse the active load/typecheck/codegen subset for direct and nested include targets without routing through `load_file`.
    /// The invariant is that syntax, binding, nested-include, export-validation, typecheck, and codegen failures all stop before the staged graph is appended to authoritative VM state.
    fn compile_include_request(
        &mut self,
        include_request: &ParserIncludeDirectivePayload,
        outer_free_identifiers: ConsList<FreeFormalBindingRef>,
    ) -> Result<ConsList<FileRecord>, LoadFileError> {
        let saved_free_binding_sets = self.free_binding_sets;
        let saved_unused_types = self.unused_types;

        let result = (|| {
            let path = self
                .heap
                .resolve_string(include_request.target_path.into())
                .map_err(|_| SourceInputError::UnreadableFile {
                    path: "<invalid include path>".to_string(),
                    source: std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "include path is not a heap string",
                    ),
                })?;
            let metadata = std::fs::metadata(&path).map_err(|source| {
                if source.kind() == std::io::ErrorKind::NotFound {
                    SourceInputError::MissingFile { path: path.clone() }
                } else {
                    SourceInputError::UnreadableFile {
                        path: path.clone(),
                        source,
                    }
                }
            })?;
            let modified_time = metadata.modified().unwrap_or_else(|_| SystemTime::now());
            let source_text = std::fs::read_to_string(&path).map_err(|source| {
                SourceInputError::UnreadableFile {
                    path: path.clone(),
                    source,
                }
            })?;
            let parse_outcome = self.parse_source_text(&path, &source_text, modified_time, false)?;
            if parse_outcome.status == ParsePhaseStatus::SyntaxError {
                return Err(IncludeDirectiveError::SyntaxErrorsPresent { path }.into());
            }

            if include_request.bindings.is_empty() {
                self.detritus_parameter_bindings = ConsList::EMPTY;
                self.missing_parameter_bindings = ConsList::EMPTY;
            } else {
                let mut include_formal_bindings: ConsList<FreeFormalBindingRef> = ConsList::EMPTY;
                if let Some(top_level_payload) = parse_outcome.top_level_payload.as_ref() {
                    for free_binding in &top_level_payload.free_bindings {
                        let original_name =
                            IdentifierDefinitionRef::alias_metadata_from_source_identifier(
                                &mut self.heap,
                                free_binding.identifier,
                            );
                        let formal_binding = FreeFormalBindingRef::new(
                            &mut self.heap,
                            free_binding.identifier,
                            original_name,
                            free_binding.type_expr,
                        );
                        include_formal_bindings.push(&mut self.heap, formal_binding);
                    }
                }

                let sorted_formals = super::bytecode::hdsort_binding_list_ref(
                    &mut self.heap,
                    include_formal_bindings.get_ref(),
                );
                let actuals = self.lower_include_binding_actuals(&include_request.bindings);
                self.bindparams(sorted_formals.into(), actuals);
                if !self.detritus_parameter_bindings.is_empty() {
                    return Err(TypecheckError::InvalidFreeBindings {
                        count: self.detritus_parameter_bindings.len(&self.heap),
                    }
                    .into());
                }
                if !self.missing_parameter_bindings.is_empty() {
                    return Err(TypecheckError::MissingFreeBindings {
                        count: self.missing_parameter_bindings.len(&self.heap),
                    }
                    .into());
                }
            }
            let include_detritus_parameter_bindings = self.detritus_parameter_bindings;
            let include_missing_parameter_bindings = self.missing_parameter_bindings;

            let directive_payload = parse_outcome
                .top_level_payload
                .as_ref()
                .map(|payload| &payload.directives);
            self.validate_exportfile_bindings_partial(directive_payload)?;

            let mut nested_includees = ConsList::EMPTY;
            if let Some(directive_payload) = directive_payload {
                for nested_include_request in &directive_payload.include_requests {
                    let nested_graph =
                        self.compile_include_request(nested_include_request, outer_free_identifiers)?;
                    self.append_file_graph(&mut nested_includees, nested_graph);
                }
            }

            self.detritus_parameter_bindings = include_detritus_parameter_bindings;
            self.missing_parameter_bindings = include_missing_parameter_bindings;
            self.free_identifiers = outer_free_identifiers;

            let current_file = parse_outcome.files.head(&self.heap);
            let current_file_definienda = current_file
                .map(|file| file.get_definienda(&self.heap))
                .unwrap_or(ConsList::EMPTY);
            let typecheck_result = super::typecheck::run_partial_typecheck(
                &mut self.heap,
                super::typecheck::TypecheckBoundaryInputs {
                    current_file,
                    detritus_parameter_bindings: include_detritus_parameter_bindings,
                    missing_parameter_bindings: include_missing_parameter_bindings,
                },
            );
            if let Some(failure) = typecheck_result.failure {
                return Err(failure.into());
            }

            self.run_export_closure_phase_partial_for_definienda(
                directive_payload,
                current_file_definienda,
                nested_includees,
            )?;

            let mut compiled_include_graph = parse_outcome.files;
            self.append_file_graph(&mut compiled_include_graph, nested_includees);
            let codegen_result = super::codegen::run_partial_codegen(
                &mut self.heap,
                super::codegen::CodegenBoundaryInputs {
                    files: compiled_include_graph,
                    current_file,
                    initializing: self.initializing,
                    undefined_names: typecheck_result.undefined_names,
                },
            );
            if let Some(failure) = codegen_result.failure {
                return Err(failure.into());
            }

            if let Some(current_file) = current_file {
                let mut definienda = current_file.get_definienda(&self.heap);
                let mut rewritten_definienda: Vec<IdentifierRecordRef> = Vec::new();
                while let Some(definiendum) = definienda.pop(&self.heap) {
                    rewritten_definienda.push(definiendum);
                }

                for modifier in &include_request.modifiers {
                    match modifier {
                        ParserIncludeModifierPayload::Suppress { identifier } => {
                            let suppressed_name = identifier.get_name(&self.heap);
                            let matching_index = rewritten_definienda.iter().position(|definiendum| {
                                definiendum.get_name(&self.heap) == suppressed_name
                            });
                            let Some(matching_index) = matching_index else {
                                return Err(IncludeDirectiveError::ModifierTargetNotFound {
                                    name: suppressed_name,
                                }
                                .into());
                            };
                            rewritten_definienda.remove(matching_index);
                        }
                        ParserIncludeModifierPayload::Rename {
                            source,
                            destination,
                        } => {
                            let original_name = source.get_name(&self.heap);
                            let renamed_name = destination.get_name(&self.heap);
                            let matching_index = rewritten_definienda.iter().position(|definiendum| {
                                definiendum.get_name(&self.heap) == original_name
                            });
                            let Some(matching_index) = matching_index else {
                                return Err(IncludeDirectiveError::ModifierTargetNotFound {
                                    name: original_name,
                                }
                                .into());
                            };

                            let original_identifier = rewritten_definienda[matching_index];
                            let original_value_field = original_identifier.get_value_field(&self.heap);
                            let original_value_raw: RawValue = original_value_field.into();

                            if rewritten_definienda.iter().enumerate().any(|(index, definiendum)| {
                                index != matching_index
                                    && definiendum.get_name(&self.heap) == renamed_name
                            }) {
                                return Err(IncludeDirectiveError::RenameDestinationClash {
                                    name: renamed_name,
                                }
                                .into());
                            }

                            let renamed_identifier = self.intern_identifier(renamed_name.as_str());
                            let original_definition = original_identifier.get_definition(&self.heap);
                            let original_datatype = original_identifier.get_datatype(&self.heap);
                            renamed_identifier.set_definition(&mut self.heap, original_definition);
                            renamed_identifier.set_datatype(&mut self.heap, original_datatype);
                            if let Some(original_value) = original_identifier.get_value(&self.heap) {
                                match original_value.get_data(&self.heap) {
                                    IdentifierValueData::Arbitrary(value)
                                        if original_value_raw >= ATOM_LIMIT
                                            && self.heap[original_value_raw].tag == Tag::Constructor =>
                                    {
                                        let constructor_value = ConstructorRef::from_ref(value.into());
                                        let constructor_index =
                                            constructor_value.constructor_index(&self.heap);
                                        let renamed_constructor_value = ConstructorRef::new(
                                            &mut self.heap,
                                            constructor_index,
                                            renamed_identifier.into(),
                                        );
                                        renamed_identifier.set_value_from_data(
                                            &mut self.heap,
                                            IdentifierValueData::Arbitrary(renamed_constructor_value.into()),
                                        );

                                        for definiendum in rewritten_definienda.iter() {
                                            let Some(type_value) = definiendum.get_value(&self.heap) else {
                                                continue;
                                            };
                                            let IdentifierValueData::Typed {
                                                arity,
                                                show_function,
                                                value_type,
                                            } = type_value.get_data(&self.heap) else {
                                                continue;
                                            };
                                            if value_type.get_identifier_value_type_kind(&self.heap)
                                                != IdentifierValueTypeKind::Algebraic
                                            {
                                                continue;
                                            }

                                            let Some(mut constructors) =
                                                value_type.algebraic_constructor_metadata(&self.heap)
                                            else {
                                                continue;
                                            };
                                            let mut constructor_entries = Vec::new();
                                            let mut changed = false;
                                            while let Some(metadata) = constructors.pop(&self.heap) {
                                                let mut parts = metadata.get_data(&self.heap);
                                                if parts.constructor == original_identifier {
                                                    parts.constructor = renamed_identifier;
                                                    changed = true;
                                                }
                                                constructor_entries.push(parts);
                                            }
                                            if !changed {
                                                continue;
                                            }

                                            let rebuilt_metadata = constructor_entries.iter().rev().fold(
                                                NIL,
                                                |metadata_list, entry| {
                                                    let metadata_ref = AlgebraicConstructorMetadataRef::new(
                                                        &mut self.heap,
                                                        AlgebraicConstructorMetadataParts {
                                                            constructor: entry.constructor,
                                                            arity: entry.arity,
                                                            fields: entry.fields,
                                                        },
                                                    );
                                                    self.heap.cons_ref(metadata_ref.into(), metadata_list)
                                                },
                                            );
                                            let rebuilt_value = IdentifierValueRef::from_type_identifier_parts(
                                                &mut self.heap,
                                                TypeIdentifierValueParts {
                                                    arity,
                                                    show_function: (show_function != Value::None)
                                                        .then_some(show_function),
                                                    kind: IdentifierValueTypeKind::Algebraic,
                                                    info: rebuilt_metadata,
                                                },
                                            );
                                            definiendum.set_value(&mut self.heap, rebuilt_value);
                                        }
                                    }
                                    data => renamed_identifier.set_value_from_data(&mut self.heap, data),
                                }
                            }
                            rewritten_definienda[matching_index] = renamed_identifier;
                        }
                    }
                }

                current_file.clear_definienda(&mut self.heap);
                for definiendum in rewritten_definienda.into_iter().rev() {
                    current_file.push_item_onto_definienda(&mut self.heap, definiendum);
                }
            }

            Ok(compiled_include_graph)
        })();

        self.free_identifiers = outer_free_identifiers;
        if result.is_err() {
            self.free_binding_sets = saved_free_binding_sets;
        }
        self.unused_types = saved_unused_types;

        result
    }

    /// Applies the include-expansion phase for the current load cycle.
    ///
    /// C parity target: `files=append1(files,mkincludes(included_files)),included_files=NIL; ld_stuff=NIL;`
    /// from `loadfile`.
    /// Current concrete behavior:
    /// - appends currently tracked `included_files` into `files` in list order,
    /// - for parser-fed include requests, compiles include source into staged file graphs through the active parse/typecheck/codegen subset,
    /// - applies the active rename/suppress modifier subset to the direct include target after nested include compilation,
    /// - commits includees only after the whole request set succeeds,
    /// - clears `included_files` after append,
    /// - clears `include_rollback_files` bookkeeping for interrupted include loads.
    ///
    /// Fuller include-sharing/typeclash parity and deeper imported-definition semantics remain deferred.
    pub(super) fn run_mkincludes_phase(
        &mut self,
        directive_payload: Option<&ParserTopLevelDirectivePayload>,
    ) -> Result<ConsList<FileRecord>, LoadFileError> {
        let Some(directive_payload) = directive_payload else {
            if self.included_files.is_empty() {
                self.include_rollback_files = ConsList::EMPTY;
                return Ok(ConsList::EMPTY);
            }

            let materialized_includees = self.included_files;
            let mut included_files = self.included_files;
            while let Some(includee) = included_files.pop(&self.heap) {
                self.files.append(&mut self.heap, includee);
            }

            self.included_files = ConsList::EMPTY;
            self.include_rollback_files = ConsList::EMPTY;
            return Ok(materialized_includees);
        };

        if directive_payload.include_requests.is_empty() {
            self.include_rollback_files = ConsList::EMPTY;
            return Ok(ConsList::EMPTY);
        }

        let mut materialized_includees = ConsList::EMPTY;
        let outer_free_identifiers = self.free_identifiers;
        for include_request in &directive_payload.include_requests {
            let compiled_include_graph =
                self.compile_include_request(include_request, outer_free_identifiers)?;
            self.append_file_graph(&mut materialized_includees, compiled_include_graph);
        }

        let mut includees_to_append = materialized_includees;
        while let Some(includee) = includees_to_append.pop(&self.heap) {
            self.files.append(&mut self.heap, includee);
        }

        self.include_rollback_files = ConsList::EMPTY;
        Ok(materialized_includees)
    }

    /// Executes the typecheck gate for the current load cycle.
    ///
    /// C parity target: `if(!SYNERR) ... checktypes();`
    /// Current concrete behavior:
    /// - delegate to the subsystem-owned partial typecheck boundary,
    /// - commit subsystem-produced unresolved-name state,
    /// - propagate typed subsystem failure when present.
    pub(super) fn run_checktypes_phase(&mut self) -> Result<(), TypecheckError> {
        let preexisting_undefined_count = self.undefined_names.len(&self.heap);
        if preexisting_undefined_count > 0 {
            return Err(TypecheckError::UndefinedNames {
                count: preexisting_undefined_count,
            });
        }

        let inputs = super::typecheck::TypecheckBoundaryInputs::from_vm(self);
        let result = super::typecheck::run_partial_typecheck(&mut self.heap, inputs);
        self.undefined_names = result.undefined_names;

        if let Some(failure) = result.failure {
            return Err(failure);
        }

        Ok(())
    }

    /// Walks one active committed type-expression subtree and inserts referenced type identifiers into `dependencies`.
    /// This exists so export closure uses one load-owned recursive dependency walk across datatype, synonym, algebraic-field, and abstract-basis type payloads.
    /// The invariant is that only identifier roots whose datatype is `type` are inserted, while the existing active application/wrapper recursion shapes remain unchanged.
    fn collect_export_type_dependencies_from_type_expr(
        &mut self,
        type_expr: TypeExprRef,
        dependencies: &mut ConsList<IdentifierRecordRef>,
    ) {
        let raw_reference: RawValue = type_expr.value().into();
        if raw_reference < ATOM_LIMIT {
            return;
        }

        match self.heap[raw_reference].tag {
            Tag::Id => {
                let identifier = IdentifierRecordRef::from_ref(raw_reference);
                if identifier.get_type_expr(&self.heap).is_builtin_type(Type::Type) {
                    dependencies.insert_ordered(&mut self.heap, identifier);
                }
            }
            Tag::Ap => {
                let application = ApNodeRef::from_ref(raw_reference);
                self.collect_export_type_dependencies_from_type_expr(
                    TypeExprRef::new(application.function_raw(&self.heap).into()),
                    dependencies,
                );
                self.collect_export_type_dependencies_from_type_expr(
                    TypeExprRef::new(application.argument_raw(&self.heap).into()),
                    dependencies,
                );
            }
            Tag::Cons | Tag::Pair | Tag::TCons => {
                self.collect_export_type_dependencies_from_type_expr(
                    TypeExprRef::new(self.heap[raw_reference].head.into()),
                    dependencies,
                );
                self.collect_export_type_dependencies_from_type_expr(
                    TypeExprRef::new(self.heap[raw_reference].tail.into()),
                    dependencies,
                );
            }
            Tag::Label => {
                self.collect_export_type_dependencies_from_type_expr(
                    TypeExprRef::new(self.heap[raw_reference].tail.into()),
                    dependencies,
                );
            }
            Tag::Share => {
                self.collect_export_type_dependencies_from_type_expr(
                    TypeExprRef::new(self.heap[raw_reference].head.into()),
                    dependencies,
                );
            }
            _ => {}
        }
    }

    /// Applies export-closure gating after typecheck.
    ///
    /// C parity target: export-list checks gated by `ND`/undefined names.
    /// Current concrete behavior:
    /// - if there is no export list, this phase is a no-op,
    /// - if undefined names are present, drop exported_identifiers and return a typed blocking error,
    /// - otherwise validate explicit export ids against committed definienda, then keep one canonical export set and continue.
    ///
    /// This implementation is intentionally partial: deeper closure/dependency
    /// analysis (for example `deps`-style traversal) remains deferred.
    pub(super) fn run_export_closure_phase_partial(
        &mut self,
        directive_payload: Option<&ParserTopLevelDirectivePayload>,
        materialized_includees: ConsList<FileRecord>,
    ) -> Result<CommittedDirectiveState, ExportValidationError> {
        let current_file_definienda = self
            .files
            .head(&self.heap)
            .map(|file| file.get_definienda(&self.heap))
            .unwrap_or(ConsList::EMPTY);
        self.run_export_closure_phase_partial_for_definienda(
            directive_payload,
            current_file_definienda,
            materialized_includees,
        )
    }

    /// Applies export-closure gating over one staged current-file definienda set.
    /// This exists so recursive include compilation can reuse the active export-validation owner without rebinding authoritative VM file state.
    /// The invariant is that explicit export-id validation and type-driven closure read only the supplied current-file definienda plus the staged include graph.
    fn run_export_closure_phase_partial_for_definienda(
        &mut self,
        directive_payload: Option<&ParserTopLevelDirectivePayload>,
        current_file_definienda: ConsList<IdentifierRecordRef>,
        materialized_includees: ConsList<FileRecord>,
    ) -> Result<CommittedDirectiveState, ExportValidationError> {
        let Some(directive_payload) = directive_payload else {
            if self.exported_identifiers == NIL {
                return Ok(CommittedDirectiveState::default());
            }

            if !self.undefined_names.is_empty() {
                self.exported_identifiers = NIL;
                return Err(ExportValidationError::BlockedByUndefinedNames);
            }

            return Ok(CommittedDirectiveState {
                exported_identifiers: self.exported_identifiers,
                export_paths: self.export_paths,
                export_embargoes: self.export_embargoes,
            });
        };

        let Some(export) = directive_payload.export.as_ref() else {
            return Ok(CommittedDirectiveState::default());
        };

        if !self.undefined_names.is_empty() {
            return Err(ExportValidationError::BlockedByUndefinedNames);
        }
        let mut exportable_definienda = ConsList::EMPTY;
        let mut current_exportable = current_file_definienda;
        while let Some(definiendum) = current_exportable.pop(&self.heap) {
            exportable_definienda.insert_ordered(&mut self.heap, definiendum);
        }
        let mut includees = materialized_includees;
        while let Some(includee) = includees.pop(&self.heap) {
            let mut definienda = includee.get_definienda(&self.heap);
            while let Some(definiendum) = definienda.pop(&self.heap) {
                exportable_definienda.insert_ordered(&mut self.heap, definiendum);
            }
        }

        let mut committed_exports = NIL;
        let mut explicit_exports: ConsList<IdentifierRecordRef> = ConsList::from_ref(export.exported_ids);
        while let Some(export_id) = explicit_exports.pop(&self.heap) {
            if !exportable_definienda.contains(&self.heap, export_id) {
                return Err(ExportValidationError::UndefinedExportedIdentifier {
                    name: export_id.get_name(&self.heap),
                });
            }
            committed_exports = ConsList::<Value>::insert_ordered_value(
                &mut self.heap,
                committed_exports,
                export_id.into(),
            );
        }

        let mut export_paths: ConsList<Value> = ConsList::from_ref(export.pathname_requests);
        while let Some(entry) = export_paths.pop_value(&self.heap) {
            if entry == Combinator::Plus.into() {
                let mut current_exports = current_file_definienda;
                while let Some(definiendum) = current_exports.pop(&self.heap) {
                    committed_exports = ConsList::<Value>::insert_ordered_value(
                        &mut self.heap,
                        committed_exports,
                        definiendum.into(),
                    );
                }
                continue;
            }

            let path = self
                .heap
                .resolve_string(entry)
                .map_err(|_| ExportValidationError::MalformedPathList)?;
            let mut included_files = materialized_includees;
            while let Some(includee) = included_files.pop(&self.heap) {
                if includee.get_file_name(&self.heap) != path {
                    continue;
                }

                let mut definienda = includee.get_definienda(&self.heap);
                while let Some(definiendum) = definienda.pop_value(&self.heap) {
                    committed_exports = ConsList::<Value>::insert_ordered_value(
                        &mut self.heap,
                        committed_exports,
                        definiendum,
                    );
                }
            }
        }

        let mut closure_queue: ConsList<IdentifierRecordRef> =
            ConsList::from_ref(committed_exports.into());
        while let Some(export_id) = closure_queue.pop(&self.heap) {
            let mut dependencies = ConsList::EMPTY;
            let datatype = export_id.get_type_expr(&self.heap);
            if !datatype.is_builtin_type(Type::Undefined) {
                self.collect_export_type_dependencies_from_type_expr(datatype, &mut dependencies);
            }

            if let Some(value) = export_id.get_value(&self.heap) {
                if let IdentifierValueData::Typed { value_type, .. } = value.get_data(&self.heap) {
                    match value_type.get_identifier_value_type_kind(&self.heap) {
                        IdentifierValueTypeKind::Synonym => {
                            self.collect_export_type_dependencies_from_type_expr(
                                value_type.synonym_rhs_type_expr(&self.heap),
                                &mut dependencies,
                            );
                        }
                        IdentifierValueTypeKind::Algebraic => {
                            if let Some(mut constructors) =
                                value_type.algebraic_constructor_metadata(&self.heap)
                            {
                                while let Some(constructor) = constructors.pop(&self.heap) {
                                    let mut fields = constructor.fields(&self.heap);
                                    while let Some(field) = fields.pop(&self.heap) {
                                        self.collect_export_type_dependencies_from_type_expr(
                                            field.type_expr(&self.heap),
                                            &mut dependencies,
                                        );
                                    }
                                }
                            }
                        }
                        IdentifierValueTypeKind::Abstract => {
                            if let Some(basis) = value_type.abstract_basis(&self.heap) {
                                let basis_type = TypeExprRef::new(basis);
                                if !basis_type.is_builtin_type(Type::Undefined) {
                                    self.collect_export_type_dependencies_from_type_expr(
                                        basis_type,
                                        &mut dependencies,
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            while let Some(dependency) = dependencies.pop(&self.heap) {
                let dependency_value: Value = dependency.into();
                if !ConsList::<Value>::contains_value(&self.heap, committed_exports, dependency_value)
                {
                    committed_exports = ConsList::<Value>::insert_ordered_value(
                        &mut self.heap,
                        committed_exports,
                        dependency_value,
                    );
                    closure_queue.insert_ordered(&mut self.heap, dependency);
                }
            }
        }

        let mut filtered_exports = NIL;
        let mut exported_identifiers: ConsList<Value> =
            ConsList::from_ref(committed_exports.into());
        while let Some(export_id) = exported_identifiers.pop_value(&self.heap) {
            if !ConsList::<Value>::contains_value(&self.heap, export.embargoes.into(), export_id) {
                filtered_exports = self.heap.cons_ref(export_id, filtered_exports);
            }
        }

        let filtered_exports = ConsList::<Value>::reversed_value(&mut self.heap, filtered_exports);

        let mut current_file_type_identifiers = ConsList::EMPTY;
        let mut current_file_types = current_file_definienda;
        while let Some(definiendum) = current_file_types.pop(&self.heap) {
            if definiendum.get_type_expr(&self.heap).is_builtin_type(Type::Type) {
                current_file_type_identifiers.insert_ordered(&mut self.heap, definiendum);
            }
        }

        if filtered_exports == NIL || current_file_type_identifiers.is_empty() {
            self.unused_types = false;
        } else {
            let mut exported_type_identifiers = ConsList::EMPTY;
            let mut referenced_type_identifiers = ConsList::EMPTY;
            let mut exported = ConsList::<IdentifierRecordRef>::from_ref(filtered_exports.into());
            while let Some(exported_identifier) = exported.pop(&self.heap) {
                if exported_identifier.get_type_expr(&self.heap).is_builtin_type(Type::Type) {
                    exported_type_identifiers.insert_ordered(&mut self.heap, exported_identifier);
                }

                let datatype = exported_identifier.get_type_expr(&self.heap);
                if !datatype.is_builtin_type(Type::Undefined) {
                    self.collect_export_type_dependencies_from_type_expr(
                        datatype,
                        &mut referenced_type_identifiers,
                    );
                }

                if let Some(value) = exported_identifier.get_value(&self.heap) {
                    if let IdentifierValueData::Typed { value_type, .. } = value.get_data(&self.heap)
                    {
                        match value_type.get_identifier_value_type_kind(&self.heap) {
                            IdentifierValueTypeKind::Synonym => {
                                self.collect_export_type_dependencies_from_type_expr(
                                    value_type.synonym_rhs_type_expr(&self.heap),
                                    &mut referenced_type_identifiers,
                                );
                            }
                            IdentifierValueTypeKind::Algebraic => {
                                if let Some(mut constructors) =
                                    value_type.algebraic_constructor_metadata(&self.heap)
                                {
                                    while let Some(constructor) = constructors.pop(&self.heap) {
                                        let mut fields = constructor.fields(&self.heap);
                                        while let Some(field) = fields.pop(&self.heap) {
                                            self.collect_export_type_dependencies_from_type_expr(
                                                field.type_expr(&self.heap),
                                                &mut referenced_type_identifiers,
                                            );
                                        }
                                    }
                                }
                            }
                            IdentifierValueTypeKind::Abstract => {
                                if let Some(basis) = value_type.abstract_basis(&self.heap) {
                                    let basis_type = TypeExprRef::new(basis);
                                    if !basis_type.is_builtin_type(Type::Undefined) {
                                        self.collect_export_type_dependencies_from_type_expr(
                                            basis_type,
                                            &mut referenced_type_identifiers,
                                        );
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }

            let mut bereaved = current_file_type_identifiers
                .difference(&mut self.heap, exported_type_identifiers);
            let unreferenced_current_types = current_file_type_identifiers
                .difference(&mut self.heap, referenced_type_identifiers);
            bereaved = bereaved.difference(&mut self.heap, unreferenced_current_types);
            self.unused_types = !bereaved.is_empty();
        }

        Ok(CommittedDirectiveState {
            exported_identifiers: filtered_exports,
            export_paths: export.pathname_requests.into(),
            export_embargoes: export.embargoes.into(),
        })
    }

    /// Emits post-export warnings for potentially bereaved/orphaned type information.
    ///
    /// C parity target: the `bereaved` warning block in `loadfile`.
    /// Current concrete behavior:
    /// - when exported_identifiers are active and orphan risk was flagged (`unused_types`),
    ///   emit a warning in verbose/make contexts,
    /// - do not fail the load path for warning-only conditions.
    ///
    /// This implementation is intentionally partial: full bereaved analysis depends
    /// on deferred type/export internals.
    pub(super) fn emit_bereaved_warnings_partial(&mut self) {
        if self.exported_identifiers != NIL
            && self.unused_types
            && (self.options.verbose || self.making)
        {
            println!("warning, export list may be incomplete - missing typenames");
        }
    }

    /// Emits diagnostics for parser-collected unused definitions/nonterminals.
    ///
    /// C parity target: the `detrop` warning block in `loadfile`.
    /// Current concrete behavior:
    /// - if deferred unused-definition diagnostics are present (`empty_production_nonterminals != NIL`),
    ///   emit a warning in verbose/make contexts,
    /// - clear the deferred diagnostics marker once processed for this load cycle.
    ///
    /// This implementation is intentionally partial: full production of these
    /// diagnostics remains deferred to parser/type integration.
    pub(super) fn emit_unused_definition_diagnostics_partial(&mut self) {
        if self.empty_production_nonterminals != NIL && (self.options.verbose || self.making) {
            println!("warning, script contains deferred unused-definition diagnostics");
        }

        self.empty_production_nonterminals = NIL;
    }

    /// Executes the codegen gate across loaded file definitions.
    ///
    /// C parity target: codegen loop and `initialising && ND!=NIL` panic branch.
    /// Current concrete behavior:
    /// - delegate to the subsystem-owned partial codegen boundary,
    /// - preserve subsystem-owned typed post-iteration failure,
    /// - otherwise succeed.
    pub(super) fn run_codegen_phase(&mut self) -> Result<(), CodegenError> {
        let inputs = super::codegen::CodegenBoundaryInputs::from_vm(self);
        let result = super::codegen::run_partial_codegen(&mut self.heap, inputs);
        if let Some(failure) = result.failure {
            return Err(failure);
        }

        Ok(())
    }

    /// Applies dump-writing and export-visibility orchestration for the load cycle.
    ///
    /// C parity target: `fixexports(); makedump(); unfixexports();` on success paths.
    /// Current concrete behavior:
    /// - run phase only for initialization or normal `.m` sources,
    /// - execute `fixexports`/`makedump` boundaries in order,
    /// - execute `unfix_exports` unwind restoration.
    ///
    /// This keeps orchestration concrete while deferring full dump serialization
    /// and deep export-visibility rewriting semantics.
    pub(super) fn run_dump_visibility_phase(
        &mut self,
        source_path: &str,
    ) -> Result<(), DumpWriteError> {
        // Initialization always writes a dump; otherwise only "normal" scripts
        // (ending in `.m`) run through fixexports/makedump/unfixexports.
        let should_run_dump_visibility = self.initializing || source_path.ends_with(".m");
        if !should_run_dump_visibility {
            return Ok(());
        }

        self.record_load_phase("dump-fixexports");
        self.fix_exports();
        self.record_load_phase("dump-write");
        self.run_makedump_boundary(source_path)?;
        self.record_load_phase("dump-unfixexports");
        self.unfix_exports();

        Ok(())
    }

    /// Applies `fixexports` visibility rewriting before dump serialization.
    ///
    /// Runs before dump serialization.
    /// Current concrete behavior:
    /// - computes invocation-scoped `internals` by internalizing non-exported IDs,
    /// - uses `%export` presence to control export-list mode (`export_list_is_active`),
    /// - preserves existing dump-phase orchestration labels and call structure.
    pub(super) fn fix_exports(&mut self) {
        // Export-list mode mirrors the temporary export gate:
        // when exported_identifiers are present we must avoid internalizing those IDs.
        let has_exports = self.exported_identifiers != NIL;
        self.export_list_is_active = has_exports;

        // `internals` is invocation-scoped output from this pass.
        // Shape: cons(internal_id, ...), where each `internal_id` is the
        // replacement/public-facing ID returned by internalization.
        self.internals = ConsList::EMPTY;

        // No files means no definienda to scan, so visibility rewriting is a no-op.
        if self.files.is_empty() {
            return;
        }

        // Branch split:
        // - no `%export` directives at all => internalize `%free` formal IDs + subsidiary files,
        // - otherwise => scan all files and skip explicitly exported IDs.
        let has_export_directive = self.exported_identifiers != NIL
            || self.export_paths != NIL
            || self.export_embargoes != NIL;

        // `exported_identifiers` is a list of identifier records that must remain public in this pass.
        let exported_identifiers: ConsList<IdentifierRecordRef> =
            ConsList::from_ref(self.exported_identifiers.into());

        if !has_export_directive {
            let mut free_identifiers = self.free_identifiers;
            while let Some(formal_binding) = free_identifiers.pop(&self.heap) {
                self.internalize_identifier_for_dump_visibility(
                    formal_binding.identifier(&self.heap),
                );
            }

            // This branch scans only subsidiary files (`rest(files)`), not the front script file.
            if let Some(mut subsidiary_files) = self.files.rest(&self.heap) {
                while let Some(file_record) = subsidiary_files.pop(&self.heap) {
                    self.collect_internalized_file_defs(file_record, exported_identifiers);
                }
            }
            return;
        }

        // Export-active branch: scan every file record, but skip members of `exported_identifiers`.
        let mut files = self.files;
        while let Some(file_record) = files.pop(&self.heap) {
            self.collect_internalized_file_defs(file_record, exported_identifiers);
        }
    }

    /// Internalizes one identifier using the current dump-visibility representation seam.
    ///
    /// Purpose: owns the `privatise`-equivalent heap mutation used by `fixexports`.
    /// Invariant: each successful internalization pushes exactly one internal-ID entry to `internals`.
    fn internalize_identifier_for_dump_visibility(
        &mut self,
        identifier: IdentifierRecordRef,
    ) -> Option<IdentifierRecordRef> {
        // We only internalize true identifier records.
        let identifier_ref = identifier.get_ref();
        if self.heap[identifier_ref].tag != Tag::Id {
            return None;
        }

        // Before rewriting heap shape, ensure typed-name definitions carry alias metadata.
        // For type names this keeps `who` in alias-capable form:
        //   cons(datapair(source_name, 0), here_info)
        // instead of bare `here_info`.
        let definition = identifier.get_definition(&self.heap);
        if RawValue::from(identifier.get_datatype(&self.heap)) == RawValue::from(Type::Type)
            && !definition.is_undefined()
        {
            let definition_ref = definition.get_ref();
            let here_info_ref = if self.heap[definition_ref].tag == Tag::Cons {
                self.heap[definition_ref].tail
            } else {
                definition_ref
            };
            let alias_metadata = definition
                .alias_metadata_pair(&self.heap)
                .unwrap_or_else(|| {
                    IdentifierDefinitionRef::alias_metadata_from_source_identifier(
                        &mut self.heap,
                        identifier,
                    )
                });
            let wrapped_definition = self
                .heap
                .cons_ref(alias_metadata.into(), Value::from(here_info_ref));
            identifier.set_definition(
                &mut self.heap,
                IdentifierDefinitionRef::from_ref(wrapped_definition.into()),
            );
        }

        // Start shape (public identifier):
        //   Tag::Id
        //   head = cons(strcons(name, who), type)
        //   tail = value
        let original_head = self.heap[identifier_ref].head;
        let mut original_value = self.heap[identifier_ref].tail;

        // For undefined identifiers, replace bare `UNDEF` with a fallback application payload:
        //   ap(datapair(source_name, 0), here_info)
        // The payload survives the ID -> private-name carrier rewrite so undefined-name
        // diagnostics remain attributable after internalization.
        if original_value == Combinator::Undef.into() {
            let definition = identifier.get_definition(&self.heap);
            let alias_metadata = definition
                .alias_metadata_pair(&self.heap)
                .unwrap_or_else(|| {
                    IdentifierDefinitionRef::alias_metadata_from_source_identifier(
                        &mut self.heap,
                        identifier,
                    )
                });
            let here_info_ref = if definition.is_undefined() {
                Combinator::Nil.into()
            } else {
                let definition_ref = definition.get_ref();
                if self.heap[definition_ref].tag == Tag::Cons {
                    self.heap[definition_ref].tail
                } else {
                    definition_ref
                }
            };
            original_value = self
                .heap
                .apply_ref(alias_metadata.into(), Value::from(here_info_ref))
                .into();
        }

        // Allocate a private-name cell (pname slot) in `heap.private_symbols`.
        // Initial private-name shape from allocator:
        //   Tag::StrCons
        //   head = private_symbol_index
        //   tail = identifier_ref (temporary payload)
        let private_name_ref = match self.heap.make_private_symbol_ref(identifier_ref.into()) {
            Value::Reference(reference) => reference,
            _ => return None,
        };
        let private_symbol_index = self.heap[private_name_ref].head;

        // Convert the allocated private-name cell into an internal ID shell by
        // retagging to `Tag::Id` and reusing the original identifier head.
        // Internal-ID shape after this step:
        //   Tag::Id
        //   head = original_head
        //   tail = <existing tail in private-name cell>
        self.heap[private_name_ref].tag = Tag::Id;
        self.heap[private_name_ref].head = original_head;

        // Convert the original public identifier cell into a private-name carrier.
        // Public-name-carrier shape after this step:
        //   Tag::StrCons
        //   head = private_symbol_index
        //   tail = original_value
        // This internalization step means public lookup now resolves via the
        // internal-ID indirection, while original value payload is preserved.
        self.heap[identifier_ref].tag = Tag::StrCons;
        self.heap[identifier_ref].head = private_symbol_index;
        self.heap[identifier_ref].tail = original_value;

        // Record the new internal ID as a restoration candidate for `unfixexports`.
        // `internals` remains the sole source of truth for later unfix traversal.
        let internal_id = IdentifierRecordRef::from_ref(private_name_ref);
        let internal_name = internal_id.get_name(&self.heap);
        self.heap
            .register_identifier_name(internal_name.as_str(), internal_id.get_ref());
        self.internals.push(&mut self.heap, internal_id);
        Some(internal_id)
    }

    /// Scans one file's definienda and internalizes each non-exported identifier.
    ///
    /// Purpose: centralizes file-def scan invariants for `fixexports` branch handling.
    /// Invariant: only `Tag::Id` definienda that are not exported are internalized.
    fn collect_internalized_file_defs(
        &mut self,
        file_record: FileRecord,
        exported_identifiers: ConsList<IdentifierRecordRef>,
    ) {
        let mut definienda = file_record.get_definienda(&self.heap);
        while let Some(definition_id) = definienda.pop(&self.heap) {
            let def_ref = definition_id.get_ref();
            if self.heap[def_ref].tag != Tag::Id {
                continue;
            }

            if exported_identifiers.contains(&self.heap, definition_id) {
                continue;
            }

            self.internalize_identifier_for_dump_visibility(definition_id);
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
        source_path: &str,
    ) -> Result<(), DumpWriteError> {
        let dump_path = PathBuf::from(format!(
            "{}{}",
            source_path.strip_suffix(".m").unwrap_or(source_path),
            ".x"
        ));
        let dump_bytes = self.serialize_dump_for_current_load_state();
        self.write_dump_file_atomically(&dump_path, &dump_bytes)?;

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
    ) -> Result<(), DumpWriteError> {
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

    /// Serializes the currently reachable dump shape for load orchestration boundaries.
    ///
    /// Success-path dumps encode file anchors with empty definition/trailer lists.
    /// Syntax-fallback dumps encode syntax sentinel + error line + old-file anchors.
    /// Invariant: output is deterministic and decode-compatible with current `load_script` section order.
    fn serialize_dump_for_current_load_state(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.push(WORD_SIZE as u8);
        bytes.push(XVERSION as u8);

        if self.files.is_empty() {
            bytes.push(0);
            bytes.extend_from_slice(&self.error_line.to_le_bytes());

            let mut old_files = self.old_files;
            while let Some(file_record) = old_files.pop(&self.heap) {
                let filename = file_record.get_file_name(&self.heap);
                bytes.push(1);
                bytes.extend_from_slice(filename.as_bytes());
                bytes.push(0);

                let modified = file_record
                    .get_last_modified(&self.heap)
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs();
                bytes.extend_from_slice(&modified.to_le_bytes());
            }

            return bytes;
        }

        let mut files = self.files;
        while let Some(file_record) = files.pop(&self.heap) {
            let filename = file_record.get_file_name(&self.heap);
            bytes.push(1);
            bytes.extend_from_slice(filename.as_bytes());
            bytes.push(0);

            let modified = file_record
                .get_last_modified(&self.heap)
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();
            bytes.extend_from_slice(&modified.to_le_bytes());

            let shareable = if file_record.is_shareable(&self.heap) {
                1
            } else {
                0
            };
            bytes.push(shareable);

            // Current parser-placeholder load path keeps file definienda empty.
            // This lone DEF_X is a placeholder for deferred `dump_defs` parity once
            // real file definition payloads become reachable.
            bytes.push(Bytecode::Definition.code());
        }

        bytes.push(0);

        // Placeholder empty `dump_defs(algshfns)` section; real object/definition
        // serialization remains deferred until non-empty payloads are reachable.
        bytes.push(Bytecode::Definition.code());

        // Placeholder `[ND]` / `[True]` marker encoding. The trailing DEF_X remains
        // the deferred `dump_ob` / `dump_defs` boundary for full parity payloads.
        if self.unused_types {
            #[allow(clippy::cast_enum_truncation)]
            bytes.push(Combinator::True as u8);
            bytes.push(Bytecode::Definition.code());
        } else {
            bytes.push(Bytecode::Definition.code());
        }

        // Placeholder empty section for the decoded `sui_generis_constructors`
        // block; real `dump_defs` parity is deferred.
        bytes.push(Bytecode::Definition.code());

        // Placeholder empty `%free` section; real `dump_ob` / `dump_defs` parity for
        // bound free-id payloads is deferred to later parser/load integration.
        bytes.push(Bytecode::Definition.code());

        // Placeholder empty internals definition list; real `dump_defs(internals)`
        // serialization is deferred until non-empty internals are dumped here.
        bytes.push(Bytecode::Definition.code());

        bytes
    }

    /// Writes dump bytes to `path` via temp-file replacement.
    ///
    /// The temp-file rename keeps partially written dumps from becoming visible at `path`.
    /// Invariant: on write failure, no success result is returned to load orchestration.
    fn write_dump_file_atomically(
        &self,
        path: &Path,
        dump_bytes: &[u8],
    ) -> Result<(), DumpWriteError> {
        let mut temp_path = path.as_os_str().to_os_string();
        temp_path.push(".tmp");
        let temp_path = PathBuf::from(temp_path);

        let mut temp_file =
            std::fs::File::create(&temp_path).map_err(|source| DumpWriteError::WriteFailed {
                path: path.display().to_string(),
                source,
            })?;

        temp_file
            .write_all(dump_bytes)
            .map_err(|source| DumpWriteError::WriteFailed {
                path: path.display().to_string(),
                source,
            })?;
        temp_file
            .sync_all()
            .map_err(|source| DumpWriteError::WriteFailed {
                path: path.display().to_string(),
                source,
            })?;

        std::fs::rename(&temp_path, path).map_err(|source| DumpWriteError::WriteFailed {
            path: path.display().to_string(),
            source,
        })
    }

    /// Parses already-materialized source text for the load pipeline.
    ///
    /// This exists so parser-boundary callers can drive classification,
    /// parser execution, and parse-result adaptation without first writing the
    /// source to disk.
    /// The invariant is that identical `source_path`, `source_text`, and
    /// `modified_time` inputs produce the same `ParsePhaseOutcome` and parser
    /// diagnostics as the former inlined path in `parse_source_script`.
    pub(super) fn parse_source_text(
        &mut self,
        source_path: &str,
        source_text: &str,
        modified_time: SystemTime,
        _is_main_script: bool,
    ) -> Result<ParsePhaseOutcome, LoadFileError> {
        let load_script_form = self.classify_load_script_form(source_path, source_text)?;
        match load_script_form {
            LoadScriptForm::Expression | LoadScriptForm::TopLevelScript => {}
            LoadScriptForm::OtherTopLevelForm => {
                return Err(SourceParseError::UnsupportedTopLevelForm {
                    path: source_path.to_string(),
                }
                .into());
            }
        }

        let placeholder_files = self.empty_environment_for_source(source_path, modified_time);
        let lexer = Lexer::new(source_path, source_text);
        let activation = self.parser_activation(load_script_form);
        let mut parser = Parser::new(lexer, activation);
        let parsed = parser.parse();

        match parser.finish_run(parsed) {
            ParserRunResult::ParsedExpression(expression) => {
                self.last_expression = expression;
                Ok(ParsePhaseOutcome {
                    status: ParsePhaseStatus::Parsed,
                    files: placeholder_files,
                    top_level_payload: None,
                })
            }
            ParserRunResult::ParsedTopLevelScript(payload) => {
                let committed_files =
                    self.commit_parsed_top_level_script(source_path, modified_time, &payload);
                Ok(ParsePhaseOutcome {
                    status: ParsePhaseStatus::Parsed,
                    files: committed_files,
                    top_level_payload: Some(payload),
                })
            }
            ParserRunResult::SyntaxError(diagnostics) => {
                self.commit_parser_run_diagnostics(diagnostics);
                Ok(ParsePhaseOutcome {
                    status: ParsePhaseStatus::SyntaxError,
                    files: placeholder_files,
                    top_level_payload: None,
                })
            }
        }
    }

    /// Parses a source file for the load pipeline.
    ///
    /// This recognizes expression input and the currently supported top-level
    /// source-script forms. It returns provisional parser payload; later load
    /// phases validate directives and use the committed file graph as the
    /// authoritative source-compilation substrate.
    pub(super) fn parse_source_script(
        &mut self,
        source_file: &File,
        source_path: &str,
        modified_time: SystemTime,
        _is_main_script: bool,
    ) -> Result<ParsePhaseOutcome, LoadFileError> {
        let mut source_text = String::new();
        let mut reader = BufReader::new(source_file);
        reader.read_to_string(&mut source_text).map_err(|source| {
            SourceInputError::UnreadableFile {
                path: source_path.to_string(),
                source,
            }
        })?;

        self.parse_source_text(source_path, &source_text, modified_time, _is_main_script)
    }

    fn commit_parsed_top_level_script(
        &mut self,
        source_path: &str,
        modified_time: SystemTime,
        payload: &ParserTopLevelScriptPayload,
    ) -> ConsList<FileRecord> {
        let files = self.empty_environment_for_source(source_path, modified_time);

        if let Some(current_file) = files.head(&self.heap) {
            for definition in &payload.definitions {
                self.commit_top_level_definition_payload(current_file, definition);
            }
            for specification in &payload.specifications {
                self.commit_top_level_specification_payload(current_file, specification);
            }
            for type_declaration in &payload.type_declarations {
                self.commit_top_level_type_declaration_payload(
                    current_file,
                    type_declaration,
                    &payload.constructor_declarations,
                );
            }
            for constructor in &payload.constructor_declarations {
                self.commit_top_level_constructor_payload(current_file, constructor);
            }
            if !payload.free_bindings.is_empty() {
                self.commit_top_level_free_bindings_payload(current_file, &payload.free_bindings);
            }
            for group in &payload.abstype_groups {
                let mut tids = NIL;
                for &type_identifier in group.type_identifiers.iter().rev() {
                    if let Some(&show_function) = group.signature_identifiers.iter().find(|identifier| {
                        self.identifier_name(**identifier)
                            == format!("show{}", self.identifier_name(type_identifier))
                    }) {
                        self.attach_type_show_function(type_identifier, show_function)
                            .expect("abstype type identifier should accept typed show-function attachment after commit");
                    }
                    tids = self.heap.cons_ref(type_identifier.into(), tids);
                }

                let ids = group
                    .signature_identifiers
                    .iter()
                    .rev()
                    .fold(NIL, |list, identifier| self.heap.cons_ref((*identifier).into(), list));
                let group_entry = self.heap.cons_ref(tids, ids);
                self.type_abstractions = ConsList::from_ref(
                    self.heap
                        .cons_ref(group_entry, self.type_abstractions.into())
                        .into(),
                );
            }
        }

        files
    }

    fn commit_top_level_definition_payload(
        &mut self,
        current_file: FileRecord,
        definition: &ParserDefinitionPayload,
    ) {
        let identifier = definition.identifier;
        let definition_metadata = self.definition_metadata_from_anchor(definition.anchor);

        identifier.set_definition(&mut self.heap, definition_metadata);
        identifier.set_datatype(&mut self.heap, Type::Undefined.into());
        identifier.set_value_from_data(
            &mut self.heap,
            IdentifierValueData::Arbitrary(definition.body.into()),
        );

        current_file.push_definiendum_once(&mut self.heap, definition.identifier);
    }

    fn commit_top_level_specification_payload(
        &mut self,
        current_file: FileRecord,
        specification: &ParserSpecificationPayload,
    ) {
        let identifier = specification.identifier;
        let type_expr = specification.type_expr;
        identifier.set_type_expr(&mut self.heap, type_expr);

        current_file.push_definiendum_once(&mut self.heap, specification.identifier);
    }

    fn commit_top_level_type_declaration_payload(
        &mut self,
        current_file: FileRecord,
        type_declaration: &ParserTypeDeclarationPayload,
        constructor_payloads: &[ParserConstructorPayload],
    ) {
        let type_identifier = type_declaration.type_identifier;
        let definition_metadata = self.definition_metadata_from_anchor(type_declaration.anchor);
        type_identifier.set_definition(&mut self.heap, definition_metadata);
        type_identifier.set_type_expr(&mut self.heap, TypeExprRef::new(Type::Type.into()));

        let existing_typed_value = type_identifier.get_value(&self.heap).and_then(|value| {
            match value.get_data(&self.heap) {
                IdentifierValueData::Typed {
                    show_function,
                    value_type,
                    ..
                } => Some((show_function, value_type)),
                _ => None,
            }
        });
        let inherited_show_function = existing_typed_value.and_then(|(show_function, _)| {
            (show_function != Value::None).then_some(show_function)
        });

        let (kind, info, show_function) = match type_declaration.kind {
            IdentifierValueTypeKind::Algebraic => (
                IdentifierValueTypeKind::Algebraic,
                self.commit_algebraic_type_constructor_info(
                    type_declaration.type_identifier.get_ref(),
                    constructor_payloads,
                ),
                None,
            ),
            IdentifierValueTypeKind::Synonym => match existing_typed_value {
                Some((_, existing_value_type))
                    if existing_value_type.get_identifier_value_type_kind(&self.heap)
                        == IdentifierValueTypeKind::Abstract =>
                {
                    (
                        IdentifierValueTypeKind::Abstract,
                        type_declaration.info,
                        inherited_show_function,
                    )
                }
                _ => (IdentifierValueTypeKind::Synonym, type_declaration.info, None),
            },
            IdentifierValueTypeKind::Abstract => match existing_typed_value {
                Some((_, existing_value_type))
                    if existing_value_type.get_identifier_value_type_kind(&self.heap)
                        == IdentifierValueTypeKind::Synonym =>
                {
                    (
                        IdentifierValueTypeKind::Abstract,
                        existing_value_type.synonym_rhs_type_expr(&self.heap).value(),
                        inherited_show_function,
                    )
                }
                Some((_, existing_value_type))
                    if existing_value_type.get_identifier_value_type_kind(&self.heap)
                        == IdentifierValueTypeKind::Abstract =>
                {
                    (
                        IdentifierValueTypeKind::Abstract,
                        existing_value_type
                            .abstract_basis(&self.heap)
                            .unwrap_or(Type::Undefined.into()),
                        inherited_show_function,
                    )
                }
                _ => (
                    IdentifierValueTypeKind::Abstract,
                    Type::Undefined.into(),
                    inherited_show_function,
                ),
            },
            _ => (type_declaration.kind, type_declaration.info, None),
        };

        let value = IdentifierValueRef::from_type_identifier_parts(
            &mut self.heap,
            TypeIdentifierValueParts {
                arity: type_declaration.arity,
                show_function,
                kind,
                info,
            },
        );
        type_identifier.set_value(&mut self.heap, value);

        current_file.push_definiendum_once(&mut self.heap, type_declaration.type_identifier);
    }

    fn commit_top_level_constructor_payload(
        &mut self,
        current_file: FileRecord,
        constructor_payload: &ParserConstructorPayload,
    ) {
        let constructor = constructor_payload.constructor;
        let parent_type = constructor_payload.parent_type;
        let definition_metadata = self.definition_metadata_from_anchor(constructor_payload.anchor);
        constructor.set_definition(&mut self.heap, definition_metadata);
        let constructor_type = self.build_declared_constructor_type(constructor_payload);
        constructor.set_type_expr(&mut self.heap, TypeExprRef::new(constructor_type));
        let constructor_index = self.constructor_ordinal_in_parent_type(parent_type, constructor);
        let constructor_value =
            ConstructorRef::new(&mut self.heap, constructor_index, constructor.into());
        constructor.set_value_from_data(
            &mut self.heap,
            IdentifierValueData::Arbitrary(constructor_value.into()),
        );

        current_file.push_definiendum_once(&mut self.heap, constructor_payload.constructor);
    }

    /// Returns the committed ordinal of a constructor within its parent algebraic type declaration.
    /// This exists so constructor runtime values use the same constructor ordering already committed on the parent type.
    /// The invariant is that the parent type has already been committed with an algebraic constructor list before constructors are materialized.
    fn constructor_ordinal_in_parent_type(
        &self,
        parent_type: IdentifierRecordRef,
        constructor: IdentifierRecordRef,
    ) -> i16 {
        let Some(parent_value) = parent_type.get_value(&self.heap) else {
            return 0;
        };
        let IdentifierValueData::Typed { value_type, .. } = parent_value.get_data(&self.heap)
        else {
            return 0;
        };
        if value_type.get_identifier_value_type_kind(&self.heap)
            != IdentifierValueTypeKind::Algebraic
        {
            return 0;
        }

        let Some(mut constructors) = value_type.algebraic_constructor_metadata(&self.heap) else {
            return 0;
        };
        let mut ordinal: i16 = 0;
        while let Some(next_constructor) = constructors.pop(&self.heap) {
            if next_constructor.constructor(&self.heap) == constructor {
                return ordinal;
            }
            ordinal += 1;
        }

        0
    }

    /// Commits parser-collected constructor payloads into subsystem-owned algebraic type metadata.
    /// This exists so parent type identifiers retain constructor order, arity, field types, and strict-field flags after parser-local shapes disappear.
    /// The invariant is that only payloads for `parent_type` are encoded, in declaration source order.
    fn commit_algebraic_type_constructor_info(
        &mut self,
        parent_type: RawValue,
        constructor_payloads: &[ParserConstructorPayload],
    ) -> Value {
        let mut committed_metadata = NIL;

        for constructor_payload in constructor_payloads.iter().rev() {
            if constructor_payload.parent_type.get_ref() != parent_type {
                continue;
            }

            let committed_fields = ConsList::from_ref(
                constructor_payload
                    .fields
                    .iter()
                    .rev()
                    .fold(NIL, |fields, field| {
                        let field_ref = AlgebraicConstructorFieldRef::new(
                            &mut self.heap,
                            AlgebraicConstructorFieldParts {
                                type_expr: TypeExprRef::new(field.type_expr),
                                is_strict: field.is_strict,
                            },
                        );
                        self.heap.cons_ref(field_ref.into(), fields)
                    })
                    .into(),
            );
            let metadata_ref = AlgebraicConstructorMetadataRef::new(
                &mut self.heap,
                AlgebraicConstructorMetadataParts {
                    constructor: constructor_payload.constructor,
                    arity: constructor_payload.arity,
                    fields: committed_fields,
                },
            );
            committed_metadata = self.heap.cons_ref(metadata_ref.into(), committed_metadata);
        }

        committed_metadata
    }

    /// Builds the result type for one declared algebraic constructor before field arrows are prepended.
    /// This exists so constructor-type assembly reuses one owner for parent-type application over declared lhs type variables.
    /// The invariant is that the returned type is `parent_type` applied to type variables `1..=arity` in source order.
    fn build_declared_parent_type_application(
        &mut self,
        parent_type: RawValue,
        arity: isize,
    ) -> Value {
        let mut parent_type_value: Value = parent_type.into();
        for typevar_index in 1..=arity {
            let typevar = self
                .heap
                .type_var_ref(Value::None, Value::Data(typevar_index as RawValue));
            parent_type_value = self.heap.apply_ref(parent_type_value, typevar);
        }

        parent_type_value
    }

    /// Builds the committed runtime type for one constructor declaration.
    /// This exists so constructor materialization consumes declared field metadata instead of relying on a nullary-only convention.
    /// The invariant is that declared fields are prepended as right-associated arrow arguments in source order.
    fn build_declared_constructor_type(
        &mut self,
        constructor_payload: &ParserConstructorPayload,
    ) -> Value {
        constructor_payload.fields.iter().rev().fold(
            self.build_declared_parent_type_application(
                constructor_payload.parent_type.get_ref(),
                constructor_payload.parent_type_arity,
            ),
            |result_type, field| {
                let field_type = TypeExprRef::new(field.type_expr);
                self.heap
                    .apply2(Type::Arrow.into(), field_type.value(), result_type)
            },
        )
    }

    fn commit_top_level_free_bindings_payload(
        &mut self,
        current_file: FileRecord,
        free_bindings: &[ParserFreeBindingPayload],
    ) {
        let mut formal_bindings: ConsList<FreeFormalBindingRef> = ConsList::EMPTY;

        for free_binding in free_bindings {
            let identifier = free_binding.identifier;
            let definition_metadata = self.definition_metadata_from_anchor(free_binding.anchor);
            let type_expr = free_binding.type_expr;
            identifier.set_definition(&mut self.heap, definition_metadata);
            identifier.set_type_expr(&mut self.heap, type_expr);

            if type_expr.is_builtin_type(Type::Type) {
                let free_type = IdentifierValueRef::from_type_identifier_parts(
                    &mut self.heap,
                    TypeIdentifierValueParts {
                        arity: 0,
                        show_function: None,
                        kind: IdentifierValueTypeKind::Free,
                        info: Combinator::Nil.into(),
                    },
                );
                identifier.set_value(&mut self.heap, free_type);
            }

            let original_name_ref = self.heap.string(identifier.get_name(&self.heap));
            let original_name = DataPair::new(&mut self.heap, original_name_ref.into(), 0.into());
            let formal_binding =
                FreeFormalBindingRef::new(&mut self.heap, identifier, original_name, type_expr);
            formal_bindings.push(&mut self.heap, formal_binding);
            current_file.push_definiendum_once(&mut self.heap, free_binding.identifier);
        }

        self.free_identifiers = ConsList::from_ref(super::bytecode::hdsort_binding_list_ref(
            &mut self.heap,
            formal_bindings.get_ref(),
        ));
        current_file.set_shareable(&mut self.heap, false);
    }

    // Todo: Promote to a typed include-actual binding API; blocker: `bindparams` still consumes raw
    //       Miranda actual-binding heap shapes; migrate to `src/vm/bytecode.rs` once the `%free`
    //       actual-binding ingress is typed.
    /// Encodes parser-owned include binding payloads into the raw actual-binding stream consumed by
    /// `bindparams`. This exists so load-owned include lowering keeps the typed payload to heap
    /// shape conversion in one ingress seam for `%free` actual binding. The invariant is that value
    /// and type bindings preserve their meaning and enter `bindparams` in identifier sort order.
    pub(super) fn lower_include_binding_actuals(
        &mut self,
        bindings: &[ParserIncludeBindingPayload],
    ) -> Value {
        let mut actual_bindings: ConsList = ConsList::EMPTY;
        for binding in bindings.iter().rev() {
            let actual = match binding {
                ParserIncludeBindingPayload::Value { identifier, body } => {
                    self.heap.cons_ref((*identifier).into(), *body)
                }
                ParserIncludeBindingPayload::Type {
                    identifier,
                    type_value,
                } => self
                    .heap
                    .apply_ref((*identifier).into(), (*type_value).into()),
            };
            actual_bindings.push(&mut self.heap, RawValue::from(actual));
        }

        super::bytecode::hdsort_binding_list_ref(&mut self.heap, actual_bindings.get_ref()).into()
    }

    fn definition_metadata_from_anchor(&mut self, anchor: FileInfoRef) -> IdentifierDefinitionRef {
        let script_file = anchor.script_file(&self.heap);
        let line_number = anchor.line_number(&self.heap);
        IdentifierDefinitionRef::new(
            &mut self.heap,
            HereInfo {
                script_file,
                line_number,
            },
            None,
        )
    }

    fn parser_activation(&mut self, load_script_form: LoadScriptForm) -> ParserActivation<'_> {
        let listdiff_function = self.listdiff_fn.into();
        let void_tuple = self
            .void_
            .get_value(&self.heap)
            .map(Value::from)
            .unwrap_or_else(|| self.void_.into());

        ParserActivation {
            heap: &mut self.heap,
            vm: ParserVmContext::new(listdiff_function, void_tuple),
            session: ParserSessionState::default(),
            deferred: ParserDeferredState::default(),
            entry_mode: match load_script_form {
                LoadScriptForm::TopLevelScript | LoadScriptForm::OtherTopLevelForm => {
                    ParserEntryMode::TopLevelScriptOnly
                }
                LoadScriptForm::Expression => ParserEntryMode::Mixed,
            },
        }
    }

    fn commit_parser_run_diagnostics(&mut self, diagnostics: ParserRunDiagnostics) {
        for diagnostic in diagnostics.diagnostics {
            if self.error_line == 0 {
                if let Some(here_info) = &diagnostic.here_info {
                    if here_info.line_number > 0 {
                        self.error_line = here_info.line_number as usize;
                    }
                }
            }

            if let Some(here_info) = diagnostic.here_info.clone() {
                let raw_here_info = FileInfoRef::from_script_file(
                    &mut self.heap,
                    here_info.script_file,
                    here_info.line_number,
                );
                self.error_locations.push(raw_here_info.get_ref());
            }

            self.parser_diagnostics.push(diagnostic);
        }
    }

    /// Classifies a source file by the top-level form at its start.
    ///
    /// `%include` and `%export` are accepted as top-level directives here. The active top-level
    /// definition slice includes simple name-parameter function forms, so classification scans the
    /// first physical line for a later `=` instead of only matching a bare `Name = ...` prefix.
    /// Other top-level forms are left to the existing deferred integration path.
    pub(super) fn classify_load_script_form(
        &mut self,
        source_path: &str,
        source_text: &str,
    ) -> Result<LoadScriptForm, LoadFileError> {
        let mut lexer = Lexer::new(source_path, source_text);
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            let lookahead: ParserLookahead =
                lexer
                    .yylex()
                    .map_err(|source| SourceInputError::UnreadableFile {
                        path: source_path.to_string(),
                        source: std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            source.to_string(),
                        ),
                    })?;
            match lookahead.token {
                Token::Newline => break,
                Token::EOF => break,
                token => tokens.push(token),
            }

            if tokens.len() >= 12 {
                break;
            }
        }

        let first = tokens.first().copied();
        let second = tokens.get(1).copied();
        let name_led = matches!(
            first,
            Some(Token::Identifier | Token::Name | Token::ConstructorName)
        );
        let typeform_led = matches!(first, Some(Token::Times | Token::TypeVar));
        let has_equal = tokens.iter().skip(1).any(|token| *token == Token::Equal);
        let has_declaration_marker = tokens.iter().skip(1).any(|token| {
            matches!(
                token,
                Token::ColonColon | Token::EqualEqual | Token::Colon2Equal
            )
        });

        Ok(match (first, second) {
            (Some(Token::Include), _)
            | (Some(Token::Export), _)
            | (Some(Token::Identifier), Some(Token::ColonColon))
            | (Some(Token::Identifier), Some(Token::EqualEqual))
            | (Some(Token::Identifier), Some(Token::Colon2Equal))
            | (Some(Token::Name), Some(Token::ColonColon))
            | (Some(Token::Name), Some(Token::EqualEqual))
            | (Some(Token::Name), Some(Token::Colon2Equal))
            | (Some(Token::Free), _)
            | (Some(Token::AbsoluteType), _) => LoadScriptForm::TopLevelScript,
            _ if name_led && has_declaration_marker => LoadScriptForm::TopLevelScript,
            _ if typeform_led && has_declaration_marker => LoadScriptForm::TopLevelScript,
            _ if name_led && has_equal => LoadScriptForm::TopLevelScript,
            (Some(Token::Lex), _)
            | (Some(Token::BNF), _)
            | (Some(Token::Type), _)
            | (Some(Token::Value), _)
            | (Some(Token::Eval), _)
            | (Some(Token::ConstructorName), Some(Token::Colon2Equal)) => {
                LoadScriptForm::OtherTopLevelForm
            }
            _ => LoadScriptForm::Expression,
        })
    }

    /// Restores internalized identifiers back to their public lookup shape.
    ///
    /// Current concrete behavior:
    /// - exits export-list mode,
    /// - in exported_identifiers-only mode, preserves internalized state for export listing,
    /// - otherwise restores each entry currently tracked in `internals` and clears it.
    pub(super) fn unfix_exports(&mut self) {
        self.export_list_is_active = false;
        if !self.options.make_exports.is_empty() {
            return;
        }

        let mut internals = self.internals;
        while let Some(internal_id) = internals.pop(&self.heap) {
            self.restore_public_identifier_from_internal(internal_id);
        }

        self.internals = ConsList::EMPTY;
    }

    /// Restores one internal ID entry to public ID shape and lookup binding.
    ///
    /// Purpose: owns the representation-sensitive unfix step for one `internals` element.
    /// Invariant: successful restoration rebinds the identifier name to the restored public ID cell.
    fn restore_public_identifier_from_internal(
        &mut self,
        internal_id: IdentifierRecordRef,
    ) -> Option<IdentifierRecordRef> {
        // `internals` should contain internal IDs produced by fix-time rewriting.
        // Expected internal-ID shape on entry:
        //   Tag::Id
        //   head = cons(strcons(name, who), type)
        //   tail = restored_public_ref
        // where `restored_public_ref` currently points to a private-name carrier.
        let internal_id_ref = internal_id.get_ref();
        if self.heap[internal_id_ref].tag != Tag::Id {
            return None;
        }

        // Resolve the canonical name before mutating heap cells so we can rebind
        // identifier lookup to the restored public cell at the end.
        let restored_name = internal_id.get_name(&self.heap);

        // The internal ID's value slot is the public-name carrier to restore.
        // Expected carrier shape before restore:
        //   Tag::StrCons
        //   head = private_symbol_index
        //   tail = value
        let restored_public_ref = self.heap[internal_id_ref].tail;
        if restored_public_ref < ATOM_LIMIT || self.heap[restored_public_ref].tag != Tag::StrCons {
            return None;
        }

        // Restore the public cell to identifier shape using the internal ID's head.
        // Public cell shape after this step:
        //   Tag::Id
        //   head = hd[internal_id_ref]
        //   tail = prior value payload (still in place)
        // Invariant: both `Id` and private-name carrier store value in `tail`, so
        // this retag+head write is sufficient for structural restoration.
        self.heap[restored_public_ref].tag = Tag::Id;
        self.heap[restored_public_ref].head = self.heap[internal_id_ref].head;

        // Some undefined-name fallback payloads are represented as applications
        // whose head is a DataPair sentinel. When present, normalize value back
        // to `UNDEF` so restored IDs report as undefined through normal paths.
        // Checked shape:
        //   restored_value_ref = ap(x, y)
        //   hd[restored_value_ref] has Tag::DataPair
        let restored_value_ref = self.heap[restored_public_ref].tail;
        if restored_value_ref >= ATOM_LIMIT && self.heap[restored_value_ref].tag == Tag::Ap {
            let head_ref = self.heap[restored_value_ref].head;
            if head_ref >= ATOM_LIMIT && self.heap[head_ref].tag == Tag::DataPair {
                self.heap[restored_public_ref].tail = Combinator::Undef.into();
            }
        }

        // Rebind name lookup to the restored public identifier cell.
        // Invariant: after unfix traversal, external name resolution should point
        // at restored public IDs, not transient internal IDs.
        self.heap
            .register_identifier_name(restored_name.as_str(), restored_public_ref);

        Some(IdentifierRecordRef::from_ref(restored_public_ref))
    }
}

impl VM {
    pub(crate) fn intern_identifier(&mut self, name: &str) -> IdentifierRecordRef {
        self.heap
            .get_identifier(name)
            .unwrap_or_else(|| self.heap.make_empty_identifier(name))
    }

    pub(crate) fn identifier_name(&self, identifier: IdentifierRecordRef) -> String {
        identifier.get_name(&self.heap)
    }

    pub(crate) fn source_name_metadata(&mut self, source_name: &str) -> DataPair {
        let source_identifier = self.intern_identifier(source_name);
        IdentifierDefinitionRef::alias_metadata_from_source_identifier(
            &mut self.heap,
            source_identifier,
        )
    }

    pub(crate) fn private_name(&mut self, value: Value) -> Value {
        self.heap.make_private_symbol_ref(value)
    }

    pub(crate) fn translate_type_identifier(&self, identifier: IdentifierRecordRef) -> Value {
        match identifier.get_name(&self.heap).as_str() {
            "bool" => Type::Bool.into(),
            "num" => Type::Number.into(),
            "char" => Type::Char.into(),
            _ => identifier.into(),
        }
    }

    pub(crate) fn listdiff_function(&self) -> Value {
        self.listdiff_fn.into()
    }

    pub(crate) fn numeric_one(&self) -> Value {
        Value::Data(1)
    }

    pub(crate) fn void_tuple(&self) -> Value {
        self.void_
            .get_value(&self.heap)
            .map(Value::from)
            .unwrap_or_else(|| self.void_.into())
    }

    pub(crate) fn attach_type_show_function(
        &mut self,
        type_identifier: IdentifierRecordRef,
        show_function: IdentifierRecordRef,
    ) -> Result<(), ParserSupportError> {
        let Some(type_value) = type_identifier.get_value(&self.heap) else {
            return Err(ParserSupportError::DeferredMutation {
                operation: "attach_type_show_function",
            });
        };

        match type_value.get_data(&self.heap) {
            IdentifierValueData::Typed {
                arity, value_type, ..
            } => {
                type_identifier.set_value_from_data(
                    &mut self.heap,
                    IdentifierValueData::Typed {
                        arity,
                        show_function: show_function.into(),
                        value_type,
                    },
                );
                Ok(())
            }
            _ => Err(ParserSupportError::DeferredMutation {
                operation: "attach_type_show_function",
            }),
        }
    }

    pub(crate) fn indent_function(&self) -> Result<Value, ParserSupportError> {
        Ok(self.indent_fn.into())
    }

    pub(crate) fn outdent_function(&self) -> Result<Value, ParserSupportError> {
        Ok(self.outdent_fn.into())
    }

    pub(crate) fn record_deferred_exports(
        &mut self,
        _exports: Value,
        _exportfiles: Value,
        _embargoes: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_exports",
        })
    }

    pub(crate) fn record_deferred_includees(
        &mut self,
        _includees: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_includees",
        })
    }

    pub(crate) fn record_deferred_freeids(
        &mut self,
        _freeids: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "record_deferred_freeids",
        })
    }

    pub(crate) fn declare_definition(
        &mut self,
        _definition: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_definition",
        })
    }

    pub(crate) fn apply_specification(
        &mut self,
        _identifier: Value,
        _specification: Value,
        _here: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "apply_specification",
        })
    }

    pub(crate) fn declare_type(
        &mut self,
        _identifier: Value,
        _kind: IdentifierValueTypeKind,
        _info: Value,
        _here: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_type",
        })
    }

    pub(crate) fn declare_constructor(
        &mut self,
        _identifier: Value,
        _type_value: Value,
        _rhs_ids: Value,
        _here: Value,
    ) -> Result<(), ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "declare_constructor",
        })
    }

    pub(crate) fn free_value(&self) -> Result<Value, ParserSupportError> {
        Err(ParserSupportError::DeferredMutation {
            operation: "free_value",
        })
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
