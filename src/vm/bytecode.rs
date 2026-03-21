use super::*;
use crate::big_num::IntegerRef;
use crate::data::api::HeapString;

/// Recursively sorts `%free` actual-binding cons cells by binding-name key.
///
/// This directly implements Miranda C `hdsort` (`miralib/data.c`) on raw binding-list refs
/// so include-time `bindparams` can merge formal/actual streams in name order.
/// Invariant: returns a list containing the same binding-pair refs as input, reordered only by key.
pub(super) fn hdsort_binding_list_ref(heap: &mut Heap, mut list_ref: RawValue) -> RawValue {
    let nil: RawValue = Combinator::NIL.into();

    if list_ref == nil {
        return nil;
    }

    if heap[list_ref].tail == nil {
        return list_ref;
    }

    let mut left_half: RawValue = nil;
    let mut right_half: RawValue = nil;

    while list_ref != nil {
        let pair_ref = heap[list_ref].head;
        right_half = RawValue::from(heap.cons_ref(pair_ref.into(), right_half.into()));
        list_ref = heap[list_ref].tail;

        std::mem::swap(&mut left_half, &mut right_half);
    }

    left_half = hdsort_binding_list_ref(heap, left_half);
    right_half = hdsort_binding_list_ref(heap, right_half);

    let mut merged_reversed: RawValue = nil;
    while left_half != nil && right_half != nil {
        let left_pair_ref = heap[left_half].head;
        let right_pair_ref = heap[right_half].head;

        if binding_name_for_hdsort(heap, left_pair_ref)
            < binding_name_for_hdsort(heap, right_pair_ref)
        {
            merged_reversed =
                RawValue::from(heap.cons_ref(left_pair_ref.into(), merged_reversed.into()));
            left_half = heap[left_half].tail;
        } else {
            merged_reversed =
                RawValue::from(heap.cons_ref(right_pair_ref.into(), merged_reversed.into()));
            right_half = heap[right_half].tail;
        }
    }

    let mut remaining = if left_half == nil {
        right_half
    } else {
        left_half
    };
    while remaining != nil {
        let pair_ref = heap[remaining].head;
        merged_reversed = RawValue::from(heap.cons_ref(pair_ref.into(), merged_reversed.into()));
        remaining = heap[remaining].tail;
    }

    let merged_reversed_list: ConsList<FreeFormalBindingRef> = ConsList::from_ref(merged_reversed);
    merged_reversed_list.reversed(heap).get_ref()
}

/// Returns the lexical sort key for one `%free` binding pair.
///
/// `hdsort` compares `get_id(hd[hd[pair]])`; this helper projects the same key through
/// `FreeFormalBindingRef` to keep C key semantics at the Rust boundary.
/// Invariant: key identity matches the name of the pair-head identifier.
fn binding_name_for_hdsort(heap: &Heap, binding_pair_ref: RawValue) -> HeapString {
    FreeFormalBindingRef::from_ref(binding_pair_ref)
        .identifier(heap)
        .get_name(heap)
}

impl VM {
    /// Reverse-association lookup over active alias entries by destination payload.
    ///
    /// Returns the source identifier (`old`) whose current value payload matches `target`.
    /// This mirrors Miranda's ALIASES scans used in the non-ID alias/pname load branch,
    /// where we need to recover an originating identifier name from a destination value.
    fn find_alias_source_id_for_target(
        &self,
        target: IdentifierValueRef,
    ) -> Option<IdentifierRecordRef> {
        let mut aliases = self.aliases;
        while let Some(alias_value) = aliases.pop_value(&self.heap) {
            let alias_ref: RawValue = alias_value.into();
            let alias_entry = AliasEntry::from_ref(alias_ref);
            let source_id = alias_entry.get_old_identifier_record(&self.heap);

            if let Some(found_val) = source_id.get_value(&self.heap) {
                if matches!(
                    found_val.get_data(&self.heap),
                    IdentifierValueData::Arbitrary(v) if v == Value::from(target.get_ref())
                ) {
                    return Some(source_id);
                }
            }
        }

        None
    }

    /// Finds the alias entry whose `old` identifier is `old_identifier`.
    ///
    /// Used by cyclic-alias handling in `load_defs` to locate and rewrite the hold payload
    /// for the exact alias record associated with the currently decoded identifier.
    fn find_alias_entry_for_old_identifier(
        &self,
        old_identifier: IdentifierRecordRef,
    ) -> Option<AliasEntry> {
        let mut aliases = self.aliases;
        while let Some(alias_value) = aliases.pop_value(&self.heap) {
            let alias_ref: RawValue = alias_value.into();
            let alias_entry = AliasEntry::from_ref(alias_ref);
            if alias_entry.old_identifier_matches(&self.heap, old_identifier) {
                return Some(alias_entry);
            }
        }

        None
    }

    /// Builds `fileinfo(current_file, 0)` for alias-error fallback application payloads.
    ///
    /// In the non-ID alias/pname load path, when an aliased destination remains undefined,
    /// Miranda applies the alias metadata to a neutral location marker so downstream DATAPAIR/
    /// application reduction can still produce a sensible diagnostic anchored to the current
    /// script, even when no precise source line is available at this decode stage.
    fn current_file_hereinfo_zero(&mut self) -> FileInfoRef {
        let current_file = self.current_file();
        FileInfoRef::from_script_file(&mut self.heap, current_file, 0)
    }

    /// Applies alias metadata to a neutral current-file location marker.
    ///
    /// Used in the non-ID alias/pname undefined-name fallback so reduction can report
    /// a coherent DATAPAIR/application diagnostic even when decode-time line precision is unavailable.
    fn apply_alias_fallback_hereinfo(
        &mut self,
        private_aka: Option<DataPair>,
    ) -> IdentifierValueRef {
        let file_info_ref = self.current_file_hereinfo_zero();
        let private_aka_value = private_aka.map_or(Combinator::NIL, Value::from);
        let applied_ref: RawValue = self
            .heap
            .apply_ref(private_aka_value, file_info_ref.into())
            .into();
        IdentifierValueRef::from_ref(applied_ref)
    }

    /// Builds `datapair(source_id, 0)` from reverse-associated alias source lookup.
    ///
    /// In the undefined-name non-ID alias/pname path, Miranda derives private-aka metadata
    /// from the source identifier associated with the destination payload, when available.
    fn private_aka_from_alias_source(&mut self, target: IdentifierValueRef) -> Option<DataPair> {
        self.find_alias_source_id_for_target(target)
            .map(|source_id| {
                IdentifierDefinitionRef::alias_metadata_from_source_identifier(
                    &mut self.heap,
                    source_id,
                )
            })
    }

    /// Ensures alias metadata exists for undefined non-ID alias fallback payloads.
    ///
    /// If `current_private_aka` is absent, this performs the same reverse ALIASES search Miranda
    /// uses to recover source metadata (`datapair(source_id, 0)`) from the alias destination.
    fn ensure_private_aka_for_undefined_target(
        &mut self,
        current_private_aka: Option<DataPair>,
        target: IdentifierValueRef,
    ) -> Option<DataPair> {
        if current_private_aka.is_none() {
            // Miranda derives fallback alias metadata by reverse-scanning ALIASES for a source
            // identifier whose current value payload equals this destination alias target.
            if let Some(found_private_aka) = self.private_aka_from_alias_source(target) {
                return Some(found_private_aka);
            }
        }

        current_private_aka
    }

    /// Writes undefined-name fallback payload to a private-name alias destination.
    ///
    /// The written value has form `apply(private_aka_or_nil, fileinfo(current_file, 0))` so
    /// downstream DATAPAIR/application reduction keeps a meaningful source anchor.
    fn write_pname_undefined_fallback_application(
        &mut self,
        private_name: PrivateNameRef,
        private_aka: Option<DataPair>,
    ) {
        let applied_value = self.apply_alias_fallback_hereinfo(private_aka);
        applied_value.store_in_private_name(&mut self.heap, private_name);
    }

    /// Binds `%free` include parameters.
    ///
    /// Heap-shape expectations:
    /// - `formal`: list of `cons(id, cons(datapair(original_name, 0), formal_type))`
    /// - `actual`: list of binding pairs keyed by `pair.head == actual_name_id`
    /// - `pair.tail`: bound payload value (value-binding payload, or AP/type payload for `==` bindings)
    ///
    /// Side-channel heap shapes written by this method:
    /// - `missing_parameter_bindings`: list of `datapair(original_name, 0)` values
    /// - `detritus_parameter_bindings`: list items are either `actual_name_id` values or
    ///   `cons(actual_name_id, datapair(formal_arity, actual_arity))` values
    /// - `free_binding_sets`: stack/list of `formal` list-reference values
    ///
    /// Invariant: matched names always update formal value payloads; mismatch diagnostics are
    /// accumulated without aborting this phase.
    pub(super) fn bindparams(&mut self, formal: Value, actual: Value) {
        // Both streams are sorted by binding name and walked in lockstep by lexical key.
        let nil = RawValue::from(Combinator::NIL);
        let mut formal_cursor: RawValue = formal.into();
        let mut actual_cursor: RawValue = actual.into();
        // Stage wrong-kind/wrong-arity entries, then append them into detritus at the end.
        let mut badkind: ConsList<Value> = ConsList::EMPTY;

        // Each call starts with fresh diagnostics and records this include's formal set.
        self.detritus_parameter_bindings = ConsList::EMPTY;
        self.missing_parameter_bindings = ConsList::EMPTY;
        self.free_binding_sets
            .push(&mut self.heap, formal_cursor.into());

        loop {
            // Advance through formals that have no matching actual yet.
            while formal_cursor != nil {
                // `formal_cursor` points to one cons cell in the formal stream.
                // `formal_binding_ref` is `cons(formal_id, formal_payload)`.
                let formal_binding_ref = self.heap[formal_cursor].head;
                let formal_payload_ref = self.heap[formal_binding_ref].tail;
                // `formal_payload` is `cons(original_name_datapair, formal_type)`.
                let formal_original_name_ref = self.heap[formal_payload_ref].head;
                let formal_name_ref = self.heap[formal_original_name_ref].head;
                let formal_name = self
                    .heap
                    .resolve_string(Value::from(formal_name_ref))
                    .unwrap_or_default();

                if actual_cursor == nil {
                    // No actuals left: every remaining formal is missing.
                    self.missing_parameter_bindings
                        .push(&mut self.heap, formal_original_name_ref.into());
                    formal_cursor = self.heap[formal_cursor].tail;
                    continue;
                }

                // `actual_cursor` points to one cons cell in the actual stream.
                // `actual_binding_ref` is keyed by `actual_binding_ref.head == actual_name_id`.
                let actual_binding_ref = self.heap[actual_cursor].head;
                let actual_name_ref = self.heap[actual_binding_ref].head;
                let actual_name =
                    IdentifierRecordRef::from_ref(actual_name_ref).get_name(&self.heap);

                if formal_name < actual_name {
                    // Formal key sorts before actual key: record missing formal and keep scanning formals.
                    self.missing_parameter_bindings
                        .push(&mut self.heap, formal_original_name_ref.into());
                    formal_cursor = self.heap[formal_cursor].tail;
                } else {
                    // Either equal (potential match) or actual is earlier (handled in outer flow).
                    break;
                }
            }

            if actual_cursor == nil {
                // Stop once actual stream is exhausted; missing formals were recorded above.
                break;
            }

            let actual_binding_ref = self.heap[actual_cursor].head;
            let actual_name_ref = self.heap[actual_binding_ref].head;
            let actual_name = IdentifierRecordRef::from_ref(actual_name_ref).get_name(&self.heap);

            if formal_cursor == nil {
                // Actual has no corresponding formal: detritus entry is just the actual name id.
                self.detritus_parameter_bindings
                    .push(&mut self.heap, actual_name_ref.into());
                actual_cursor = self.heap[actual_cursor].tail;
                continue;
            }

            let formal_binding_ref = self.heap[formal_cursor].head;
            let formal_payload_ref = self.heap[formal_binding_ref].tail;
            let formal_original_name_ref = self.heap[formal_payload_ref].head;
            let formal_name_ref = self.heap[formal_original_name_ref].head;
            let formal_name = self
                .heap
                .resolve_string(Value::from(formal_name_ref))
                .unwrap_or_default();

            if formal_name != actual_name {
                // Name mismatch with both streams present means actual name is not `%free` in this file.
                self.detritus_parameter_bindings
                    .push(&mut self.heap, actual_name_ref.into());
                actual_cursor = self.heap[actual_cursor].tail;
                continue;
            }

            // Matched names: compute kind/arity compatibility.
            // Convention: `-1` means "value binding" rather than "type binding".
            let formal_type_ref = self.heap[formal_payload_ref].tail;
            let mut formal_arity = -1;
            if formal_type_ref == RawValue::from(Type::Type) {
                // Type formal (`==` expected): extract arity from
                // `formal_id.value = cons(cons(arity, showfn), cons(kind, info))`.
                let formal_id_ref = self.heap[formal_binding_ref].head;
                let value_ref = self.heap[formal_id_ref].tail;
                if matches!(Value::from(value_ref), Value::Reference(_)) {
                    let arity_pair_ref = self.heap[value_ref].head;
                    if matches!(Value::from(arity_pair_ref), Value::Reference(_)) {
                        formal_arity = self.heap[arity_pair_ref].head;
                    }
                }
            }

            let mut actual_arity = -1;
            if self.heap[actual_binding_ref].tag == Tag::Ap {
                // AP-shaped actual indicates a type-binding payload;
                // read arity from the payload head pair.
                let type_value_ref = self.heap[actual_binding_ref].tail;
                if matches!(Value::from(type_value_ref), Value::Reference(_)) {
                    let arity_pair_ref = self.heap[type_value_ref].head;
                    if matches!(Value::from(arity_pair_ref), Value::Reference(_)) {
                        actual_arity = self.heap[arity_pair_ref].head;
                    }
                }
            }

            if formal_arity != actual_arity {
                // Wrong kind/arity is staged as
                // `cons(actual_name_id, datapair(formal_arity, actual_arity))`.
                let arity_pair: RawValue = self
                    .heap
                    .data_pair_ref(formal_arity.into(), actual_arity.into())
                    .into();
                let badkind_entry: RawValue = self
                    .heap
                    .cons_ref(actual_name_ref.into(), arity_pair.into())
                    .into();
                badkind.push(&mut self.heap, badkind_entry.into());
            }

            // For matched names, copy payload by writing `formal_id.tail = actual_binding.tail`.
            let formal_id_ref = self.heap[formal_binding_ref].head;
            let actual_payload_ref = self.heap[actual_binding_ref].tail;
            self.heap[formal_id_ref].tail = actual_payload_ref;

            // Streams advance together only on key match.
            formal_cursor = self.heap[formal_cursor].tail;
            actual_cursor = self.heap[actual_cursor].tail;
        }

        // Finalize by appending staged wrong-kind entries into the detritus channel.
        while let Some(badkind_entry) = badkind.pop_value(&self.heap) {
            self.detritus_parameter_bindings
                .push(&mut self.heap, badkind_entry);
        }
    }

    /// Gets the file name for the topmost file in the `vm.files` cons list, which _should_ be the current file.
    pub(super) fn current_file(&self) -> String {
        assert!(!self.files.is_empty());
        let f = self.files.head(&self.heap).unwrap();

        f.get_file_name(&self.heap)
    }

    /// Loads a compiled script from `in_file` for the `source_file`.
    pub(super) fn load_script(
        &mut self,
        in_file: &File,
        mut source_file: String,
        aliases: ConsList, // List of cons(new_id, old_id)
        parameters: Value,
        is_main_script: bool,
    ) -> Result<ConsList<FileRecord>, LoadScriptError> {
        /*
        extern word nextpn,ND,errline,algshfns,internals,freeids,includees,SGC;
        extern char *dicp, *dicq;
        word ch,files=NIL;
        TORPHANS=BAD_DUMP=0;
        CLASHES=NIL;
        */
        // This holds the return value.
        let mut files: ConsList<FileRecord> = ConsList::EMPTY;
        self.clashes = ConsList::EMPTY;

        if let Ok(prefix) = prefix(&self.heap, self.prefix_stack) {
            make_relative_path(&mut source_file, &prefix);
        }

        let mut file_bytes = vec![];
        {
            // Scope of f_reader
            let mut f_reader = BufReader::new(in_file);
            let bytes_read = f_reader
                .read_to_end(&mut file_bytes)
                .map_err(BytecodeDecodeError::unexpected_eof_with_source)?;
            if bytes_read < 16 {
                return Err(BytecodeDecodeError::unexpected_eof().into());
            }
        }
        // An iterator over the bytes of the file.
        let mut byte_iter = file_bytes.iter().peekable();

        // Parse the machine word size
        // First byte: `__WORDSIZE` (64 bits in my case, so `__WORDSIZE == 64 == 0x40`.)
        if *byte_iter.next().unwrap() as usize != WORD_SIZE {
            return Err(BytecodeDecodeError::ArchitectureMismatch.into());
        }

        // Parse the bytecode version
        // Second byte: `XVERSION`, the bytecode version. (Latest Miranda` == 83 == 0x53`)
        if *byte_iter.next().unwrap() as i32 != XVERSION {
            return Err(BytecodeDecodeError::WrongBytecodeVersion.into());
        }

        // Todo: Re-evaluate placement of alias installation relative to bytecode parsing.
        //       Blocker: load paths currently assume aliases are obeyed before any DEF decoding.
        //       Migration target: load-phase sequencing review in `src/vm/load.rs` + `src/vm/aliases.rs`.
        self.obey_aliases(aliases)?;

        // PNBASE = nextpn; // base for relocation of internal names in dump
        self.private_symbol_base_index = self.heap.private_symbols.len();
        // I think the idea is that the indices of the names in the serialized binary bytecode assume an empty private
        // symbol table, and that may not be the case if the current file is, say, included in another file. So indices
        // in the bytecode are actually _relative_ to the index of the first private symbol of the current file, and
        // adding private_name_base gives the absolute index in the vector of private names.

        // Reset per-load suppression bookkeeping before decoding this dump stream.
        self.suppressed = ConsList::EMPTY; // SUPPRESSED  // list of `-id' aliases successfully obeyed
        self.suppressed_type_aliases = ConsList::EMPTY; // TSUPPRESSED // list of -typename aliases (illegal just now)

        // The following parses the rest of the binary file. See
        // (Serialized Binary Representation.md)[../Serialized Binary Representation.md]
        // for more details. The first two bytes containing the word size and bytecode version have already been parsed.

        // Each pass through the while loop parses a single
        // [ [filename]
        //   [mtime]
        //   [shareable]
        //   [definition-list] ]
        // block. (Note: This is skipped over in the case of a syntax-error script.)
        while let Some(ch) = byte_iter.next() {
            if *ch == 0 {
                if !aliases.is_empty() {
                    self.unalias(aliases);
                }
                return Ok(files);
            }

            // If we encounter a `ch==1` *before* we ever even see a file, it is because it is the magic number for a
            // type-error script.
            // Todo: Hoist this pre-file type-error sentinel handling out of the main file loop.
            //       Blocker: current decode loop interleaves sentinels and file blocks with shared iterator state.
            //       Migration target: a staged dump-header parser before file-block iteration.
            if *ch == 1 && files.is_empty() {
                // The next w bytes (8 bytes) give the line number of the error.
                let error_line_prefetch: usize =
                    get_machine_word(&mut byte_iter.by_ref().copied())?;
                if is_main_script {
                    // But only save it if this is the main script.
                    self.error_line = error_line_prefetch;
                }
                // Todo: Preserve/report multiple type-error line entries when present.
                //       Blocker: VM currently stores a single `error_line` scalar.
                //       Migration target: aggregate type-error diagnostics in VM load state.
            }

            // Parse the file name and modified time

            let (mut filename, modified_time) = parse_filename_modified_time(&mut byte_iter)?;
            if let Ok(prefix) = prefix(&self.heap, self.prefix_stack) {
                make_absolute_path(&mut filename, &prefix);
            }

            // Parse sharable bit
            let sharable: bool = *byte_iter.next().unwrap_or(&0u8) == 1;

            #[cfg(feature = "debug")]
            println!("loading: {}({:?})", filename, modified_time);

            if files.is_empty() {
                // Is this the right dump file?
                if filename != source_file {
                    // I don't think unalias is needed here.
                    if !aliases.is_empty() {
                        self.unalias(aliases);
                    }
                    return Err(BytecodeDecodeError::WrongSourceFile.into());
                }
            }

            // Add a new `FileRecord` to `files`
            {
                // Scope of `new_defs`, `file_record`

                // Warning: `load_defs` side effects id's in namebuckets, cannot  be  undone  by
                //          `unload`  until  attached  to  global `files', so interrupts are disabled during
                //          `load_script` - see steer.c
                // For big dumps this may be too coarse - FIX
                let new_defs =
                    ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
                let file_record =
                    FileRecord::new(&mut self.heap, filename, modified_time, sharable, new_defs);
                files.push(&mut self.heap, file_record);
            }
        }

        // Dump of syntax error state
        // If we got here, we broke out of the while because we encountered `ch==0`. If we did so before we ever found a
        // file, it is because the `0` is the magic number for a syntax error script.
        if files.is_empty() {
            // The next w bytes (8 bytes) give the line number of the error.
            {
                // Scope of `error_line_prefetch`
                let error_line_prefetch: usize =
                    get_machine_word(&mut byte_iter.by_ref().copied())?;
                if is_main_script {
                    // But only save it if this is the main script.
                    self.error_line = error_line_prefetch;
                }
            }
            // Todo: Preserve/report multiple type-error line entries when present.
            //       Blocker: VM currently stores a single `error_line` scalar.
            //       Migration target: aggregate type-error diagnostics in VM load state.

            while let Some(_ch) = byte_iter.next() {
                // Parse filename and modified time.
                let (mut filename, modified_time) = parse_filename_modified_time(&mut byte_iter)?;
                if let Ok(prefix) = prefix(&self.heap, self.prefix_stack) {
                    make_absolute_path(&mut filename, &prefix);
                }

                if self.old_files.is_empty() {
                    // Is this the right dump file?
                    if filename != source_file {
                        // I don't think unalias is needed here.
                        if !aliases.is_empty() {
                            self.unalias(aliases);
                        }
                        return Err(BytecodeDecodeError::WrongSourceFile.into());
                    }
                }

                // Note: This mirrors the block inside the while-loop, except it adds to `old_files`, sets `sharable` to false,
                // and has empty `defienda`.
                {
                    // Scope of `file_record`
                    // Add a new `FileRecord` to `old_files`
                    // Todo: Confirm and document exact C parity rationale for `old_files` shape
                    //       (`share=false`, empty definienda) on syntax-error dumps.
                    //       Blocker: upstream C intent is implicit and currently under-documented.
                    //       Migration target: loadfile parity notes + this branch documentation.
                    let file_record = FileRecord::new(
                        &mut self.heap,
                        filename,
                        modified_time,
                        false,
                        ConsList::EMPTY,
                    );
                    self.old_files.push(&mut self.heap, file_record);
                }
            }

            if !aliases.is_empty() {
                self.unalias(aliases)
            }

            return Ok(files); // Equivalent to `return Ok(Nil);`
        }

        // Parse algebraic show functions
        {
            // scope of new_defs
            let new_defs: RawValue = self.load_defs(&mut byte_iter.by_ref().copied())?.into();
            self.algebraic_show_functions
                .append(&mut self.heap, new_defs);
        }

        // Parse [ND] or [True] (Are there type orphans?)
        if self.load_defs(&mut byte_iter.by_ref().copied())? == Combinator::True.into() {
            self.undefined_names = ConsList::EMPTY;
            self.unused_types = true;
        }

        // Parse DEF_X
        // Parse sui generis constructors
        // Todo: Reconcile this constructors block with durable binary-format docs.
        //       Blocker: documented format omits this decoded segment.
        //       Migration target: `Serialized Binary Representation` docs + evidence artifacts.
        {
            let new_defs: RawValue = self.load_defs(&mut byte_iter.by_ref().copied())?.into();
            self.sui_generis_constructors
                .append(&mut self.heap, new_defs);
        }

        // Parse DEF_X
        // Parse free_identifiers
        if is_main_script || self.included_files.is_empty() {
            self.free_identifiers =
                ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
        } else {
            let defs = self.load_defs(&mut byte_iter.by_ref().copied())?;
            let sorted_parameters = Value::from(hdsort_binding_list_ref(
                &mut self.heap,
                RawValue::from(parameters),
            ));
            self.bindparams(defs, sorted_parameters);
        }

        // Housekeeping: always restore alias diversions after decoding when alias list was non-empty.
        if !aliases.is_empty() {
            self.unalias(aliases)
        }

        // Parse DEF_X
        // Parse internals
        if is_main_script {
            self.internals =
                ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
        }

        Ok(files.reversed(&mut self.heap))
    }

    /// load a sequence of definitions terminated by `DEF_X`, or a single object terminated
    /// by `DEF_X`, from the byte stream `byte_iter`.
    ///
    /// The type of the returned value has to be an opaque type, because any serializable object can be returned.
    // Todo: Split `load_defs` result into a typed enum covering list-vs-single-object returns.
    //       Blocker: callers currently depend on opaque `Value` shape with mixed expectations.
    //       Migration target: typed `LoadDefsResult` and call-site migration in VM load/bytecode paths.
    pub(super) fn load_defs(
        &mut self,
        byte_iter: &mut dyn Iterator<Item = u8>,
    ) -> Result<Value, BytecodeDecodeError> {
        // Holds a list of definitions in cases where multiple definitions are read.
        let mut defs: ConsList = ConsList::EMPTY;
        // Holds the components of an item that have been read so far. When all of the components have been read, the
        // item will be created using `item_stack`.
        let mut item_stack: Vec<RawValue> = Vec::new();

        while let Some(ch) = byte_iter.next() {
            // Decode the byte
            let code = if let Some(code) = Bytecode::from_u8(ch) {
                code
            } else {
                // It is a RawValue.
                let v = if ch > 127 {
                    ch as RawValue + 256
                } else {
                    ch as RawValue
                };
                item_stack.push(v);
                continue;
            };

            match code {
                Bytecode::Char => {
                    let v = next(byte_iter)?;
                    item_stack.push(v as RawValue + 128);
                }

                Bytecode::TypeVariable => {
                    let v = next(byte_iter)?;
                    let type_var = self.heap.type_var_ref(Value::None, (v as RawValue).into());
                    item_stack.push(type_var.into());
                }

                Bytecode::Short => {
                    let encoded = next(byte_iter)?;
                    let integer = IntegerRef::decode_short_bytecode(&mut self.heap, encoded);
                    item_stack.push(integer.get_ref());
                }

                Bytecode::Integer => {
                    let mut words = vec![get_word_raw_value(byte_iter)?];
                    loop {
                        let word = get_word_raw_value(byte_iter)?;
                        words.push(word);
                        if word == -1 {
                            break;
                        }
                    }

                    let integer = IntegerRef::decode_int_x_bytecode(&mut self.heap, &words);
                    item_stack.push(integer.get_ref());
                }

                Bytecode::Double => {
                    let real_number = get_word_f64(byte_iter)?;

                    // Todo: Avoid lossy/non-obvious `f64`<->`RawValue` conversion boundary here.
                    //       Blocker: heap real-value API currently routes through `RawValue` conversion.
                    //       Migration target: direct `f64` real-number heap constructor path.
                    item_stack.push(self.heap.real_ref(real_number).into());
                }

                Bytecode::Unicode => {
                    let v = get_word_raw_value(byte_iter)?;

                    item_stack.push(self.heap.unicode_ref(v).into());
                }

                // Reads in the index of a private symbol. Differs from `Bytecode::PrtivateName1` in that it only reads two
                // bytes.
                Bytecode::PrivateName => {
                    let mut v: usize = get_u16_le(byte_iter)? as usize;

                    // See the notes for `private_symbol_base_index` in `vm.load_script()`.
                    // Turn relative index into absolute index.
                    v += self.private_symbol_base_index;

                    let ps = self.heap.get_nth_private_symbol_ref(v);
                    item_stack.push(ps.into());

                    // Todo: Rename/document private-symbol index flow consistently with current
                    //       `private_symbols` terminology.
                    //       Blocker: legacy naming (`pnvec`/private_names) is carried through comments.
                    //       Migration target: bytecode private-name decode docs and local comment cleanup.
                    /* Miranda source code is:
                    ch = getc(f);
                    ch = PNBASE + (ch | (getc(f) << 8));
                    *stackp++ = ch < nextpn ? pnvec[ch] : sto_pn(ch);
                    // efficiency hack for *stackp++ = sto_pn(ch);
                    */
                }

                // This differs from Bytecode::PrivateName in that is reads `MACHINE_WORD_SIZE` bytes.
                Bytecode::PrivateName1 => {
                    let mut v = get_machine_word(byte_iter)?;
                    // See the notes  for `private_symbol_base_index` in `vm.load_script()`.
                    v += self.private_symbol_base_index;

                    let ps = self.heap.get_nth_private_symbol_ref(v);
                    item_stack.push(ps.into());
                }

                Bytecode::Construct => {
                    let v: i16 = get_i16_le(byte_iter)?;
                    // Wrap the top value on the stack with the constructor.
                    if item_stack.is_empty() {
                        item_stack.push(Combinator::Nil.into());
                    }
                    let last_value = item_stack.last_mut().unwrap();
                    let wrapped_value =
                        ConstructorRef::new(&mut self.heap, v, (*last_value).into());
                    *last_value = wrapped_value.get_ref();

                    let new_value =
                        ConstructorRef::new(&mut self.heap, v, Value::from(wrapped_value));
                    item_stack.push(new_value.get_ref());
                }

                Bytecode::ReadVals => {
                    let previous_value = item_stack.pop().unwrap_or(Combinator::Nil.into());
                    let new_value = self
                        .heap
                        .start_read_vals_ref(Value::None, previous_value.into());
                    item_stack.push(new_value.into());

                    self.readvals_script_is_active = true;
                }

                Bytecode::ID => {
                    let name: String = parse_string(byte_iter)?;

                    // C `name()` behavior: look up an identifier by text, creating it if absent.
                    let id = self
                        .heap
                        .get_identifier(name.as_str())
                        .unwrap_or_else(|| self.heap.make_empty_identifier(name.as_str()));
                    let id_type = id.get_datatype(&self.heap);

                    if id_type == Type::New.into() {
                        // C FIX1 path: `new_t` marks a name-clash destination.
                        self.clashes.insert_ordered(&mut self.heap, id);
                        item_stack.push(NIL.into());
                    } else if id_type == Type::Alias.into() {
                        // Follow alias diversion (`id_val(id)`).
                        match id.get_value(&self.heap) {
                            None => return Err(BytecodeDecodeError::MalformedDefinition),
                            Some(id_value) => item_stack.push(id_value.get_ref()),
                        }
                    } else {
                        item_stack.push(id.get_ref());
                    }
                }

                Bytecode::AKA => {
                    let name = parse_string(byte_iter)?;
                    // Shape: `datapair(get_id(name()), 0)`.
                    let pair = IdentifierDefinitionRef::alias_metadata_from_source_name(
                        &mut self.heap,
                        name.as_str(),
                    );

                    item_stack.push(pair.get_ref());
                }

                Bytecode::Here => {
                    let first = next(byte_iter)?;
                    let file_path: String = if first == 0 {
                        // `first == 0` is shorthand for "current file".
                        self.current_file()
                    } else if first != b'/' {
                        // Transform path into absolute path.
                        let prefix_ref = self.prefix_stack.value_head(&self.heap).unwrap();
                        let prefix = self.heap.resolve_string(prefix_ref).unwrap();

                        format!(
                            "{}{}{}",
                            prefix,
                            char::from(first),
                            parse_string(byte_iter)?
                        )
                    } else {
                        format!("/{}", parse_string(byte_iter)?)
                    };
                    // C: `fileinfo(get_id(name()), line)`.
                    // Keep name-table side effects (`name()`) by ensuring an identifier exists,
                    // but store the file path as a canonical string in `FILEINFO.head`.
                    let _ = self.heap.make_empty_identifier(file_path.as_str());
                    let line_number = get_u16_le(byte_iter)? as usize;
                    let file_info = FileInfoRef::from_script_file(
                        &mut self.heap,
                        file_path,
                        line_number as isize,
                    );

                    item_stack.push(file_info.get_ref());
                }

                Bytecode::Definition => {
                    // This function (`load_defs`) loads a sequence of definitions terminated by DEF_X, or a single object
                    // terminated by DEF_X. When we encounter `DEF_X`, we need to construct the item whose components have
                    // accumulated in `item_stack`. Also, in some cases `DEF_X` does not signal to stop loading definitions. A
                    // summary of the uses of `DEF_X`:
                    //
                    // | Case | Use                                   | Form                            |
                    // |------|:--------------------------------------|:--------------------------------|
                    // | 1    | Terminate a list of definitions       | `[definition*] DEF_X`           |
                    // | 2    | Terminate a single identifier record  | `[val] [type] [who] [id] DEF_X` |
                    // | 3    | Terminate a private name              | `[val] [pname] DEF_X`           |
                    // | 4    | Precedes list of free identifiers     | `DEF_X [free_identifiers]`      |
                    // | 5    | Precedes definition list of internals | `DEF_X [definition-list]`       |

                    // The different cases can be partially distinguished by the number of items already read into `defs`.
                    match item_stack.len() {
                        0 => {
                            // Case 1: Definition list delimiter, signals the end of a definition list.
                            return Ok(defs.reversed(&mut self.heap).into());
                        }

                        1 => {
                            // Case 2: Object delimiter, signals the end of an identifier record.
                            let item: RawValue = item_stack.pop().unwrap();
                            return Ok(item.into());
                        }

                        2 => {
                            // Case 3: Private name delimiter, signals end of a private name.
                            let private_name = PrivateNameRef::from_ref(item_stack.pop().unwrap());
                            let value = IdentifierValueRef::from_ref(item_stack.pop().unwrap());
                            value.store_in_private_name(&mut self.heap, private_name);
                            defs.push(&mut self.heap, private_name.get_ref());
                        }

                        4 => {
                            let mut top_item = *item_stack.last().unwrap();

                            if self.heap[top_item].tag != Tag::Id {
                                if top_item == NIL.into() {
                                    // FIX1
                                    item_stack.clear();
                                    continue;
                                }

                                // An ID aliased to a pname.
                                // A pname has the form `strcons(index_in_private_names_vector, value)`,
                                // where value (I think) is an Identifier Record.

                                item_stack.pop(); // Value already in top_item

                                // This is the "id aliased to pname" branch (non-ID destination).
                                // Store the raw alias target for later `SUPPRESSED` membership checks.
                                let private_name = PrivateNameRef::from_ref(top_item);
                                let suppressed_target = IdentifierValueRef::from_ref(top_item);
                                let alias_target = suppressed_target;
                                self.suppressed.push(&mut self.heap, suppressed_target);

                                // In the non-ID alias destination branch, `who` is used only to
                                // recover optional alias metadata (`aka`) for undefined fallback
                                // reporting; we do not attach definition metadata to non-ID
                                // destinations directly.
                                let who_field =
                                    IdentifierDefinitionRef::from_ref(item_stack.pop().unwrap());
                                let mut private_aka = who_field.alias_metadata_pair(&self.heap);

                                // We keep the decoded type discriminator only for the
                                // non-synonym typed-name suppression rule.
                                let new_id_type = item_stack.pop().unwrap();

                                // The value of the private name.
                                let new_id_value =
                                    IdentifierValueRef::from_ref(item_stack.pop().unwrap());
                                new_id_value.store_in_private_name(&mut self.heap, private_name);

                                if new_id_value.typed_kind(&self.heap).is_some() {
                                    if new_id_value
                                        .is_non_synonym_typed_name(&self.heap, new_id_type)
                                    {
                                        // Suppressed typename
                                        if let Some(found_id) =
                                            self.find_alias_source_id_for_target(alias_target)
                                        {
                                            self.suppressed_type_aliases
                                                .push(&mut self.heap, found_id);
                                        }
                                    } else if new_id_value.is_undefined() {
                                        // Special kludge for undefined names, necessary only if we allow names specified
                                        // but not defined to be %included.
                                        private_aka = self.ensure_private_aka_for_undefined_target(
                                            private_aka,
                                            alias_target,
                                        );
                                        self.write_pname_undefined_fallback_application(
                                            private_name,
                                            private_aka,
                                        );
                                    }

                                    defs.push(&mut self.heap, top_item);
                                    continue;
                                }
                            }

                            // Control reaches here only when `top_item` is `Tag::Id`:
                            // the immediately preceding branch handles non-ID cases and `continue`s.
                            top_item = *item_stack.last().unwrap();
                            debug_assert_eq!(self.heap[top_item].tag, Tag::Id);
                            let new_id = IdentifierRecordRef::from_ref(top_item);
                            let new_id_type = new_id.get_datatype(&self.heap);
                            // The id's type will be an immediate value (not a reference) in the cases in the if condition below.
                            // Likewise for the id's value.
                            if new_id_type != Type::New.into()
                                && (new_id_type != Type::Undefined.into()
                                    || new_id.get_value_field(&self.heap)
                                        != Combinator::Undef.into())
                            {
                                if new_id_type == Type::Alias.into() {
                                    // cyclic aliasing
                                    let Some(alias_entry) =
                                        self.find_alias_entry_for_old_identifier(new_id)
                                    else {
                                        // Todo: Route this through structured nonfatal diagnostics.
                                        //       Blocker: no load-phase nonfatal diagnostic channel exists yet.
                                        //       Migration target: VM load diagnostics/reporting path in `src/vm/load.rs`.
                                        eprintln!(
                                            "impossible event in cyclic alias ({:?})",
                                            new_id.get_name(&self.heap)
                                        );
                                        item_stack.clear();
                                        continue;
                                    };
                                    defs.push(&mut self.heap, top_item);

                                    // Manipulating the alias, not an id.
                                    let definition = IdentifierDefinitionRef::from_ref(
                                        item_stack.pop().unwrap(),
                                    );
                                    let datatype: Value = item_stack.pop().unwrap().into();
                                    let value_ref = item_stack.pop().unwrap();
                                    let value = if value_ref == Combinator::Nil.into() {
                                        None
                                    } else {
                                        Some(IdentifierValueRef::from_ref(value_ref))
                                    };
                                    let restored_core = IdentifierCoreData {
                                        definition,
                                        datatype,
                                        value,
                                    };
                                    alias_entry.set_cyclic_hold_data(&mut self.heap, restored_core);
                                    continue;
                                }

                                self.clashes.insert_ordered(&mut self.heap, new_id);
                                item_stack.clear();
                            } else {
                                defs.push(&mut self.heap, top_item);

                                #[cfg(feature = "debug")]
                                println!("{} undumped", new_id.get_name(&self.heap));

                                item_stack.pop(); // top_item
                                                  // who
                                new_id.set_definition(
                                    &mut self.heap,
                                    IdentifierDefinitionRef::from_ref(item_stack.pop().unwrap()),
                                );
                                // type
                                new_id
                                    .set_datatype(&mut self.heap, item_stack.pop().unwrap().into());
                                // value
                                new_id.set_value(
                                    &mut self.heap,
                                    IdentifierValueRef::from_ref(item_stack.pop().unwrap()),
                                );
                            }
                        }

                        _ => {
                            // Todo: Preserve Miranda's fallback return behavior for this case.
                            //       Blocker: Rust currently treats it as malformed and errors out.
                            //       Migration target: explicit parity decision + typed return handling.
                            return Err(BytecodeDecodeError::MalformedDefinition);
                        }
                    } // end math on item_stack.len()
                } // end Bytecode::Definitions match branch

                Bytecode::Apply => {
                    // Miranda does not check length of item_stack
                    let top_item = item_stack.pop().unwrap();
                    let next_item = item_stack.pop().unwrap();
                    if next_item == Combinator::Read.into() && top_item == 0 {
                        item_stack.push(self.common_stdin.into());
                    } else if next_item == Combinator::ReadBin.into() && top_item == 0 {
                        item_stack.push(self.common_stdinb.into());
                    } else {
                        item_stack.push(
                            self.heap
                                .apply_ref(next_item.into(), top_item.into())
                                .into(),
                        );
                    }
                } // end Bytecode::Apply branch

                Bytecode::Cons => {
                    let head = item_stack.pop().unwrap();
                    let tail = item_stack.pop().unwrap();
                    item_stack.push(self.heap.cons_ref(head.into(), tail.into()).into());
                }
            } // end match on bytecode value
        }

        // Miranda returns `defs`, too.
        Err(BytecodeDecodeError::MalformedDefinition) // Miranda: "should unsetids"
    }
}

fn prefix(heap: &Heap, prefix_stack: ConsList) -> Result<String, ()> {
    let str_ref = prefix_stack.value_head(heap).ok_or(())?;
    heap.resolve_string(str_ref)
}

/// Convenience function that returns the next byte or `BytecodeDecodeError::UnexpectedEof`.
fn next(byte_iter: &mut dyn Iterator<Item = u8>) -> Result<u8, BytecodeDecodeError> {
    match byte_iter.next() {
        Some(ch) => Ok(ch),

        None => Err(BytecodeDecodeError::unexpected_eof()),
    }
}
