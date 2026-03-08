use super::*;

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
        let private_aka_value = private_aka.map_or(Combinator::NIL.into(), Value::from);
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
    // Todo: Promote `pname_ref` to a typed private-name wrapper once that proxy exists.
    fn write_pname_undefined_fallback_application(
        &mut self,
        pname_ref: RawValue,
        private_aka: Option<DataPair>,
    ) {
        let applied_value = self.apply_alias_fallback_hereinfo(private_aka);
        applied_value.store_in_private_name(&mut self.heap, pname_ref);
    }

    pub(super) fn hdsort(&mut self, _params: Value) -> Value {
        unimplemented!()
    }

    pub(super) fn bindparams(&mut self, _formal: Value, _actual: Value) {
        unimplemented!()
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
    ) -> Result<ConsList<FileRecord>, BytecodeError> {
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
                .map_err(BytecodeError::unexpected_eof_with_source)?;
            if bytes_read < 16 {
                return Err(BytecodeError::unexpected_eof());
            }
        }
        // An iterator over the bytes of the file.
        let mut byte_iter = file_bytes.iter().peekable();

        // Parse the machine word size
        // First byte: `__WORDSIZE` (64 bits in my case, so `__WORDSIZE == 64 == 0x40`.)
        if *byte_iter.next().unwrap() as usize != WORD_SIZE {
            return Err(BytecodeError::ArchitectureMismatch);
        }

        // Parse the bytecode version
        // Second byte: `XVERSION`, the bytecode version. (Latest Miranda` == 83 == 0x53`)
        if *byte_iter.next().unwrap() as i32 != XVERSION {
            return Err(BytecodeError::WrongBytecodeVersion);
        }

        // Todo: Why is this even here? It doesn't depend on the contents of the file!
        self.obey_aliases(aliases)?;

        // PNBASE = nextpn; // base for relocation of internal names in dump
        self.private_symbol_base_index = self.heap.private_symbols.len();
        // I think the idea is that the indices of the names in the serialized binary bytecode assume an empty private
        // symbol table, and that may not be the case if the current file is, say, included in another file. So indices
        // in the bytecode are actually _relative_ to the index of the first private symbol of the current file, and
        // adding private_name_base gives the absolute index in the vector of private names.

        // ToDo: Is this RE-setting the values of `suppressed*`? Might they be non-empty? Neither appear to be added to
        //       in this function.
        self.suppressed = ConsList::EMPTY; // SUPPRESSED  // list of `-id' aliases successfully obeyed
        self.suppressed_t = ConsList::EMPTY; // TSUPPRESSED // list of -typename aliases (illegal just now)

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
            // Todo: It is awkward to have this inside the loop.
            if *ch == 1 && files.is_empty() {
                // The next w bytes (8 bytes) give the line number of the error.
                let error_line_prefetch: usize =
                    get_machine_word(&mut byte_iter.by_ref().copied())?;
                if is_main_script {
                    // But only save it if this is the main script.
                    self.error_line = error_line_prefetch;
                }
                // Todo: What if there are multiple type errors?
            }

            // Parse the file name and modified time

            let (mut filename, modified_time) = parse_filename_modified_time(&mut byte_iter)?;
            if let Ok(prefix) = prefix(&self.heap, self.prefix_stack) {
                make_absolute_path(&mut filename, &prefix);
            }

            // Parse sharable bit
            let sharable: bool = *byte_iter.next().unwrap_or(&0u8) == 1;

            #[cfg(feature = "debug")]
            println("loading: {}({})", filename, _modified_time);

            if files.is_empty() {
                // Is this the right dump file?
                if filename != source_file {
                    // I don't think unalias is needed here.
                    if !aliases.is_empty() {
                        self.unalias(aliases);
                    }
                    return Err(BytecodeError::WrongSourceFile);
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
            // Todo: What if there are multiple type errors?

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
                        return Err(BytecodeError::WrongSourceFile);
                    }
                }

                // Note: This mirrors the block inside the while-loop, except it adds to `old_files`, sets `sharable` to false,
                // and has empty `defienda`.
                {
                    // Scope of `file_record`
                    // Add a new `FileRecord` to `old_files`
                    // Todo: WTF is `old_files`? Why are we using it instead of `files`? Why are we setting its `sharable` to
                    //       `false`, `defienda` to `NIL`? It is the most recent set of sources, in case of interrupted or failed
                    //       compilation. Why its definitions and sharable flag are not saved is a mystery.
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
        // Todo: This does not appear in the binary description
        {
            let new_defs: RawValue = self.load_defs(&mut byte_iter.by_ref().copied())?.into();
            self.sui_generis_constructors
                .append(&mut self.heap, new_defs);
        }

        // Parse DEF_X
        // Parse free_ids
        if is_main_script || self.includees.is_empty() {
            self.free_ids =
                ConsList::from_ref(self.load_defs(&mut byte_iter.by_ref().copied())?.into());
        } else {
            let defs = self.load_defs(&mut byte_iter.by_ref().copied())?;
            let sorted_parameters = self.hdsort(parameters);
            self.bindparams(defs, sorted_parameters);
        }

        // Housekeeping
        // Todo: Do we unalias unconditionally?
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
    // Todo: Some calls to `load_defs()` assume that a cons list, or even a `ConsList<IdentifierRecordRef>`, is returned,
    //       but it's clear that there are times when the return value is not a cons list.
    //       The code in Miranda for this is a complete and total mess.
    pub(super) fn load_defs(
        &mut self,
        byte_iter: &mut dyn Iterator<Item = u8>,
    ) -> Result<Value, BytecodeError> {
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
                item_stack.push(v.into());
                continue;
            };

            match code {
                Bytecode::Char => {
                    let v = next(byte_iter)?;
                    item_stack.push(v as RawValue + 128);
                }

                Bytecode::TypeVariable => {
                    let v = next(byte_iter)?;
                    let type_var = self
                        .heap
                        .type_var_ref(Value::None.into(), (v as RawValue).into());
                    item_stack.push(type_var.into());
                }

                // Todo: Do we want to support "small" integers? Yes for now, for Miranda compatibility.
                Bytecode::Short => {
                    let mut v = next(byte_iter)?;
                    if (v & 128u8) != 0 {
                        v = v | (!127u8);
                    }
                    item_stack.push(self.heap.small_int_ref(v as RawValue).into());
                }

                Bytecode::Integer => {
                    // Allow the very first value to be -1.
                    let mut v: RawValue = get_word_raw_value(byte_iter)?;
                    let int_list: RawValue = self.heap.integer_ref(v).into();

                    item_stack.push(int_list);

                    // The list of ints is constructed from the head to the tail, the opposite from if we used `push`.
                    // `cursor_ref` points to the cell whose tail is the next insertion point.
                    let mut cursor_ref: RawValue = int_list;
                    v = get_word_raw_value(byte_iter)?;

                    while v != -1 {
                        // Construct a boxed integer and store it in the tail of the previous boxed integer
                        let new_cell_ref: RawValue = self.heap.integer_ref(v).into();
                        self.heap[cursor_ref].tail = new_cell_ref;
                        // Read the next integer from the byte iterator
                        v = get_word_raw_value(byte_iter)?;
                        // Update the cursor to point to the tail of the newly constructed boxed integer
                        cursor_ref = new_cell_ref;
                    }
                }

                Bytecode::Double => {
                    let real_number = get_word_f64(byte_iter)?;

                    // Todo: We convert isize-->f64 and then f64-->isize. This is stupid.
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

                    // Todo: This is an index into `vm.private_names`?
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
                    *last_value = self.heap.constructor_ref(v, (*last_value).into()).into();

                    let new_value = self.heap.constructor_ref(v, (*last_value).into());
                    item_stack.push(new_value.into());
                }

                Bytecode::ReadVals => {
                    let previous_value = item_stack.pop().unwrap_or(Combinator::Nil.into());
                    let new_value = self
                        .heap
                        .start_read_vals_ref(Value::None.into(), previous_value.into());
                    item_stack.push(new_value.into());

                    self.rv_script = true;
                }

                Bytecode::ID => {
                    let name: String = parse_string(byte_iter)?;

                    // C `name()` behavior: look up an identifier by text, creating it if absent.
                    let id = self
                        .heap
                        .get_identifier(name.as_str())
                        .unwrap_or_else(|| self.heap.make_empty_identifier(name.as_str()));
                    let id_type = id.get_type(&self.heap);

                    if id_type == Type::New.into() {
                        // C FIX1 path: `new_t` marks a name-clash destination.
                        self.clashes.insert_ordered(&mut self.heap, id);
                        item_stack.push(NIL.into());
                    } else if id_type == Type::Alias.into() {
                        // Follow alias diversion (`id_val(id)`).
                        match id.get_value(&self.heap) {
                            None => return Err(BytecodeError::MalformedDef),
                            Some(id_value) => item_stack.push(id_value.get_ref()),
                        }
                    } else {
                        item_stack.push(id.get_ref());
                    }
                }

                Bytecode::AKA => {
                    let name = parse_string(byte_iter)?;
                    // C: `datapair(get_id(name()), 0)`.
                    // `name()` creates an identifier when missing, and `get_id(...)` extracts its
                    // canonical string pointer.
                    let source_id = self.heap.make_empty_identifier(name.as_str());
                    let pair = IdentifierDefinitionRef::alias_metadata_from_source_identifier(
                        &mut self.heap,
                        source_id,
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
                    // | 4    | Precedes list of free IDs             | `DEF_X [freeids]`               |
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
                            let item = item_stack.pop().unwrap();
                            let value = IdentifierValueRef::from_ref(item_stack.pop().unwrap());
                            value.store_in_private_name(&mut self.heap, item);
                            defs.push(&mut self.heap, item);
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
                                let pname_ref = top_item;
                                let suppressed_target = IdentifierValueRef::from_ref(top_item);
                                let alias_target = suppressed_target;
                                self.suppressed.push(&mut self.heap, suppressed_target);

                                // Todo: Why are we throwing away the who field?
                                let who_field =
                                    IdentifierDefinitionRef::from_ref(item_stack.pop().unwrap());
                                let mut private_aka = who_field.alias_metadata_pair(&self.heap);

                                // Todo: Why are we throwing away the type field?
                                let new_id_type = item_stack.pop().unwrap();

                                // The value of the private name.
                                let new_id_value =
                                    IdentifierValueRef::from_ref(item_stack.pop().unwrap());
                                new_id_value.store_in_private_name(&mut self.heap, pname_ref);

                                if new_id_value.typed_kind(&self.heap).is_some() {
                                    if new_id_value
                                        .is_non_synonym_typed_name(&self.heap, new_id_type)
                                    {
                                        // Suppressed typename
                                        if let Some(found_id) =
                                            self.find_alias_source_id_for_target(alias_target)
                                        {
                                            self.suppressed_t.push(&mut self.heap, found_id);
                                        }
                                    } else if new_id_value.is_undefined() {
                                        // Special kludge for undefined names, necessary only if we allow names specified
                                        // but not defined to be %included.
                                        private_aka = self.ensure_private_aka_for_undefined_target(
                                            private_aka,
                                            alias_target,
                                        );
                                        self.write_pname_undefined_fallback_application(
                                            pname_ref,
                                            private_aka,
                                        );
                                    }

                                    defs.push(&mut self.heap, top_item.into());
                                    continue;
                                }
                            }

                            // Control reaches here only when `top_item` is `Tag::Id`:
                            // the immediately preceding branch handles non-ID cases and `continue`s.
                            top_item = *item_stack.last().unwrap();
                            debug_assert_eq!(self.heap[top_item].tag, Tag::Id);
                            let new_id = IdentifierRecordRef::from_ref(top_item);
                            let new_id_type = new_id.get_type(&self.heap);
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
                                        // Todo: Make infrastructure for nonfatal errors.
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
                                new_id.set_type(&mut self.heap, item_stack.pop().unwrap().into());
                                // value
                                new_id.set_value(
                                    &mut self.heap,
                                    IdentifierValueRef::from_ref(item_stack.pop().unwrap()),
                                );
                            }
                        }

                        _ => {
                            // Todo: Miranda also returns the defs
                            return Err(BytecodeError::MalformedDef);
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
        return Err(BytecodeError::MalformedDef); // Miranda: "should unsetids"
    }
}

fn prefix(heap: &Heap, prefix_stack: ConsList) -> Result<String, ()> {
    let str_ref = prefix_stack.value_head(heap).ok_or(())?;
    heap.resolve_string(str_ref)
}

/// Convenience function that returns the next byte or `BytecodeError::UnexpectedEOF`.
fn next(byte_iter: &mut dyn Iterator<Item = u8>) -> Result<u8, BytecodeError> {
    match byte_iter.next() {
        Some(ch) => Ok(ch),

        None => Err(BytecodeError::unexpected_eof()),
    }
}
