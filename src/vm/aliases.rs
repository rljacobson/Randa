use super::*;

impl VM {
    /// Installs load-time alias diversions (`old -> new`) before bytecode body parsing.
    ///
    /// C parity reference: `miralib/data.c` `load_script` alias prologue.
    ///
    /// High-level phases:
    /// 1) For each alias entry `cons(new, old)`, save `old` core payload into hold,
    ///    then mutate `old` to alias type with value `new`.
    /// 2) Detect destination-name clashes.
    ///    If clashes exist, rollback immediately (`unalias`) and return `DestinationNameClash`.
    /// 3) Apply Miranda FIX1 (`new_t` marking) so the later missing-aliasee pass
    ///    in `unalias` preserves the intended diagnostics in clash+missing edge cases.
    pub(super) fn obey_aliases(&mut self, aliases: ConsList) -> Result<(), AliasInstallError> {
        if !aliases.is_empty() {
            // For each `old' install diversion to `new', i.e. make the `old` identifier an alias for the `new` identifier.
            // If alias is of form `-old`, `new' is a `pname` (private name).

            // C uses global `ALIASES` for follow-on paths (`unscramble`, missing-aliasee reporting,
            // and reverse-association checks during load-def decoding). We mirror that behavior by
            // retaining the list on `self.aliases` for later phases.
            self.aliases = aliases;
            let mut alias_iterator = aliases;

            while let Some(alias_entry) = pop_alias_entry(&self.heap, &mut alias_iterator) {
                // Alias-list entry shape at install time: `cons(new, old)`.
                // In Miranda, `new` may be either:
                // - an identifier (`Tag::Id`), or
                // - a private name/pname target (not `Tag::Id`).
                // We always install the diversion old -> new, regardless of which case this is.
                let new_target = alias_entry.get_new_target(&self.heap);
                let old: IdentifierRecordRef = alias_entry.get_old_identifier_record(&self.heap);
                let hold = IdentifierCoreRef::from_old_identifier(&mut self.heap, old);

                old.set_type(&mut self.heap, Type::Alias.into());
                // We make old an alias of new.
                old.set_value(&mut self.heap, new_target);

                // Name-clash detection only applies when `new` is actually an identifier.
                // C predicate:
                //   if (tag[new] == ID)
                //     if ((id_type(new) != undef_t || id_val(new) != UNDEF) && id_type(new) != alias_t)
                //       CLASHES = add1(new, CLASHES);
                if let Some(new_id) = alias_entry.get_new_identifier_record(&self.heap) {
                    let new_datatype = new_id.get_type(&self.heap);
                    let new_value_field = new_id.get_value_field(&self.heap);
                    if (new_datatype != Value::from(Type::Undefined)
                        || new_value_field != Combinator::Undef.into())
                        && new_datatype != Value::from(Type::Alias)
                    {
                        // This is a clash because alias installation requires the destination to be
                        // "not already defined" in the sense above. A defined/non-undef destination
                        // would conflict with diverting another name to it during load.
                        //
                        // C uses `add1` (set-like insertion without sorted-order guarantees).
                        // We keep stable ascending order in Rust for deterministic traversal/output;
                        // membership semantics remain set-like.
                        self.clashes.insert_ordered(&mut self.heap, new_id); //add1(&mut self.heap, new, &mut self.clashes);
                    }
                }

                // Replace alias head (`new`) with hold payload, yielding `cons(hold, old)`.
                // This is the write `hd[hd[a]] = hold`, used later by rollback (`unalias`/`unscramble`).
                // `hold` is intentionally not an identifier; it is the saved pre-alias core tuple.
                alias_entry.set_hold(&mut self.heap, hold);
            }

            if !self.clashes.is_empty() {
                // We unalias to restore all `old` identifiers and keep subsequent
                // reporting/state transitions coherent.
                self.unalias(aliases);
                return Err(AliasInstallError::DestinationNameClash); // BAD_DUMP = -2;
            }

            // The following block was inserted to deal with the pathological case that the destination of an alias (not
            // part of a cyclic alias) has a direct definition in the file and the aliasee is missing from the file - this is
            // both name clash and missing aliasee, but without fix the two errors cancel each other out and are unreported
            let mut alias_iterator = aliases; // Not technically an iterator.
            while let Some(alias_entry) = pop_alias_entry(&self.heap, &mut alias_iterator) {
                let old = alias_entry.get_old_identifier_record(&self.heap);
                // In this phase, `old` has already been rewritten as an alias, so `id_val(old)` is
                // the alias destination raw word (`new`) and can be ID or non-ID.
                let new_target: RawValue = old.get_value_field(&self.heap).into();

                // Miranda FIX1:
                //   if(tag[ch=id_val(old)]==ID)
                //   if(id_type(ch)!=alias_t)
                //      id_type(ch)=new_t;
                //
                // Interpretation: mark the destination identifier as `new` when it is an ID and not
                // already an alias. This ensures subsequent missing-aliasee reporting is preserved in
                // the pathological clash+missing case described above.
                if self.heap[new_target].tag == Tag::Id {
                    let new_id = IdentifierRecordRef::from_ref(new_target);
                    if new_id.get_datatype(&self.heap) != Type::Alias.into() {
                        new_id.set_type(&mut self.heap, Type::New.into());
                    }
                }
            } // FIX1
        }

        Ok(())
    }

    /// Remove old to new diversions installed in `obey_aliases`. (Miranda's `unscramble()`.)
    pub(super) fn unalias(&mut self, aliases: ConsList) {
        // `aliases` contains alias entries created during load, each conceptually `cons(new, old)`.
        // During `obey_aliases`, we mutate each alias entry so `head` becomes a temporary hold payload:
        //
        //   alias_entry = cons(hold, old)
        //   hold       = cons(old_who, cons(old_type, old_value))
        //
        // where `old_who`/`old_type`/`old_value` are the pre-alias fields of `old`.
        //
        // This first pass mirrors C `unscramble`'s first loop:
        // 1) restore each `old` identifier from `hold`,
        // 2) write `new` back into `alias_entry.head` (so second pass sees `cons(new, old)`).
        let mut cursor = aliases;

        while let Some(alias_ref_value) = cursor.pop_value(&self.heap) {
            let alias_ref: RawValue = alias_ref_value.into();
            let alias_entry = AliasEntry::from_ref(alias_ref);
            let old = alias_entry.get_old_identifier_record(&self.heap);
            let hold = alias_entry
                .get_hold(&self.heap)
                .expect("Alias entry is missing hold payload during unalias.");
            // During alias installation, `old.value` was overwritten with the alias destination `new`.
            // That value can be either an ID reference or a pname/non-ID payload.
            let new_target: IdentifierValueRef = match old.get_value(&self.heap) {
                Some(v) => v,
                _ => {
                    panic! {"Impossible value found in aliases."}
                }
            };

            // C: `hd[hd[aliases]] = new`.
            // In our representation this writes `new` back into `alias_entry.head`, restoring
            // the pair shape `cons(new, old)` for pass 2 missing-target checks.
            alias_entry.set_hold_value(&mut self.heap, new_target);

            let restored_core = hold.get_data(&self.heap);

            // id_who(old)=hd[hold], id_type(old)=hd[tl[hold]], id_val(old)=tl[tl[hold]]
            old.set_core_data(&mut self.heap, restored_core);
        } // end iter over `aliases`

        // Second pass mirrors C `unscramble`'s ALIASES loop.
        // We rebuild `self.aliases` to contain only "missing aliasees" (`old` identifiers whose destinations
        // cannot be resolved after rollback and suppression/clash handling).
        // This list is consumed later by error-reporting and follow-on load logic.
        let mut missing_aliases: ConsList = ConsList::EMPTY;

        cursor = self.aliases;
        while let Some(alias_entry) = pop_alias_entry(&self.heap, &mut cursor) {
            let old_id = alias_entry.get_old_identifier_record(&self.heap);
            let new_target = alias_entry.get_new_target(&self.heap);

            if let Some(new_id) = alias_entry.get_new_identifier_record(&self.heap) {
                // FIX1
                if new_id.get_type(&self.heap) == Type::New.into() {
                    new_id.set_type(&mut self.heap, Type::Undefined.into());
                }

                if new_id.get_type(&self.heap) == Type::Undefined.into() {
                    missing_aliases.push(&mut self.heap, old_id.get_ref());
                } else if !self.clashes.contains(&self.heap, new_id) {
                    let new_def: IdentifierDefinitionRef = new_id.get_definition(&self.heap);
                    let new_def_data: IdentifierDefinitionData = new_def.get_data(&self.heap);

                    // Install aka info in new
                    match new_def_data {
                        IdentifierDefinitionData::Alias { .. } => { /* pass */ }

                        // If it's not an alias
                        _ => {
                            // C: id_who(new) = cons(datapair(get_id(old), 0), id_who(new));
                            // The constructor encapsulates this alias-metadata shape.
                            let aliased_definition = IdentifierDefinitionRef::from_alias_source(
                                &mut self.heap,
                                old_id,
                                new_def,
                            );
                            new_id.set_definition(&mut self.heap, aliased_definition);
                        }
                    }
                }
            } else {
                // C: `if(tag[new] != ID) { if(!member(SUPPRESSED,new)) missing_aliases=cons(old,...); continue; }`
                //
                // Interpretation:
                // - `new` is not an identifier object (typically a pname/private-name target), so we cannot
                //   attach aka/who metadata to it.
                // - If this non-ID target was not intentionally suppressed, the missing aliasee is `old`.
                //
                // `self.suppressed` stores destination targets as identifier-value refs because
                // this branch is specifically `tag[new] != ID`.
                if !self.suppressed.contains(&self.heap, new_target) {
                    missing_aliases.push(&mut self.heap, old_id.get_ref())
                }
                continue;
            }
        }

        // Transmits info about missing aliasees
        self.aliases = missing_aliases;
    }
}

/// Pops one alias entry from `cursor` and projects it as `AliasEntry`.
///
/// Alias lists are represented as cons-list payloads containing raw references to
/// `cons(new_target, old_identifier)` tuples. This helper keeps the raw extraction and
/// proxy projection at one boundary point for alias-phase loops.
// Todo: Introduce a typed alias-list iterator/adaptor for alias entry traversal.
//       Blocker: `ConsList` currently exposes generic payload traversal only.
//       Migration target: typed alias-list iterator in `src/data/api/*` and this boundary.
fn pop_alias_entry(heap: &Heap, cursor: &mut ConsList) -> Option<AliasEntry> {
    cursor
        .pop_value(heap)
        .map(Into::into)
        .map(AliasEntry::from_ref)
}
