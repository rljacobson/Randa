use super::*;

impl VM {
    /// Clear out current script in preparation for reloading
    pub(super) fn unload(&mut self) {
        self.sorted = false;
        self.spec_location = ConsList::EMPTY;
        self.rv_script = false;
        self.algebraic_show_functions = ConsList::EMPTY;
        self.heap.private_symbols.clear();

        // Todo: Make `unsetids` take a `ConsList`.
        self.unset_ids(self.new_type_names);
        self.new_type_names = ConsList::EMPTY;

        self.unset_ids(self.free_ids);
        self.free_ids = ConsList::EMPTY;

        self.sui_generis_constructors = ConsList::EMPTY;
        self.includees = ConsList::EMPTY;
        self.type_abstractions = ConsList::EMPTY;
        self.undefined_names = ConsList::EMPTY;

        self.unset_ids(self.internals);
        self.internals = ConsList::EMPTY;

        while !self.files.is_empty() {
            // let files: ConsList   = ConsList::from_ref(self.files.into());
            // Guaranteed to unwrap because self.files != ConsList::EMPTY.
            let file: FileRecord = self.files.pop(&self.heap).unwrap();

            let definienda = file.get_definienda(&self.heap);
            if !definienda.is_empty() {
                self.unset_ids(ConsList::<IdentifierRecordRef>::from_ref(
                    definienda.get_ref(),
                ))
                // unsetids(fil_defs(hd[files]));
            }
            file.clear_definienda(&mut self.heap); // fil_defs(hd[files]) = NIL;
        }

        // Remember `self.mkinclude_files` is a nested list of lists.
        while !self.mkinclude_files.is_empty() {
            let mut file_list = self.mkinclude_files.pop(&self.heap).unwrap();

            while !file_list.is_empty() {
                let file: FileRecord = file_list.pop(&self.heap).unwrap();
                let definienda: ConsList = file.get_definienda(&self.heap);

                if !definienda.is_empty() {
                    // Todo: Miranda checks that the item has Tag::Id and just continues if not.
                    //       Do we expect everything in `id_list` to be an identifier?
                    self.unset_ids(ConsList::<IdentifierRecordRef>::from_ref(
                        definienda.get_ref(),
                    ))
                    // unsetids(fil_defs(hd[files]));
                }
            }
        }
    }

    pub(super) fn unset_ids(&mut self, mut id_list: ConsList<IdentifierRecordRef>) {
        while !id_list.is_empty() {
            // Todo: Miranda checks that the item has Tag::Id and just continues if not.
            //       Do we expect everything in `id_list` to be an identifier?
            let id_record: IdentifierRecordRef = id_list.pop(&mut self.heap).unwrap();
            id_record.unset_id(&mut self.heap);
            // should we remove from namebucket ?
        }
    }
}
