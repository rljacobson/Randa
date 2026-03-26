use super::*;

impl VM {
    pub(super) fn mira_setup(&mut self) {
        self.setup_constants();
        self.setup_standard_types();

        // reset_pns(); // Setup private namespace. We use a vector that requires no setup.
        // Enters the primitive identifiers into the primitive environment. Called by "mira_setup".
        self.primlib()
    }

    /// Most of Miranda's `mira_setup()` is here.
    ///
    /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
    /// [`Heap::default()`](crate::data::heap::Heap::default()),
    /// [`Heap::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
    /// [`Heap::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
    pub(super) fn setup_constants(&mut self) {
        // Referenced below
        // Nil lives in `Heap` (`Heap::nill`) because some `Heap` functions use it
        self.void_ = self.heap.make_empty_identifier("()");
        IdentifierRecordRef::new(
            &mut self.heap,
            "()".to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Void.into(),
            None,
        );
        // *self.heap.id_type(self.void_) = Type::Void as RawValue;
        // *self.heap.id_val(self.void_)  = self.heap.constructor(0, self.void_).into();
        let value = ConstructorRef::new(&mut self.heap, 0, self.void_.into());
        self.void_.set_value(
            &mut self.heap,
            IdentifierValueRef::from_ref(value.get_ref()),
        );

        self.common_stdin = self.heap.apply_ref(Combinator::Read.into(), Value::from(0));
        self.common_stdinb = self
            .heap
            .apply_ref(Combinator::ReadBin.into(), Value::from(0));
        let readvals = self.heap.readvals_ref(0.into(), 0.into());
        self.cook_stdin = self.heap.apply_ref(
            readvals,
            // Todo: Should Offside be a combinator?
            Token::Offside.into(),
        );
        self.concat = self.heap.make_empty_identifier("concat");
        self.diagonalise = self.heap.make_empty_identifier("diagonalise");
        self.indent_fn = self.heap.make_empty_identifier("indent");
        self.listdiff_fn = self.heap.make_empty_identifier("listdiff");
        self.main_id = self.heap.make_empty_identifier("main"); // Miranda: change to magic scripts 19.11.2013
        self.message = self.heap.make_empty_identifier("sys_message");
        self.outdent_fn = self.heap.make_empty_identifier("outdent");
        self.showabstract = self.heap.make_empty_identifier("showabstract");
        self.bool_show_function = self.heap.make_empty_identifier("showbool");
        self.char_show_function = self.heap.make_empty_identifier("showchar");
        self.function_show_function = self.heap.make_empty_identifier("showfunction");
        self.list_show_function = self.heap.make_empty_identifier("showlist");
        self.internal_number_show_function = self.heap.make_empty_identifier("shownum1");
        self.pair_show_function = self.heap.make_empty_identifier("showpair");
        self.paren_show_function = self.heap.make_empty_identifier("showparen");
        self.string_show_function = self.heap.make_empty_identifier("showstring");
        self.void_show_function = self.heap.make_empty_identifier("showvoid");
        self.showwhat = self.heap.make_empty_identifier("showwhat");
        let stdout_ = self.heap.string("Stdout");
        self.stdout = ConstructorRef::new(&mut self.heap, 0, stdout_.into()).into();
    }

    /// This is tsetup() in Miranda.
    ///
    /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
    /// [`Heap::default()`](crate::data::heap::Heap::default()),
    /// [`Heap::setup_constants()`](crate::data::heap::Heap::setup_constants()), and
    /// [`Heap::setup_standard_types()`](crate::data::heap::Heap::setup_standard_types()).
    pub(super) fn setup_standard_types(&mut self) {
        self.numeric_function_type = self
            .heap
            .arrow_type_ref(Type::Number.into(), Type::Number.into());
        self.numeric_function2_type = self
            .heap
            .arrow_type_ref(Type::Number.into(), self.numeric_function_type);
        self.boolean_function_type = self
            .heap
            .arrow_type_ref(Type::Bool.into(), Type::Bool.into());
        self.boolean_function2_type = self
            .heap
            .arrow_type_ref(Type::Bool.into(), self.boolean_function_type);
        self.char_list_type = self.heap.list_type_ref(Type::Char.into());
        self.string_function_type = self
            .heap
            .arrow_type_ref(self.char_list_type, self.char_list_type);

        // tfnumnum is identical to self.numeric_function_type
        // let tfnumnum   = tf(num_t   , num_t);

        let number_list_type: Value = self.heap.list_type_ref(Type::Number.into());
        self.range_step_type =
            self.heap
                .arrow2_type_ref(Type::Number.into(), Type::Number.into(), number_list_type);

        self.range_step_until_type = self
            .heap
            .arrow_type_ref(Type::Number.into(), self.range_step_type);
    }

    /// The primdef function just creates an identifier on the heap and appends it to the primitive environment.
    pub(super) fn primitive_synonym_definition(&mut self, name: &str, type_: Type) {
        // self.primdef("num"  , make_typ(0, 0, IdentifierValueType::Synonym, Type::Number), Type::Type);
        let h_id_value = IdentifierValueRef::from_type_identifier_parts(
            &mut self.heap,
            TypeIdentifierValueParts {
                arity: 0,
                show_function: None,
                kind: IdentifierValueTypeKind::Synonym,
                info: type_.into(),
            },
        );
        let h_id = IdentifierRecordRef::new(
            &mut self.heap,
            name.to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Type.into(),
            Some(h_id_value),
        );

        self.primitive_environment.push(&mut self.heap, h_id);
    }

    /// The analog of `primitive_synonym_definition` for the constants `True` and `False`
    pub(super) fn primitive_bool_definition(&mut self, name: &str, tv: RawValue) {
        // self.primdef("True" , Value::Data(1), Type::Bool); // accessible only to 'finger'

        let h_value_data = IdentifierValueData::Arbitrary(Value::Data(tv));
        let h_value = IdentifierValueRef::new(&mut self.heap, h_value_data);
        let h_bool_constant = IdentifierRecordRef::new(
            &mut self.heap,
            name.to_string(),
            IdentifierDefinitionRef::undefined(),
            Type::Bool.into(),
            Some(h_value),
        );

        self.primitive_environment
            .push(&mut self.heap, h_bool_constant);
    }

    /// Enters the primitive identifiers into the primitive environment, sets up predefined ids not referred to by
    /// `parser.y`. Called by [VM::mira_setup()].
    pub(super) fn primlib(&mut self) {
        self.primitive_synonym_definition("num", Type::Number);
        self.primitive_synonym_definition("char", Type::Char);
        self.primitive_synonym_definition("bool", Type::Bool);

        self.primitive_bool_definition("True", 1); // accessible only to 'finger'
        self.primitive_bool_definition("False", 0); // likewise - FIX LATER

        // The preceding code is the equivalent of the following.
        // self.primdef("num"  , make_typ(0, 0, IdentifierValueType::Synonym, Type::Number), Type::Type);
        // self.primdef("char" , make_typ(0, 0, IdentifierValueType::Synonym, Type::Char)  , Type::Type);
        // self.primdef("bool" , make_typ(0, 0, IdentifierValueType::Synonym, Type::Bool)  , Type::Type);
        // self.primdef("True" , Value::Data(1), Type::Bool);
        // self.primdef("False", Value::Data(0), Type::Bool);
    }

    /// Adds the item (type, identifier, etc.) to the environment, i.e. cons it onto the definienda of the first
    /// item in the `files` cons list.
    pub(super) fn add_to_environment(&mut self, item: IdentifierRecordRef) {
        // The thread of pointers goes like this:
        //     self.files == cons(first, rest);
        //     head(self.files) == first
        //     first == cons(cons(fileinfo(filename, mtime), share), definienda)
        //     tail( first  ) == definienda
        if let Some(current_file) = self.files.head(&self.heap) {
            current_file.push_item_onto_definienda(&mut self.heap, item);
        }
    }

    /// A convenience method used by `privlib(..)` and `stdlib(..)`, see below. It creates an identifier
    /// with the given name, value, and datatype, constructing the value according to whether the name is
    /// that of a constructor (capitalized) or not, and then it adds the identifier to the environment.
    pub(super) fn predefine_identifier<T>(&mut self, name: &str, value: Value, datatype: T)
    where
        T: Into<Value>,
    {
        let id_ref = self.heap.predefine_identifier(name, value, datatype);

        self.add_to_environment(id_ref);
    }

    // Todo: Why aren't privlib, primlib, and stdlib combined into one and/or all called at once?
    // Todo: Is it ok that some of these are identical to the ones in `stdlib(..)`?
    ///  Adds some internally defined identifiers to the environment. Called when compiling `<prelude>`.
    pub(super) fn privlib(&mut self) {
        self.predefine_identifier("offside", Token::Offside.into(), self.char_list_type); // Used by `indent' in prelude
        self.predefine_identifier("changetype", Combinator::I.into(), Type::Wrong); // Type::Wrong to prevent being typechecked
        self.predefine_identifier("first", Combinator::Hd.into(), Type::Wrong);
        self.predefine_identifier("rest", Combinator::Tl.into(), Type::Wrong);
        // The following added to make prelude compilable without `stdenv`
        self.predefine_identifier("code", Combinator::Code.into(), Type::Undefined);
        let concat = self.heap.apply2(
            Combinator::FoldR.into(),
            Combinator::Append.into(),
            Combinator::Nil.into_value(),
        );
        self.predefine_identifier("concat", concat, Type::Undefined);
        self.predefine_identifier("decode", Combinator::Decode.into(), Type::Undefined);
        self.predefine_identifier("drop", Combinator::Drop.into(), Type::Undefined);
        self.predefine_identifier("error", Combinator::Error_.into(), Type::Undefined);
        self.predefine_identifier("filter", Combinator::Filter.into(), Type::Undefined);
        self.predefine_identifier("foldr", Combinator::FoldR.into(), Type::Undefined);
        self.predefine_identifier("hd", Combinator::Hd.into(), Type::Undefined);
        self.predefine_identifier("map", Combinator::Map.into(), Type::Undefined);
        self.predefine_identifier("shownum", Combinator::ShowNum.into(), Type::Undefined);
        self.predefine_identifier("take", Combinator::Take.into(), Type::Undefined);
        self.predefine_identifier("tl", Combinator::Tl.into(), Type::Undefined);
    }

    /// Called when compiling `<stdenv>`. Adds some internally defined identifiers to the environment
    pub(super) fn stdlib(&mut self) {
        self.predefine_identifier("arctan", Combinator::Arctan_Fn.into(), Type::Undefined);
        self.predefine_identifier("code", Combinator::Code.into(), Type::Undefined);
        self.predefine_identifier("cos", Combinator::Cos_Fn.into(), Type::Undefined);
        self.predefine_identifier("decode", Combinator::Decode.into(), Type::Undefined);
        self.predefine_identifier("drop", Combinator::Drop.into(), Type::Undefined);
        self.predefine_identifier("entier", Combinator::Entier_Fn.into(), Type::Undefined);
        self.predefine_identifier("error", Combinator::Error_.into(), Type::Undefined);
        self.predefine_identifier("exp", Combinator::Exp_Fn.into(), Type::Undefined);
        self.predefine_identifier("filemode", Combinator::FileMode.into(), Type::Undefined);
        self.predefine_identifier("filestat", Combinator::FileStat.into(), Type::Undefined); // Added Feb 91
        self.predefine_identifier("foldl", Combinator::FoldL.into(), Type::Undefined);
        self.predefine_identifier("foldl1", Combinator::FoldL1.into(), Type::Undefined); // New at release 2
        let huge_num = self.heap.real_ref(f64::MAX);
        // Note: Miranda says, "`hugenum' is the largest fractional number that can exist in this implementation (should
        // be around 1e308 for IEEE standard 64 bit floating point." We use the exact value provided by `f64::MAX`.
        self.predefine_identifier("hugenum", huge_num, Type::Undefined);
        self.predefine_identifier("last", Combinator::ListLast.into(), Type::Undefined);
        self.predefine_identifier("foldr", Combinator::FoldR.into(), Type::Undefined);
        self.predefine_identifier("force", Combinator::Force.into(), Type::Undefined);
        self.predefine_identifier("getenv", Combinator::GetEnv.into(), Type::Undefined);
        self.predefine_identifier("integer", Combinator::Integer.into(), Type::Undefined);
        self.predefine_identifier("log", Combinator::Log_Fn.into(), Type::Undefined);
        self.predefine_identifier("log10", Combinator::Log10_Fn.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("merge", Combinator::Merge.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("numval", Combinator::NumVal.into(), Type::Undefined);
        self.predefine_identifier("read", Combinator::StartRead.into(), Type::Undefined);
        self.predefine_identifier("readb", Combinator::StartReadBin.into(), Type::Undefined);
        self.predefine_identifier("seq", Combinator::Seq.into(), Type::Undefined);
        self.predefine_identifier("shownum", Combinator::ShowNum.into(), Type::Undefined);
        self.predefine_identifier("showhex", Combinator::ShowHex.into(), Type::Undefined);
        self.predefine_identifier("showoct", Combinator::ShowOct.into(), Type::Undefined);
        self.predefine_identifier("showfloat", Combinator::ShowFloat.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("showscaled", Combinator::ShowScaled.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("sin", Combinator::Sin_Fn.into(), Type::Undefined);
        self.predefine_identifier("sqrt", Combinator::Sqrt_Fn.into(), Type::Undefined);
        self.predefine_identifier("system", Combinator::Exec.into(), Type::Undefined); // New at release 2
        self.predefine_identifier("take", Combinator::Take.into(), Type::Undefined);
        let tiny_num = self.heap.real_ref(f64::MIN_POSITIVE);
        // Note: This is likely different from Miranda's `mktiny()`. Miranda says, "`tinynum' is
        // the smallest positive fractional number that can be distinguished from zero in this
        // implementation (should be around 1e-324 for IEEE standard 64 bit floating point)."
        // We instead use the exact value provided by `f64::MIN_POSITIVE`.
        self.predefine_identifier("tinynum", tiny_num, Type::Undefined); // New at release 2
        self.predefine_identifier("zip2", Combinator::Zip.into(), Type::Undefined);
        // New at release 2
    }
}
