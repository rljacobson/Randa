/*!

See the `values` module for more information about how data is encoded in a `HeapCell`.

 */

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, IndexMut};

use crate::constants::SIGNBIT;
use crate::data::api::{
    ApNodeRef, ConstructorRef, HeapObjectProxy, IdentifierDefinitionRef, IdentifierRecordRef,
};
use crate::{
    compiler::Token,
    constants::INIT_SPACE,
    data::{
        tag::Tag,
        types::Type,
        values::{RawValue, Value},
        Combinator, ATOM_LIMIT,
    },
};

type ValueResult = Result<Value, ()>;

// Todo: Give these a home. Seems like they should be combinators.
static GENERATOR: RawValue = 0;
static GUARD: RawValue = 1;
static REPEAT: RawValue = 2;

/// The fundamental unit of data for data that lives in the heap.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct HeapCell {
    pub tag: Tag,
    pub head: RawValue,
    pub tail: RawValue,
}

impl HeapCell {
    pub fn new<R1: Into<RawValue>, R2: Into<RawValue>>(tag: Tag, head: R1, tail: R2) -> HeapCell {
        // Trivial implementation, but nice to have in case we decide we need a nontrivial implementation in the future.
        HeapCell {
            tag,
            head: head.into(),
            tail: tail.into(),
        }
    }
}

// #[derive(Clone)]
pub struct Heap {
    // Heap limits
    // ToDo: Should this be replaced by `self.heap.capacity`?
    /// This is SPACE in Miranda and is initialized as `SPACE = INIT_SPACE`. See also [`DEFAULT_SPACE`](DEFAULT_SPACE),
    /// [`SPACE_LIMIT`](SPACE_LIMIT), [`INIT_SPACE`](INIT_SPACE), and [`BIG_TOP`](BIG_TOP).
    heap_space: usize,

    data: Vec<HeapCell>,
    strings: Vec<String>,
    /// String interning map: text -> `Tag::String` heap reference.
    // Todo: Could we use the string's index in `heap.strings` for the symbol table key instead of having a second copy
    //       of the string?
    pub(crate) symbol_table: HashMap<String, Value>,

    /// Identifier lookup map: name -> `Tag::Id` heap reference.
    pub(crate) identifier_table: HashMap<String, RawValue>,

    /// The private environment is represented as a vector of references to items of the form `strcons(index, value)`,
    /// where `index` is the index of the item in `private_symbols`, and `value` is (a reference to) the item. This is
    /// one of the few things Miranda doesn't store as a cons list in the heap.
    pub(crate) private_symbols: Vec<RawValue>, // pnvec

    /// Special heap value NILL
    pub(crate) nill: Value,
}

impl Debug for Heap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Heap{{size={}, strings={}}}",
            self.data.len(),
            self.strings.len()
        )
    }
}

impl Default for Heap {
    /// Setup and initialization of [`Heap`](crate::data::heap::Heap) occurs in
    /// [`VM::build()`](crate::vm::VM::build()),
    /// [`VM::setup_constants()`](crate::vm::VM::setup_constants()), and
    /// [`VM::setup_standard_types()`](crate::vm::VM::setup_standard_types()).
    fn default() -> Self {
        let mut heap = Heap {
            // Heap limits
            heap_space: INIT_SPACE, // SPACE = INIT_SPACE;

            data: Vec::with_capacity(INIT_SPACE),
            strings: vec![],
            symbol_table: HashMap::new(),
            identifier_table: HashMap::new(),
            private_symbols: Vec::with_capacity(200), // This is the initial capacity in Miranda.

            nill: Value::Uninitialized,
        };

        // Todo: Why have a reference to this instead of just using Combinator::Nil?
        //       That's what Miranda does, but is there a reason? To have it on the heap, but why?
        //       Possible answer: So it can be used in contexts that allow Combinator values that are not actually
        //       combinators.
        // Nill lives in `Heap` because some `Heap` functions use it.
        heap.nill = heap.cons_ref(Value::Token(Token::Constant), Value::from(Combinator::Nil));

        heap
    }
}

// region Index impls
impl Index<RawValue> for Heap {
    type Output = HeapCell;

    fn index(&self, index: RawValue) -> &Self::Output {
        // RawValues are the `index + ATOM_LIMIT`
        let idx = index - ATOM_LIMIT;
        if idx >= 0 {
            &self.data[idx as usize]
        } else {
            unreachable!(
                "Tried to index into the heap with a RawValue that doesn't represent a reference."
            )
        }
    }
}

impl IndexMut<RawValue> for Heap {
    fn index_mut(&mut self, index: RawValue) -> &mut Self::Output {
        // RawValues are the `index + ATOM_LIMIT`
        let idx = index - ATOM_LIMIT;
        if idx >= 0 {
            &mut self.data[idx as usize]
        } else {
            unreachable!(
                "Tried to index into the heap with a RawValue that doesn't represent a reference."
            )
        }
    }
}

impl Index<Value> for Heap {
    type Output = HeapCell;

    fn index(&self, value: Value) -> &Self::Output {
        if let Value::Reference(idx) = value {
            &self.data[idx as usize]
        } else {
            unreachable!(
                "Tried to use a non-Value::Reference variant as a reference. This is a bug."
            )
        }
    }
}

impl IndexMut<Value> for Heap {
    fn index_mut(&mut self, value: Value) -> &mut Self::Output {
        if let Value::Reference(idx) = value {
            &mut self.data[idx as usize]
        } else {
            unreachable!(
                "Tried to use a non-Value::Reference variant as a reference. This is a bug."
            )
        }
    }
}
// endregion

impl Heap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Reports the `heap_space + ATOM_LIMIT`.
    fn top(&self) -> usize {
        // Todo: Should this just report `self.heap.capacity + ATOM_LIMIT`?
        self.heap_space + ATOM_LIMIT as usize
    }

    // region Generic read/write functions

    /// A convenience method used by "privlib" and "stdlib", see below. It creates an identifier with the given name,
    /// value, and datatype, constructing the value according to whether the name is that of a constructor
    /// (capitalized) or not.
    pub(crate) fn predefine_identifier<T>(
        &mut self,
        name: &str,
        value: Value,
        data_type: T,
    ) -> IdentifierRecordRef
    where
        T: Into<Value>,
    {
        let id_ref = IdentifierRecordRef::new(
            self,
            name.to_string(),
            IdentifierDefinitionRef::undefined(),
            data_type.into(),
            None, // Value to be filled in after we get a reference to this identifier.
        );

        let id_value: Value = if is_capitalized(name) {
            let constructor_index: i16 = Into::<RawValue>::into(value) as i16;
            ConstructorRef::new(self, constructor_index, id_ref.get_ref().into()).into()
        } else {
            value
        };

        self[id_ref.get_ref()].tail = id_value.into();

        id_ref
    }

    /// Registers `name` in the symbol table and strings list and creates a "bare bones"
    /// identifier structure on the heap for the given name, returning a reference to the heap object.
    pub fn make_empty_identifier(&mut self, name: &str) -> IdentifierRecordRef {
        if let Some(existing) = self.identifier_table.get(name) {
            return IdentifierRecordRef::from_ref(*existing);
        }

        // return self.make(ID, cons(strcons(p1, NIL), undef_t), UNDEF);
        IdentifierRecordRef::new(
            self,
            name.to_string(),
            IdentifierDefinitionRef::from_ref(0),
            Type::Undefined.into(),
            None,
        )
    }

    pub fn get_identifier(&self, name: &str) -> Option<IdentifierRecordRef> {
        self.identifier_table
            .get(name)
            .map(|reference| IdentifierRecordRef::from_ref(*reference))
    }

    pub(crate) fn register_identifier_name(&mut self, name: &str, reference: RawValue) {
        self.identifier_table.insert(name.to_string(), reference);
    }

    /*
    fn make(&mut self, t: u8, x: u16, y: u16) -> u16 {
      let mut listp = 0;

      while self.data[listp].tag as i32 > 0 {
        listp += 1;
      }
      // Todo: It's not clear how any of this GC stuff will apply.

      // find next cell with zero or negative tag (=unwanted)
      if listp == self.top() {
        if self.heap_space != SPACE_LIMIT {
          if !compiling {
            self.heap_space = SPACE_LIMIT;
          } else if claims <= self.heap_space / 4 && nogcs > 1 {
            // during compilation we raise false ceiling whenever residency reaches 75% on 2 successive gc's
            static mut wait: u16 = 0;
            let sp = self.heap_space;
            if wait > 0 {
              wait -= 1;
            } else {
              self.heap_space += self.heap_space / 2;
              wait = 2;
              self.heap_space = 5000 * (1 + (self.heap_space - 1) / 5000); /* round upwards */
            }
            if self.heap_space > SPACE_LIMIT {
              self.heap_space = SPACE_LIMIT;
            }
            if atgc && self.heap_space > sp {
              println!("\n<<increase heap from {} to {}>>\n", sp, self.heap_space);
            }
          }
        }

        // If after raising the false space ceiling we still do not have a free cell, then run  the GC and try again.
        if listp == TOP {
          gc();

          if t > Tag::StrCons as TagRepresentationType {
            mark(x);
          }
          if t >= INT {
            mark(y);
          }
          return make(t, head, tail);
        }
      }
      claims += 1;
      tag[listp] = t;
      hd[listp] = x;
      tl[listp] = y;
      listp
    }
    */

    /// Resolves a reference to the `HeapCell` it points to.
    pub fn resolve(&self, value: Value) -> Result<HeapCell, ()> {
        match value {
            Value::Reference(r) => Ok(self.data[r as usize]),

            _ => {
                println!("Failed to resolve: {:?}", value);
                Err(())
            }
        }
    }

    /// Resolves a reference to the `HeapCell` it points to without checking if the value is a reference.
    /// The only use for this version of `resolve` is to avoid unwrapping an `Option` in cases where you
    /// know the value type.
    pub fn resolve_unchecked(&self, value: Value) -> HeapCell {
        match value {
            Value::Reference(r) => self.data[r as usize],

            _ => panic!("Attempted to dereference a non-reference value."),
        }
    }

    /// Pushes the given cell into the heap and returns a reference to the `HeapCell` in the heap (the index of the `HeapCell` in `heap.data`).
    fn put_cell(&mut self, cell: HeapCell) -> RawValue {
        let idx = self.data.len();
        self.data.push(cell);
        idx as RawValue
    }

    /// Typed wrapper around [`Heap::put_cell`] that returns a typed heap reference.
    pub fn put_cell_ref(&mut self, cell: HeapCell) -> Value {
        Value::Reference(self.put_cell(cell))
    }

    /// Same as `put_cell`, but creates the `HeapCell` as well. This is Miranda's `make`, roughly speaking.
    fn put(&mut self, tag: Tag, head: RawValue, tail: RawValue) -> RawValue {
        self.put_cell(HeapCell::new(tag, head, tail))
    }

    /// Typed wrapper around [`Heap::put`] that returns a typed heap reference.
    pub fn put_ref(&mut self, tag: Tag, head: Value, tail: Value) -> Value {
        Value::Reference(self.put(tag, head.into(), tail.into()))
    }

    /// Dereference the given reference, and if the result has `tag`, returns the cell. If `reference` isn't a
    /// reference, or if the tag of the referent is not `tag`, returns `Err(())`.
    pub fn expect(&self, tag: Tag, reference: Value) -> Result<HeapCell, ()> {
        if let Value::Reference(idx) = reference {
            let found_cell = self.data[idx as usize];
            if found_cell.tag == tag {
                return Ok(found_cell);
            } else {
                println!("Expected: {:?}\nFound: {:?}", tag, found_cell.tag);
            }
        }
        // Else:
        Err(())
    }

    /// If maybe_reference is a reference, dereference it, and check that its tag is `tag`. If so, return the head.
    /// Otherwise, return None.
    pub fn expect_head(&self, tag: Tag, reference: Value) -> ValueResult {
        if let Value::Reference(idx) = reference {
            let cell = self.data[idx as usize];
            if cell.tag == tag {
                return Ok(cell.head.into());
            }
        }
        // Else:
        println!("Expected: {:?}\nFound: {:?}", tag, reference);
        Err(())
    }

    /// If maybe_reference is a reference, dereference it, and check that its tag is `tag`. If so, return the tail.
    /// Otherwise, return None.
    ///
    /// Because `expect_tail` takes an `Option<Value>`, it is composable with itself.
    pub fn expect_tail(&self, tag: Tag, reference: Value) -> ValueResult {
        if let Value::Reference(idx) = reference {
            let cell = self.data[idx as usize];
            if cell.tag == tag {
                return Ok(cell.tail.into());
            }
        }
        // Else:
        println!("Expected: {:?}\nFound: {:?}", tag, reference);
        Err(())
    }

    /// Resolve a string reference (or legacy string index payload) to its string value.
    pub fn resolve_string(&self, value: Value) -> Result<String, ()> {
        match value {
            Value::Reference(reference) => {
                let cell = self.data.get(reference as usize).ok_or(())?;
                if cell.tag != Tag::String {
                    return Err(());
                }
                self.strings.get(cell.head as usize).cloned().ok_or(())
            }
            Value::Data(index) => self.strings.get(index as usize).cloned().ok_or(()),
            _ => Err(()),
        }
    }

    /// Create a new private symbol with `value` and return a reference to it.
    fn make_private_symbol(&mut self, value: Value) -> RawValue {
        let new_symbol: RawValue = self
            .strcons_ref(Value::Data(self.private_symbols.len() as RawValue), value)
            .into();
        self.private_symbols.push(new_symbol);

        new_symbol
    }

    pub fn make_private_symbol_ref(&mut self, value: Value) -> Value {
        Value::Reference(self.make_private_symbol(value))
    }

    /// If `n < private_symbols.len()`, returns `private_symbols[n]`. If `n >= private_symbols.len()`, creates new
    /// "empty" private symbols for all j with `private_symbols.len() <= j <= n` and returns `private_symbols[n]`.
    //  ToDo: This sounds dumb.
    fn get_nth_private_symbol(&mut self, n: usize) -> RawValue {
        let length = self.private_symbols.len();
        if n >= length {
            for j in length..=n {
                let new_symbol: RawValue = self
                    .strcons_ref(Value::Data(j as RawValue), Value::from(Combinator::Undef))
                    .into();
                self.private_symbols.push(new_symbol);
            }
        }

        self.private_symbols[n]
    }

    pub fn get_nth_private_symbol_ref(&mut self, n: usize) -> Value {
        Value::Reference(self.get_nth_private_symbol(n))
    }

    // endregion

    /* data abstractions for identifiers (see also sto_id() in data.c)
    #define get_id(x) ((char *)hd[hd[hd[x]]])
    #define id_who(x) tl[hd[hd[x]]]
    #define id_type(x) tl[hd[x]]
    #define id_val(x) tl[x]
    #define isconstructor(x) (tag[x]==ID&&isconstrname(get_id(x)))
    #define isvariable(x) (tag[x]==ID&&!isconstrname(get_id(x)))
    */

    // region Heap cell creation convenience functions

    /// Registers the string in the strings list, creates the string object on the heap, and places a reference to the
    /// heap string object into the symbol table, returning the heap reference.
    pub fn string<T: Into<String>>(&mut self, text: T) -> RawValue {
        let text: String = text.into();

        // Cannot use `Entry` API because we would need a second mutable borrow of self to create the ID on the heap.
        if let Some(value) = self.symbol_table.get(&text) {
            return (*value).into();
        }

        let idx = self.strings.len();
        let string_ref = self.put(
            Tag::String,
            Value::Data(idx as RawValue).into(),
            Value::None.into(),
        );

        self.symbol_table.insert(text.clone(), string_ref.into());
        self.strings.push(text);

        string_ref
    }

    fn small_int(&mut self, value: RawValue) -> RawValue {
        let v = match value < 0 {
            true => SIGNBIT | (-value),
            false => value,
        };

        self.put(Tag::Int, v, 0)
    }

    pub fn small_int_ref(&mut self, value: isize) -> Value {
        Value::Reference(self.small_int(value))
    }

    /// Creates a `HeapCell` with tag `Tag::Int`, head `value`, and tail `NIL`.
    /// This does _not_ create a cons list of ints.
    fn integer(&mut self, value: RawValue) -> RawValue {
        self.put(Tag::Int, value, 0)
    }

    pub fn integer_ref(&mut self, value: isize) -> Value {
        Value::Reference(self.integer(value))
    }

    fn identifier(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Id, head, tail)
    }

    pub fn identifier_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.identifier(head.into(), tail.into()))
    }

    fn data_pair(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::DataPair, head, tail)
    }

    pub fn data_pair_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.data_pair(head.into(), tail.into()))
    }

    fn file_info(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::FileInfo, head, tail)
    }

    pub fn file_info_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.file_info(head.into(), tail.into()))
    }

    fn constructor(&mut self, n: i16, x: Value) -> RawValue {
        self.put(Tag::Constructor, n as RawValue, x.into())
    }

    pub fn constructor_ref(&mut self, n: i16, x: Value) -> Value {
        Value::Reference(self.constructor(n, x))
    }

    fn strcons(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::StrCons, head, tail)
    }

    /// Typed wrapper around [`Heap::strcons`] that returns a typed heap reference.
    pub fn strcons_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.strcons(head.into(), tail.into()))
    }

    fn cons(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Cons, head, tail)
    }

    /// Typed wrapper around [`Heap::cons`] that returns a typed heap reference.
    pub fn cons_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.cons(head.into(), tail.into()))
    }

    fn lambda(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Lambda, head, tail)
    }

    pub fn lambda_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.lambda(head.into(), tail.into()))
    }

    /// Lowers a function-form lhs application spine into nested lambdas around `body` and returns
    /// the lowered lhs/body pair. This exists so parser-facing definition lowering can own
    /// Miranda's fnform rewrite through one typed helper instead of duplicating `Tag::Ap` walks
    /// inline. The invariant is that non-constructor `f x y = body` lowers to lhs `f` and body
    /// `lambda(x, lambda(y, body))`, while non-fnform lhs values stay unchanged.
    pub fn lower_function_form_lambdas(&mut self, lhs: Value, body: Value) -> (Value, Value) {
        let mut lhs_ref: RawValue = lhs.into();
        if lhs_ref < ATOM_LIMIT || self[lhs_ref].tag != Tag::Ap {
            return (lhs, body);
        }

        let mut leftmost = ApNodeRef::from_ref(lhs_ref);
        let mut function_raw = leftmost.function_raw(self);
        while let Some(nested) = leftmost.function_application(self) {
            leftmost = nested;
            function_raw = leftmost.function_raw(self);
        }

        if function_raw < ATOM_LIMIT || self[function_raw].tag != Tag::Id {
            return (lhs, body);
        }

        let function = IdentifierRecordRef::from_ref(function_raw);
        if is_capitalized(function.get_name(self).as_str()) {
            return (lhs, body);
        }

        let mut lowered_body = body;
        while lhs_ref >= ATOM_LIMIT && self[lhs_ref].tag == Tag::Ap {
            let application = ApNodeRef::from_ref(lhs_ref);
            lowered_body = self.lambda_ref(application.argument_raw(self).into(), lowered_body);
            lhs_ref = application.function_raw(self);
        }

        (lhs_ref.into(), lowered_body)
    }

    fn let_(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Let, head, tail)
    }

    pub fn let_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.let_(head.into(), tail.into()))
    }

    fn letrec(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::LetRec, head, tail)
    }

    pub fn letrec_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.letrec(head.into(), tail.into()))
    }

    fn share(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Share, head, tail)
    }

    pub fn share_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.share(head.into(), tail.into()))
    }

    fn pair(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Pair, head, tail)
    }

    pub fn pair_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.pair(head.into(), tail.into()))
    }

    fn start_read_vals(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::StartReadValues, head, tail)
    }

    pub fn start_read_vals_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.start_read_vals(head.into(), tail.into()))
    }

    fn tcons(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::TCons, head, tail)
    }

    pub fn tcons_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.tcons(head.into(), tail.into()))
    }

    fn tries(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Tries, head, tail)
    }

    pub fn tries_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.tries(head.into(), tail.into()))
    }

    fn type_var(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::TypeVar, head, tail)
    }

    pub fn type_var_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.type_var(head.into(), tail.into()))
    }

    fn label(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Label, head, tail)
    }

    pub fn label_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.label(head.into(), tail.into()))
    }

    fn show(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Show, head, tail)
    }

    pub fn show_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.show(head.into(), tail.into()))
    }

    fn readvals(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::StartReadValues, head, tail)
    }

    pub fn readvals_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.readvals(head.into(), tail.into()))
    }

    fn apply(&mut self, head: RawValue, tail: RawValue) -> RawValue {
        self.put(Tag::Ap, head, tail)
    }

    /// Typed wrapper around [`Heap::apply`] that returns a typed heap reference.
    pub fn apply_ref(&mut self, head: Value, tail: Value) -> Value {
        Value::Reference(self.apply(head.into(), tail.into()))
    }

    pub fn apply2(&mut self, x: Value, y: Value, z: Value) -> Value {
        let f = self.apply_ref(x, y);
        self.apply_ref(f, z)
    }

    pub fn apply3(&mut self, w: Value, x: Value, y: Value, z: Value) -> Value {
        let f = self.apply2(w, x, y);
        self.apply_ref(f, z)
    }

    /// Boxes a real number (an `f64`).
    fn real(&mut self, number: f64) -> RawValue {
        let bits = f64::to_bits(number) as RawValue;
        self.put(Tag::Double, Value::Data(bits).into(), Value::None.into())
    }

    pub fn real_ref(&mut self, number: f64) -> Value {
        Value::Reference(self.real(number))
    }

    /// Creates a (boxed) Unicode character.
    fn unicode(&mut self, code_point: RawValue) -> RawValue {
        self.put(Tag::Unicode, code_point, Value::None.into())
    }

    pub fn unicode_ref(&mut self, code_point: isize) -> Value {
        Value::Reference(self.unicode(code_point))
    }

    // endregion

    // region Type creation and checking functions

    /// Returns true iff the value is 1) an identifier that 2) is a constructor,
    /// which occurs iff 1) its tag is `Tag::Identifier` and 2) its name is capitalized.
    pub fn is_constructor(&self, reference: Value) -> bool {
        let identifier_cell: HeapCell = match self.expect(Tag::Id, reference) {
            Ok(cell) => cell,
            Err(_) => return false,
        };

        let strcons = self[identifier_cell.head].head;
        let name_ref = self[strcons].head;

        is_capitalized(self.strings[name_ref as usize].as_str())
    }

    /// Is the type referenced by `reference` an Arrow type? Not only checks the type but also that the correct
    /// `HeapCell` `Tag` is present at each level of the composition.
    pub fn is_arrow_type(&self, reference: Value) -> bool {
        self.is_type_auxiliary(reference, Type::Arrow)
    }

    /// Is the type referenced by `reference` a Comma type?  Not only checks the type but also that the correct
    /// `HeapCell` `Tag` is present at each level of the composition.
    pub fn is_comma_type(&self, reference: Value) -> bool {
        self.is_type_auxiliary(reference, Type::Comma)
    }

    /// Code common to is_arrow_type and is_comma_type is factored out into this auxiliary function. Note that this
    /// function checks that correct tags are present.
    fn is_type_auxiliary(&self, reference: Value, type_required: Type) -> bool {
        if !matches!(reference, Value::Reference(_)) {
            return false;
        }

        let reference = RawValue::from(reference);

        if self[reference].tag != Tag::Ap {
            return false;
        }

        let type_application = ApNodeRef::from_ref(reference);
        let Some(operator_application) = type_application.function_application(self) else {
            return false;
        };

        operator_application.function_raw(self) == RawValue::from(type_required)
    }

    /// Is the type referenced by reference a list type? Not only checks the type but also that the correct
    /// `HeapCell` `Tag` is present at each level of the composition.
    pub fn is_list_type(&self, reference: Value) -> bool {
        if !matches!(reference, Value::Reference(_)) {
            return false;
        }

        let reference = RawValue::from(reference);

        if self[reference].tag != Tag::Ap {
            return false;
        }

        ApNodeRef::from_ref(reference).function_raw(self) == RawValue::from(Type::List)
    }

    pub fn is_type_variable(&self, reference: Value) -> bool {
        self.expect(Tag::TypeVar, reference).is_ok()
    }

    /// Is the type referenced by reference a compound type? Note that this function returns true for
    /// list and arrow types.
    pub fn is_compound_type(&self, reference: Value) -> bool {
        self.expect(Tag::Ap, reference).is_ok()
    }

    pub fn is_bound_type(&self, reference: Value) -> bool {
        if !matches!(reference, Value::Reference(_)) {
            return false;
        }

        let reference = RawValue::from(reference);

        if self[reference].tag != Tag::Ap {
            return false;
        }

        ApNodeRef::from_ref(reference).function_raw(self) == RawValue::from(Type::Bind)
    }

    fn arrow_type(&mut self, arg1: Value, arg2: Value) -> RawValue {
        self.apply2(Type::Arrow.into(), arg1, arg2).into()
    }

    pub fn arrow_type_ref(&mut self, arg1: Value, arg2: Value) -> Value {
        Value::Reference(self.arrow_type(arg1, arg2))
    }

    fn arrow2_type(&mut self, arg1: Value, arg2: Value, arg3: Value) -> RawValue {
        let composed = self.arrow_type_ref(arg2, arg3);
        self.arrow_type(arg1, composed)
    }

    pub fn arrow2_type_ref(&mut self, arg1: Value, arg2: Value, arg3: Value) -> Value {
        Value::Reference(self.arrow2_type(arg1, arg2, arg3))
    }

    fn arrow3_type(&mut self, arg1: Value, arg2: Value, arg3: Value, arg4: Value) -> RawValue {
        let composed = self.arrow2_type_ref(arg2, arg3, arg4);
        self.arrow_type(arg1, composed)
    }

    pub fn arrow3_type_ref(&mut self, arg1: Value, arg2: Value, arg3: Value, arg4: Value) -> Value {
        Value::Reference(self.arrow3_type(arg1, arg2, arg3, arg4))
    }

    fn arrow4_type(
        &mut self,
        arg1: Value,
        arg2: Value,
        arg3: Value,
        arg4: Value,
        arg5: Value,
    ) -> RawValue {
        let composed = self.arrow3_type_ref(arg2, arg3, arg4, arg5);
        self.arrow_type(arg1, composed)
    }

    pub fn arrow4_type_ref(
        &mut self,
        arg1: Value,
        arg2: Value,
        arg3: Value,
        arg4: Value,
        arg5: Value,
    ) -> Value {
        Value::Reference(self.arrow4_type(arg1, arg2, arg3, arg4, arg5))
    }

    fn list_type(&mut self, arg: Value) -> RawValue {
        self.apply_ref(Type::List.into(), arg).into()
    }

    pub fn list_type_ref(&mut self, arg: Value) -> Value {
        Value::Reference(self.list_type(arg))
    }

    fn pair_type(&mut self, arg1: Value, arg2: Value) -> RawValue {
        let inner = self.apply2(Type::Comma.into(), arg2, Type::Void.into());
        self.apply2(Type::Comma.into(), arg1, inner).into()
    }

    pub fn pair_type_ref(&mut self, arg1: Value, arg2: Value) -> Value {
        Value::Reference(self.pair_type(arg1, arg2))
    }

    // endregion
}

// Todo: Find a better home for this function.
/// Returns true iff the first non-'$' character of word is uppercase.
pub(crate) fn is_capitalized(word: &str) -> bool {
    for c in word.chars() {
        if c == '$' {
            continue;
        }
        return c.is_uppercase();
    }
    // Consists only of '$'
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::api::{IdentifierValueTypeData, IdentifierValueTypeRef};
    use crate::data::types::Type;

    #[test]
    fn round_trip_values() {
        let x: Value = Value::Data(42);
        let y: Value = Value::Data(43);
        let expected: HeapCell = HeapCell::new(Tag::DataPair, x, y);
        let mut heap: Heap = Heap::new();
        let reference: RawValue = heap.data_pair_ref(x, y).into();
        let result: HeapCell = heap.resolve(reference.into()).unwrap();

        assert_eq!(result, expected);
    }

    #[test]
    fn composition() {
        let mut heap: Heap = Heap::new();
        let noodles_ref = heap.string("noodles");
        let _wontons_ref = heap.string("wontons");
        let salad_ref = heap.string("salad.ml");

        let x = heap.cons_ref(
            Value::Data(2),
            Value::Data(1), // "wontons"
        );

        let value_type =
            IdentifierValueTypeRef::new(&mut heap, IdentifierValueTypeData::PlaceHolder);

        let y = heap.cons_ref(value_type.get_ref().into(), Combinator::Nil.into());
        let value = heap.cons_ref(x, y); //cons(cons(arity,showfn),cons(placeholder_t,NIL))

        let aka = heap.data_pair_ref(Value::Reference(noodles_ref), Value::Data(0)); // Aliasing noodles.
        let y = heap.file_info_ref(Value::Reference(salad_ref), Value::Data(328)); // salad.ml
        let who = heap.cons_ref(aka, y);
        // cons(aka,hereinfo)
        // fileinfo(script,line_no)

        let x = heap.strcons_ref(Value::Reference(noodles_ref), who);
        let id_head = heap.cons_ref(x, Value::Data(Type::Number as RawValue).into());
        let id: RawValue = heap.put_ref(Tag::Id, id_head, value).into(); // cons(strcons(name,who),type)

        let id_record = IdentifierRecordRef::from_ref(id);

        let ident = id_record.get_data(&heap);
        println!("Identifier:\n\t{}", ident.name);
        assert!(true);
    }

    #[test]
    fn type_queries_follow_application_proxy_shape() {
        let mut heap = Heap::new();

        let arrow = heap.apply2(Type::Arrow.into(), Type::Bool.into(), Type::Char.into());
        let list = heap.apply_ref(Type::List.into(), Type::Bool.into());
        let bound = heap.apply_ref(Type::Bind.into(), Type::Bool.into());

        assert!(heap.is_arrow_type(arrow));
        assert!(heap.is_list_type(list));
        assert!(heap.is_bound_type(bound));
    }

    #[test]
    fn function_form_lowering_wraps_arguments_in_nested_lambdas() {
        let mut heap = Heap::new();
        let function = heap.make_empty_identifier("f");
        let x = heap.make_empty_identifier("x");
        let y = heap.make_empty_identifier("y");
        let inner = ApNodeRef::new(&mut heap, function.into(), x.into());
        let lhs = ApNodeRef::new(&mut heap, inner.into(), y.into());
        let body = Combinator::Plus.into();

        let (lowered_lhs, lowered_body) = heap.lower_function_form_lambdas(lhs.into(), body);

        assert_eq!(lowered_lhs, function.into());
        let outer_lambda: RawValue = lowered_body.into();
        assert_eq!(heap[outer_lambda].tag, Tag::Lambda);
        assert_eq!(heap[outer_lambda].head, x.get_ref());

        let inner_lambda = heap[outer_lambda].tail;
        assert_eq!(heap[inner_lambda].tag, Tag::Lambda);
        assert_eq!(heap[inner_lambda].head, y.get_ref());
        assert_eq!(heap[inner_lambda].tail, RawValue::from(body));
    }

    #[test]
    fn function_form_lowering_skips_constructor_heads() {
        let mut heap = Heap::new();
        let constructor = heap.make_empty_identifier("Cons");
        let argument = heap.make_empty_identifier("x");
        let lhs = ApNodeRef::new(&mut heap, constructor.into(), argument.into());
        let body = Combinator::Plus.into();

        let (lowered_lhs, lowered_body) = heap.lower_function_form_lambdas(lhs.into(), body);

        assert_eq!(lowered_lhs, lhs.into());
        assert_eq!(lowered_body, body);
    }
}
