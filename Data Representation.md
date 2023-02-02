# Representation Of Data On The Heap

The `data` module collects things having to do with representing (encoding/decoding) _values_. Everything is stored
on a heap. The heap holds `HeapCell`s, which have the form
```text
  ┌────────────┬──────────────────┬──────────────────┐
  │ Tag[0..31] │ HeadValue[0..63] │ TailValue[0..63] │
  └────────────┴──────────────────┴──────────────────┘
```

```Rust
pub struct HeapCell {
  pub tag : Tag,       // TagRepresentationType   = i32
  pub head: RawValue,  // ValueRepresentationType = isize
  pub tail: RawValue,  // ValueRepresentationType = isize
}
```

A `Tag` has representation `TagRepresentationType`, which will probably be an `i32`. The head and tail
values will have type `Value` (an enum) in client code, which is represented internally and on the
heap by a `ValueRepresentationType`, an `isize`. With very little effort, `ValueRepresentationType`
could probably be a `u32`, but this is not what Miranda does on 64-bit systems, so neither do we.

## Identifiers

 The `head` of an `Id` contains `cons(strcons(name,who),type)` and the `tail` has the value.
 The `name` is a reference to a heap string (see Strings below). The `who` field contains one of:

* `NIL` (the combinator literal) for a name that is totally undefined
* `hereinfo` for a name that has been defined or specified, where `hereinfo` is `fileinfo(script,line_no)`
* `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
     is of the form `datapair(oldn,0)`, `oldn` being a string.

The value field (tail) of type identifier takes one of the following forms:
 * `cons(cons(arity, showfn), cons(algebraic_t,  constructors)`
 * `cons(cons(arity, showfn), cons(synonym_t,    rhs))`
 * `cons(cons(arity, showfn), cons(abstract_t,   basis))`
 * `cons(cons(arity, showfn), cons(placeholder_t,NIL))`
 * `cons(cons(arity, showfn), cons(free_t,       NIL))`
 * `UNDEF` (combinator literal)
 * _Some other value_

An example of "some other value" is in the case of identifiers that are also constructors. Their value field 
contains `construct(value, id_ref)`, where `id_ref` is a (circular) reference back to the identifier itself.

|              | Arity    | Show Functions |            | Type             | Info             |
|:-------------| :------- | :------------- | :--------- | :--------------- | :--------------- |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `algebraic_t,`   | `constructors )` |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `synonym_t,`     | `rhs  ))`        |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `abstract_t,`    | `basis ))`       |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `placeholder_t,` | `NIL ))`         |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `free_t,`        | `NIL ))`         |
 | `UNDEF`      |
 | _Value_      |

where

```C
#define algebraic_t 0
#define synonym_t 1
#define abstract_t 2
#define placeholder_t 3
#define free_t 4
```

(Note: The Miranda source fails to mention the `UNDEF` and _"other literal value"_ cases.)

<div style="display:none;">
@startwbs
  + Id
  ++ cons
  +++ strcons
  ++++ name
  ++++ who (nil/hereinfo)
  +++++ fileinfo
  ++++++ script
  ++++++ line
  +++ type
  ++ cons (UNDEF/Literal)
  +++ cons
  ++++ arity
  ++++ showfn
  +++ cons
  ++++ type
  ++++ typeinfo or nil
  @endwbs
</div>

  ![diagram](Data%20Representation.png)

## Types

### representation of types 

See src/data/types.rs.

```c
#define undef_t 0
#define bool_t 1
#define num_t 2
#define char_t 3
#define list_t 4
#define comma_t 5
#define arrow_t 6
#define void_t 7
#define wrong_t 8
#define bind_t 9
#define type_t 10
#define strict_t 11
#define alias_t 12
#define new_t 13
#define isarrow_t(t) (tag[t]==AP&&tag[hd[t]]==AP&&hd[hd[t]]==arrow_t)
#define iscomma_t(t) (tag[t]==AP&&tag[hd[t]]==AP&&hd[hd[t]]==comma_t)
#define islist_t(t) (tag[t]==AP&&hd[t]==list_t)
#define isvar_t(t) (tag[t]==TVAR)
#define iscompound_t(t) (tag[t]==AP)
```

NOTES:

User defined types are represented by Miranda identifiers (of type "`type`"),
generic types (e.g. "`**`") by Miranda numbers, and compound types are
built up with `AP` nodes, e.g. "`a->b`" is represented by '`ap2(arrow_t,a,b)`'
Applying `bind_t` to a type variable, thus: `ap(bind_t,tv)`, indicates that
it is not to be instantiated. Applying `strict_t` to a type represents the
'`!`' operator of algebraic type definitions.

## Strings

Strings are stored as
```rust
HeapCell {
  tag : Tag::String,
  head: idx,
  tail: 0
}
```
~~where `idx = high_bytes as usize << 32 + low_bytes as usize` is an index into a string table.~~

**EDIT:** Actually we just use `idx=head` as the index into the string table by itself, because we changed 
`ValueRepresentationType` to be `isize`. (Also, a `u32` is probably more than enough, anyway.) On the heap a string 
looks like:

|  `Tag::String`  |  `Value::Data(idx as ValueRepresentationType)`  |  `Value::None`  |
|:---------------:|:-----------------------------------------------:|:---------------:|



## Other pointers

There may be cases where a reference to memory outside of the heap is required. This can be achieved by
creating a new `Tag` variant and then structuring the `HeapCell` as follows:
```rust
HeapCell{
  tag : Tag::NewPointerType,
  head: high_bytes,
  tail: low_bytes
}
```
~~Then the pointer can be recovered as follows:~~
```rust
let pointer_value: usize = high_bytes as usize << 32 + low_bytes as usize;
unsafe{
  let *ptr: MyDataType* = std::mem::transmute(pointer_value);
}
```
~~Wrap this in an API to client code need not use `transmute`.~~

**EDIT:** Since `ValueRepresentationType` is an `isize`, pointers fit in just the head. (See notes on Strings.)

