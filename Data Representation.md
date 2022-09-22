# Representation Of Data On The Heap

## Identifiers

 The `head` of an `Id` contains `cons(strcons(name,who),type)` and the `tail` has the value.
  The `who` field contains one of:

* `NIL` (for a name that is totally undefined)
* `hereinfo` for a name that has been defined or specified, where `hereinfo` is `fileinfo(script,line_no)`
* `cons(aka,hereinfo)` for a name that has been aliased, where `aka`
     is of the form `datapair(oldn,0)`, `oldn` being a string.


Miranda:

The value field of type identifier takes one of the following forms
 * `cons(cons(arity,showfn),cons(algebraic_t,constructors)`
 * `cons(cons(arity,showfn),cons(synonym_t,rhs))`
 * `cons(cons(arity,showfn),cons(abstract_t,basis))`
 * `cons(cons(arity,showfn),cons(placeholder_t,NIL))`
 * `cons(cons(arity,showfn),cons(free_t,NIL))`

|              | Arity    | Show Functions |            | Type             | Info             |
| :----------- | :------- | :------------- | :--------- | :--------------- | :--------------- |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `algebraic_t,`   | `constructors )` |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `synonym_t,`     | `rhs  ))`        |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `abstract_t,`    | `basis ))`       |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `placeholder_t,` | `NIL ))`         |
| `cons(cons(` | `arity,` | `showfn`       | `), cons(` | `free_t,`        | `NIL ))`         |

where

```C
#define algebraic_t 0
#define synonym_t 1
#define abstract_t 2
#define placeholder_t 3
#define free_t 4
```





## Strings

Strings are stored as
```rust
HeapCell{
  tag: Tag::String,
  head: high_bytes,
  tail: low_bytes
}
```
where `idx = high_bytes as usize << 32 + low_bytes as usize` is an index into a string table.

## Other pointers


There may be cases where a reference to memory outside of the heap is required. This can be achieved by
creating a new `Tag` variant and then structuring the `HeapCell` as follows:
```rust
HeapCell{
  tag: Tag::NewPointerType,
  head: high_bytes,
  tail: low_bytes
}
```
Then the pointer can be recovered as follows:
```rust
let pointer_value: usize = high_bytes as usize << 32 + low_bytes as usize;
unsafe{
  let *ptr: MyDataType* = std::mem::transmute(pointer_value);
}
```
Wrap this in an API to client code need not use `transmute`.

