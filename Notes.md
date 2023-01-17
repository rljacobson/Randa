# Notes


## The high level "API"

Low-level bytecode manipulation happens on the level of heap cells, which consist of a tag, a head, and a tail. Data
structures are just compositions of combinators encoded as values and references contained in heads and tails. Types
are almost completely opaque, distinguished by context alone. Accessing members of a structure is also opaque w.r.t.
readability: `id_type = hd[tl[the_val(x)]]`.

Several data structures are implemented "by proxy" in an object that internally holds a reference to the object on
the heap. The proxy object implements constructors and accessors for the members of the object. With such a proxy
object, an object on the heap can be treated just like any other object:

```rust

```

## Miranda design oddities

### `Heap`-resident data

A lot of data is stored in the Randa heap:
  * files
  * prefix_stack
  * free_ids
  * ids_used
  * includees
  * …

Unless this data is exposed to Miranda code, there is no reason to store them on the Randa heap. It would be simpler 
and probably more efficient to put them in a vector. It isn't clear at all that they are exposed to Randa code. In 
fact, the IR and bytecode of programs are also stored on the heap. Is Miranda homoiconic? 

One possible reason to store them on the Randa heap is to know how much heap memory a given program uses. But what 
constitutes "the program"? Another reason is to get a garbage collector "for free"–actually, at the cost of pointer 
chasing, which must be significant when every single thing is boxed. Also, RIAA makes this of questionable benefit.

Strangely, private names are not stored in the heap. They are stored in the vector `pnvec`. Other good candidates 
for storing in vectors:
  * `suppressed` and `suppressed_t` (`SUPPRESSED`, `TSUPPRESSED`), which seem to only be used for error reporting 
    during script loading.
  * `clashes`, same.
  * `prefix_stack`, for resolving relative file paths during loading/unloading scripts.
  * 

### `Type`, `Tag`, etc., versus `Combinator`. 

There is a lot of overlap between other "types" (implemented as `#define`s) and `Combinator`. Why isn't `Combinator` 
used when it could be? Why have multiple versions of "undefined"/"Empty"/"None"/"NIL"?

## My own design errors

### Abundance of Value-like Rust types

I have the types `Type`, `Combinator`, etc., that implement `Into<Value>`. Then I have the type `Value`, an enum 
that has a variant for any data type that can live in the heap. Then there is `RawValue`, a newtype for the numeric 
type `ValueRepresentationType`, which itself is an alias for `isize`. `RawValue`/`ValueRepresentationType` are meant 
to be the lowest-level binary representation of data in the heap, and all `Value`s can be serialized as 
`RawValue`/`ValueRepresentationType`. In practice, everything is convertible to a `Value`, and everything is 
convertible to a `RawValue` and therefore a `ValueRepresentationType`. 
 1. `RawValue` isn't enforcing any type safety if you can always just do `.into()` to convert 
    `RawValue` <--> `ValueRepresentationType`. So `RawValue` isn't doing anything.
 2. It's not clear which functions should take a `Value` versus a `RawValue`/`ValueRepresentationType`. Consequently,
    `.into::<Value>()` & `.into::<RawValue>()` are used routinely even in cases where they don't make sense. For 
    example, it's common to call `heap.cons(thing1.into::<Value>(), thing2.into::<Value>())` even for cases where it 
    is known that one or both `thing`_j's deserializes to them wrong `Value` variant. Fortunately, it doesn't matter,
    because `RawValue` -->`Value`-->`RawValue` is always the identity, even if the intermediate `Value` is 
    nonsensical in context.
 3. In the case of the higher-level API functions like `Heap::cons` and friends, the `Value` arguments are just 
    turned right back into `RawValue`/`ValueRepresentationType` anyway. 
