# Notes


## The high level "API"

Low-level bytecode manipulation happens on the level of heap cells, which consist of a tag, a head, and a tail. Data
structures are just compositions of combinators encoded as values and references contained in heads and tails. Types
are almost completely opaque, distinguished by context alone. Accessing members of a structure is also opaque w.r.t.
readability: `id_type = hd[tl[the_val(x)]]`.

Several data structures are implemented "by proxy" in an object that internally holds a reference to the object on
the heap. The proxy object implements constructors and accessors for the members of the object. With such a proxy
object, an object on the heap can be treated just like any other object.

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
chasing, which must be significant when every single thing is boxed. Also, RAII makes this of questionable benefit.

Strangely, private names are not stored in the heap. They are stored in the vector `pnvec`. Other good candidates 
for storing in vectors:
  * `suppressed` and `suppressed_t` (`SUPPRESSED`, `TSUPPRESSED`), which seem to only be used for error reporting 
    during script loading.
  * `clashes`, same.
  * `prefix_stack`, for resolving relative file paths during loading/unloading scripts.

### `Type`, `Tag`, etc., versus `Combinator`. 

There is a lot of overlap between other "types" (implemented as `#define`s) and `Combinator`. Why isn't `Combinator` 
used when it could be? Why have multiple versions of "undefined"/"Empty"/"None"/"NIL"?

## Implementation Challenges

Miranda implements a GC mark-and-sweep strategy that uses a combination of root lists
and conservative stack memory scanning. Conservative scanning of stack memory is not a
Rust-friendly implementation strategy. We will need to come up with an alternative scheme.
