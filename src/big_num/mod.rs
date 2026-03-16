/*!

Module root for the Miranda bigint fidelity port.

The bigint subsystem is now part of the active crate module graph, but broad runtime
adoption is still intentionally staged. For now this module remains mostly self-contained
so activation and follow-on seam migration can proceed incrementally.

The intended integration path remains:

1. faithful internal algorithm port,
2. `IntegerRef` heap proxy boundary,
3. bytecode codec adoption,
4. lifted runtime API adoption.

*/

mod algorithms;
mod bytecode;
mod constants;
mod integer_ref;
mod internal;
mod text;

pub(crate) use integer_ref::IntegerRef;
