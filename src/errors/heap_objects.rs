/*!

Errors related to the manipulation of objects on the heap. These errors are used in `data::api`.

 */
use thiserror::Error;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Error)]
pub enum HeapObjectError {
    #[error("heap object is malformed")]
    Malformed,
}
