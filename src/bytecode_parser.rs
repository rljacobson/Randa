/*!

Methods for parsing serialized bytecode binaries, which Miranda calls "dump" files. This code is factored out of
`load_script`, which was a dumpster fire of spaghetti.

*/

use std::{
  mem::size_of,
  slice::Iter,
  iter::Peekable,
  time::{Duration, SystemTime}
};
use num_traits::FromPrimitive;

use crate::{
  data::{
    api::{ConsList, IdentifierRecord},
    Heap
  },
  compiler::bytecode::Bytecode,
  errors::BytecodeError
};
use crate::data::{RawValue, Tag, Value, ValueRepresentationType};
use crate::data::api::HeapObjectProxy;


// Convenience alias, saves some typing
type PResult<T> =  Result<T, BytecodeError>;


/// Convenience function that returns the next byte or `BytecodeError::UnexpectedEOF`.
fn next(byte_iter: &mut dyn Iterator<Item=u8>) -> PResult<u8> {
  match byte_iter.next() {
    Some(ch) => Ok(ch),

    None => {
      Err(BytecodeError::UnexpectedEOF)
    }
  }
}


/// Given an iterator of bytes, extracts a (platform-dependent, little-endian) word as a `usize` from the iterator.
pub(crate) fn get_machine_word(byte_iter: &mut dyn Iterator<Item=u8>) -> PResult<usize> {
  let mut shift_by = 0;
  let word_size = size_of::<usize>();
  let mut accumulator: usize = 0;

  for _ in 0.. word_size {
    // Miranda does not check for EOF here.
    if let Ok(next_byte) = byte_iter.next() {
      accumulator |= next_byte << shift_by;
      shift_by += word_size;
    } else {
      return Err(BytecodeError::UnexpectedEOF);
    }
  }

  Ok(accumulator)
}

/// Extracts a machine word from the byte iterator and reinterprets it as a value of type `N`. Note: `N` must be the
/// same size as `usize`.
pub(crate) fn get_number<N>(byte_iter: &mut dyn Iterator<Item=u8>) -> PResult<N> {
  let machine_word = get_machine_word(byte_iter)?;
  let value: N =
      unsafe{
        // usize --> N
        std::mem::transmute::<usize, N>(machine_word)
      };

  Ok(value)
}

/// Extracts two bytes from `byte_iter` and interprets them as a `N` value.
pub(crate) fn get_number_16bit<N>(byte_iter: &mut dyn Iterator<Item=u8>) -> N {
  let mut v: N = next(byte_iter)? as N;
  v += (next(byte_iter)? as N) << 8;
  v
}

/// The line number of an error is encoded as a little-endian machine word. Consumes a machine word, returning it as a
/// usize.
pub(crate) fn parse_line_number(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<usize> {
  get_number::<usize>(byte_iter)
}


/// Parses the filename and the last modified time, returning a `String` and a `SystemTime` value.
pub(crate) fn parse_filename_modified_time(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<(String, SystemTime)> {
  // Parse the file name
  let mut filename: String = parse_string(byte_iter)?;

  // Todo: Do we throw an error if `filename` is empty?

  // Parse the file's last modified time
  // Parse file modified time
  // Todo: Is this the right way to deserialize an mtime? How is it serialized?
  let modified_time = SystemTime::UNIX_EPOCH + Duration::from_secs(get_number::<u64>(byte_iter)?);

  Ok((filename, modified_time))
}

pub(crate) fn parse_string(byte_iter: &mut dyn Iterator<Item=u8>) -> PResult<String> {
  let mut char_vec: Vec<u8> = vec![];

  char_vec.extend(
    byte_iter.by_ref()
             .take_while(|c| *c != 0)
             .cloned()
  );

  Ok(String::from_utf8_lossy(&*char_vec).into())
}

/// Interprets the next byte as a boolean value, consuming the byte.
pub(crate) fn parse_sharable_flag(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<bool> {
  Ok(next(byte_iter)? == 1)
}

