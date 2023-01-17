/*!

Methods for parsing serialized bytecode binaries, which Miranda calls "dump" files. This code is factored out of
`load_script`, which was a dumpster fire of spaghetti.

*/

use std::iter::Peekable;
use std::slice::Iter;
use std::mem::size_of;
use std::time::{Duration, SystemTime};

use crate::errors::BytecodeError;

// Convenience alias, saves some typing
type PResult<T> =  Result<T, BytecodeError>;


/// Given an iterator of bytes, extracts a (platform-dependent, little-endian) word from the iterator.
pub(crate) fn get_machine_word(byte_iter: &mut dyn Iterator<Item=u8>) -> PResult<usize> {
  let mut shift_by = 0;
  let word_size = size_of::<usize>();
  let mut accumulator: usize = 0;

  for _ in 0.. word_size {
    // Miranda also does not check for EOF here.
    if let Ok(next_byte) = byte_iter.next() {
      accumulator |= byte_iter.next().unwrap_or(0) << shift_by;
      shift_by += word_size;
    } else {
      return Err(BytecodeError::UnexpectedEOF);
    }
  }

  Ok(accumulator)
}


/// The line number of an error is encoded as a little-endian machine word. Consumes a machine word, returning it as a
/// usize.
pub(crate) fn parse_line_number(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<usize> {
  get_machine_word(byte_iter)
}


/// Parses the filename and the last modified time, returning a `String` and a `SystemTime` value.
pub(crate) fn parse_filename_modified_time(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<(String, SystemTime)> {
  // Parse the file name
  let mut filename: String;
  let mut char_vec: Vec<u8> = vec![*ch];

  char_vec.extend(
    byte_iter.by_ref()
             .take_while(|c| *c != 0)
             .cloned()
  );
  filename = String::from_utf8_lossy(&*char_vec).into();

  // Todo: Do we throw an error if `filename` is empty?

  // Parse the file's last modified time
  // Parse file modified time
  // Todo: Is this the right way to deserialize an mtime? How is it serialized?
  let modified_time = SystemTime::UNIX_EPOCH + Duration::from_secs(get_machine_word(byte_iter)? as u64);

  Ok((filename, modified_time))
}


/// Interprets the next byte as a boolean value, consuming the byte.
pub(crate) fn parse_sharable_flag(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<bool> {
  if let Some(ch) = byte_iter.next() {
    Ok(ch == 1)
  } else {
    Err(BytecodeError::UnexpectedEOF)
  }
}


