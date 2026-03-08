/*!

Methods for parsing serialized bytecode binaries, which Miranda calls "dump" files. This code is factored out of
`load_script`, which was a dumpster fire of spaghetti.

*/

use crate::data::RawValue;
use crate::errors::BytecodeError;
use std::{
    iter::Peekable,
    slice::Iter,
    time::{Duration, SystemTime},
};

// Convenience alias, saves some typing
type PResult<T> = Result<T, BytecodeError>;

/// Convenience function that returns the next byte or `BytecodeError::UnexpectedEOF`.
fn next(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<u8> {
    match byte_iter.next() {
        Some(ch) => Ok(ch),

        None => Err(BytecodeError::unexpected_eof()),
    }
}

/// Given an iterator of bytes, extracts a (platform-dependent, little-endian) word as a `usize` from the iterator.
pub(crate) fn get_machine_word(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<usize> {
    let mut bytes = [0u8; std::mem::size_of::<usize>()];
    for byte in &mut bytes {
        *byte = next(byte_iter)?;
    }
    Ok(usize::from_le_bytes(bytes))
}

pub(crate) fn get_word_raw_value(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<RawValue> {
    let mut bytes = [0u8; std::mem::size_of::<RawValue>()];
    for byte in &mut bytes {
        *byte = next(byte_iter)?;
    }
    Ok(RawValue::from_le_bytes(bytes))
}

pub(crate) fn get_word_f64(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<f64> {
    let mut bytes = [0u8; std::mem::size_of::<f64>()];
    for byte in &mut bytes {
        *byte = next(byte_iter)?;
    }
    Ok(f64::from_le_bytes(bytes))
}

pub(crate) fn get_word_u64(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<u64> {
    let mut bytes = [0u8; std::mem::size_of::<u64>()];
    for byte in &mut bytes {
        *byte = next(byte_iter)?;
    }
    Ok(u64::from_le_bytes(bytes))
}

pub(crate) fn get_u16_le(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<u16> {
    let lo = next(byte_iter)?;
    let hi = next(byte_iter)?;
    Ok(u16::from_le_bytes([lo, hi]))
}

pub(crate) fn get_i16_le(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<i16> {
    let lo = next(byte_iter)?;
    let hi = next(byte_iter)?;
    Ok(i16::from_le_bytes([lo, hi]))
}

/// The line number of an error is encoded as a little-endian machine word. Consumes a machine word, returning it as a
/// usize.
pub(crate) fn parse_line_number(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<usize> {
    get_machine_word(&mut byte_iter.by_ref().copied())
}

/// Parses the filename and the last modified time, returning a `String` and a `SystemTime` value.
pub(crate) fn parse_filename_modified_time(
    byte_iter: &mut Peekable<Iter<u8>>,
) -> PResult<(String, SystemTime)> {
    // Parse the file name
    let filename: String = parse_string(&mut byte_iter.by_ref().copied())?;

    // Todo: Do we throw an error if `filename` is empty?

    // Parse the file's last modified time
    // Parse file modified time
    // Todo: Is this the right way to deserialize an mtime? How is it serialized?
    let modified_time = SystemTime::UNIX_EPOCH
        + Duration::from_secs(get_word_u64(&mut byte_iter.by_ref().copied())?);

    Ok((filename, modified_time))
}

pub(crate) fn parse_string(byte_iter: &mut dyn Iterator<Item = u8>) -> PResult<String> {
    let mut char_vec: Vec<u8> = vec![];

    char_vec.extend(byte_iter.take_while(|c| *c != 0));

    Ok(String::from_utf8_lossy(&char_vec).into())
}

/// Interprets the next byte as a boolean value, consuming the byte.
pub(crate) fn parse_sharable_flag(byte_iter: &mut Peekable<Iter<u8>>) -> PResult<bool> {
    Ok(next(&mut byte_iter.by_ref().copied())? == 1)
}
