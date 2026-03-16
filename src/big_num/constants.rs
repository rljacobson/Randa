//! Miranda integer representation constants shared by the bigint port.

use crate::data::RawValue;

/// Sign-bit mask stored in the first integer cell head.
///
/// This preserves Miranda's `SIGNBIT` convention so trusted integer access can
/// distinguish negative values without changing the heap representation.
pub(crate) const SIGN_BIT_MASK: RawValue = 0o20000000000;

/// Base of each little-endian bigint digit.
///
/// This is Miranda's `IBASE`, the base-`2^15` digit radix used by heap
/// integers, bytecode integer payloads, and the arithmetic helpers in `big.c`.
pub(crate) const BIGINT_DIGIT_BASE: RawValue = 0o100000;

/// Mask for the payload bits of one bigint digit.
///
/// This is Miranda's `MAXDIGIT`, used to strip the sign bit from the first
/// cell and to normalize digit arithmetic back into the admitted digit range.
pub(crate) const BIGINT_DIGIT_MASK: RawValue = 0o77777;

/// Bit width of one bigint digit.
///
/// This is Miranda's `DIGITWIDTH`, used by scalar conversion and digit-level
/// arithmetic to shift between host integers and base-`2^15` heap digits.
pub(crate) const BIGINT_DIGIT_BITS: usize = 15;

/// Largest power of ten below `BIGINT_DIGIT_BASE`.
///
/// This is Miranda's `PTEN`, used by decimal parsing and formatting to batch
/// decimal work into chunks that fit inside one bigint digit.
pub(crate) const DECIMAL_CHUNK_BASE: RawValue = 10_000;

/// Largest power of sixteen that fits within one bigint digit.
///
/// This is Miranda's `PSIXTEEN`, used by hexadecimal parsing and formatting so
/// radix conversion stays aligned with the original `big.c` chunk sizes.
pub(crate) const HEX_CHUNK_BASE: RawValue = 4096;

/// Largest power of eight that fits within one bigint digit.
///
/// This is Miranda's `PEIGHT`, used by octal parsing and formatting to mirror
/// the original Miranda conversion helpers.
pub(crate) const OCTAL_CHUNK_BASE: RawValue = 0o100000;

/// Number of decimal digits in one `DECIMAL_CHUNK_BASE` chunk.
///
/// This is Miranda's `TENW`, used by decimal formatting to emit fixed-width
/// interior chunks after repeated division.
pub(crate) const DECIMAL_CHUNK_WIDTH: usize = 4;

/// Number of octal digits represented by one bigint digit.
///
/// This is Miranda's `OCTW`, used by octal formatting to mirror the original
/// chunk width assumptions in `bigtostr8`.
pub(crate) const OCTAL_CHUNK_WIDTH: usize = 5;

#[cfg(test)]
mod tests {
    use super::{
        BIGINT_DIGIT_BASE, BIGINT_DIGIT_BITS, BIGINT_DIGIT_MASK, DECIMAL_CHUNK_BASE,
        DECIMAL_CHUNK_WIDTH, HEX_CHUNK_BASE, OCTAL_CHUNK_BASE, OCTAL_CHUNK_WIDTH,
    };

    #[test]
    fn radix_and_chunk_constants_match_miranda_contract() {
        assert_eq!(BIGINT_DIGIT_BASE, 1 << BIGINT_DIGIT_BITS);
        assert_eq!(BIGINT_DIGIT_MASK, BIGINT_DIGIT_BASE - 1);
        assert_eq!(DECIMAL_CHUNK_BASE, 10_000);
        assert_eq!(HEX_CHUNK_BASE, 4096);
        assert_eq!(OCTAL_CHUNK_BASE, BIGINT_DIGIT_BASE);
        assert_eq!(DECIMAL_CHUNK_WIDTH, 4);
        assert_eq!(OCTAL_CHUNK_WIDTH, 5);
    }
}
