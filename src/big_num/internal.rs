//! Shared internal helpers for bigint construction and digit-vector work.

use super::constants::{BIGINT_DIGIT_BASE, BIGINT_DIGIT_BITS, BIGINT_DIGIT_MASK};
use super::integer_ref::IntegerRef;
use crate::data::{api::HeapObjectProxy, Heap};

impl IntegerRef {
    /// Collects magnitude digits from a trusted integer in little-endian order.
    ///
    /// This helper keeps digit-vector algorithms independent of direct heap-link
    /// manipulation while preserving Miranda's admitted integer shape.
    /// Mutation/allocation: non-mutating with respect to heap integers; allocates a fresh host-side digit vector.
    pub(super) fn collect_magnitude_digits(&self, heap: &Heap) -> Vec<isize> {
        let mut digits = vec![self.low_digit(heap)];
        let mut current = self.next_digit_cell(heap);

        while let Some(cell) = current {
            digits.push(cell.digit_word(heap));
            current = cell.next_digit_cell(heap);
        }

        digits
    }

    /// Builds a trusted integer chain from little-endian magnitude digits and a root sign bit.
    ///
    /// This helper centralizes first-cell sign encoding for bigint internals
    /// so algorithms and codecs produce the same normalized heap shape.
    /// Mutation/allocation: allocates a fresh heap integer chain; does not mutate any pre-existing integer cells.
    pub(super) fn build_integer_from_digits(
        heap: &mut Heap,
        digits: &[isize],
        root_signbit: isize,
    ) -> IntegerRef {
        debug_assert!(!digits.is_empty());

        let result = IntegerRef::from_stored_cell(heap, digits[0] | root_signbit, None);
        let mut tail_slot = result.get_ref();

        for &digit in &digits[1..] {
            let next = IntegerRef::from_stored_cell(heap, digit, None);
            heap[tail_slot].tail = next.get_ref();
            tail_slot = next.get_ref();
        }

        result
    }
}

/// Trims redundant high zero digits while preserving canonical zero.
///
/// This helper keeps digit-vector computations aligned with the bigint
/// normalization contract before they are reified back into heap objects.
/// Mutation/allocation: mutates the provided host-side digit vector in place; touches no heap data.
pub(super) fn normalize_digit_vec(digits: &mut Vec<isize>) {
    while digits.len() > 1 && digits.last() == Some(&0) {
        digits.pop();
    }
}

/// Multiplies a little-endian magnitude digit slice by one base digit.
///
/// This helper supports long division and chunked parsing without constructing
/// intermediate heap objects for every scaled magnitude.
/// Mutation/allocation: non-mutating with respect to the input slice; allocates a fresh host-side result vector.
pub(super) fn multiply_digit_slice_by_digit(
    digits: &[isize],
    digit_multiplier: isize,
) -> Vec<isize> {
    let mut result = Vec::with_capacity(digits.len() + 1);
    let mut carry = 0u128;

    for &digit in digits {
        let product = (digit as u128) * (digit_multiplier as u128) + carry;
        result.push((product & BIGINT_DIGIT_MASK as u128) as isize);
        carry = product >> BIGINT_DIGIT_BITS;
    }

    if carry != 0 {
        result.push(carry as isize);
    }

    normalize_digit_vec(&mut result);
    result
}

/// Prepends zero digits to multiply a magnitude by a power of the bigint base.
///
/// This helper supports long division alignment and mirrors the same base-power
/// shift concept used by Miranda's `shift` helper.
/// Mutation/allocation: non-mutating with respect to the input slice; allocates a fresh shifted host-side vector.
pub(super) fn shift_digits_left(digits: &[isize], shift: usize) -> Vec<isize> {
    let mut shifted = vec![0; shift];
    shifted.extend_from_slice(digits);
    shifted
}

/// Divides a little-endian magnitude digit vector by a small divisor and returns the remainder.
///
/// This helper is shared by formatting code and single-digit arithmetic
/// helpers so quotient normalization remains consistent across modules.
/// Mutation/allocation: mutates the provided host-side digit vector in place to hold the quotient; allocates only temporary host-side scratch data.
pub(super) fn divide_digit_vec_by_small(digits: &mut Vec<isize>, divisor: isize) -> isize {
    let mut quotient_big_endian = Vec::with_capacity(digits.len());
    let mut remainder = 0isize;

    for &digit in digits.iter().rev() {
        let dividend = remainder * BIGINT_DIGIT_BASE + digit;
        quotient_big_endian.push(dividend / divisor);
        remainder = dividend % divisor;
    }

    let first_non_zero = quotient_big_endian
        .iter()
        .position(|&digit| digit != 0)
        .unwrap_or(quotient_big_endian.len().saturating_sub(1));
    let mut quotient_digits = quotient_big_endian[first_non_zero..].to_vec();
    quotient_digits.reverse();
    normalize_digit_vec(&mut quotient_digits);
    *digits = quotient_digits;

    remainder
}

/// Multiplies a little-endian magnitude digit vector by a small host integer in place.
///
/// This helper supports Miranda-style chunked text parsing without allocating
/// intermediate heap integers for each multiply step.
/// Mutation/allocation: mutates the provided host-side digit vector in place; touches no heap data.
pub(super) fn multiply_digit_vec_by_small_in_place(digits: &mut Vec<isize>, multiplier: isize) {
    let mut carry = 0isize;

    for digit in digits.iter_mut() {
        let product = multiplier * *digit + carry;
        *digit = product % BIGINT_DIGIT_BASE;
        carry = product / BIGINT_DIGIT_BASE;
    }

    while carry != 0 {
        digits.push(carry % BIGINT_DIGIT_BASE);
        carry /= BIGINT_DIGIT_BASE;
    }
}

/// Adds a small host integer into a little-endian magnitude digit vector in place.
///
/// This helper completes Miranda's chunked `r = f * r + d` text-parsing step
/// while preserving normalized digit-vector output.
/// Mutation/allocation: mutates the provided host-side digit vector in place; touches no heap data.
pub(super) fn add_small_to_digit_vec_in_place(digits: &mut Vec<isize>, addend: isize) {
    let mut carry = addend;
    let mut index = 0usize;

    while carry != 0 {
        if index == digits.len() {
            digits.push(0);
        }

        let sum = digits[index] + carry;
        digits[index] = sum % BIGINT_DIGIT_BASE;
        carry = sum / BIGINT_DIGIT_BASE;
        index += 1;
    }
}
