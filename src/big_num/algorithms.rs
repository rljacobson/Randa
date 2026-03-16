//! Dormant home for faithful Rust ports of `miralib/big.c` helper families.

use super::constants::{BIGINT_DIGIT_BASE, BIGINT_DIGIT_BITS, BIGINT_DIGIT_MASK, SIGN_BIT_MASK};
use super::integer_ref::IntegerRef;
use super::internal::{
    divide_digit_vec_by_small, multiply_digit_slice_by_digit, normalize_digit_vec,
    shift_digits_left,
};
use crate::data::{api::HeapObjectProxy, Heap};

impl IntegerRef {
    /// Adds two non-negative magnitudes while applying an explicit sign bit to the result root.
    ///
    /// This is Miranda's `big_plus`, which exists so signed `bigplus` and
    /// `bigsub` can delegate to a shared magnitude helper without losing the original
    /// heap-shape semantics.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates a fresh result chain on the heap.
    pub(super) fn add_magnitudes_with_result_sign(
        &self,
        heap: &mut Heap,
        other: IntegerRef,
        signbit: isize,
    ) -> IntegerRef {
        let x_digits = self.collect_magnitude_digits(heap);
        let y_digits = other.collect_magnitude_digits(heap);
        let mut result_digits = Vec::with_capacity(x_digits.len().max(y_digits.len()) + 1);
        let mut carry = 0isize;
        let shared_len = x_digits.len().min(y_digits.len());

        for index in 0..shared_len {
            let sum = x_digits[index] + y_digits[index] + carry;
            carry = if (sum & BIGINT_DIGIT_BASE) != 0 { 1 } else { 0 };
            result_digits.push(sum & BIGINT_DIGIT_MASK);
        }

        let remainder = if x_digits.len() > y_digits.len() {
            &x_digits[shared_len..]
        } else {
            &y_digits[shared_len..]
        };

        for &digit in remainder {
            let sum = digit + carry;
            carry = if (sum & BIGINT_DIGIT_BASE) != 0 { 1 } else { 0 };
            result_digits.push(sum & BIGINT_DIGIT_MASK);
        }

        if carry != 0 {
            result_digits.push(1);
        }

        Self::build_integer_from_digits(heap, &result_digits, signbit)
    }

    /// Subtracts one non-negative magnitude from another and normalizes the result.
    ///
    /// This is Miranda's `big_sub`, including the complement-and-trim behavior
    /// that removes redundant high-order zero cells before trusted results escape.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates a fresh result chain on the heap.
    pub(super) fn subtract_magnitudes(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        match self.compare_magnitude_ordering(heap, other) {
            0 => IntegerRef::from_i64(heap, 0),
            1 => {
                let digits = subtract_larger_magnitude_digits(
                    &self.collect_magnitude_digits(heap),
                    &other.collect_magnitude_digits(heap),
                );
                Self::build_integer_from_digits(heap, &digits, 0)
            }
            -1 => {
                let digits = subtract_larger_magnitude_digits(
                    &other.collect_magnitude_digits(heap),
                    &self.collect_magnitude_digits(heap),
                );
                Self::build_integer_from_digits(heap, &digits, SIGN_BIT_MASK)
            }
            _ => unreachable!(),
        }
    }

    /// Multiplies a non-negative magnitude by one base-`2^15` digit.
    ///
    /// This is Miranda's `stimes`, which exists to support schoolbook
    /// multiplication and long division without exposing digit-level heap surgery.
    /// Mutation/allocation: non-mutating with respect to the input integer; allocates fresh result cells on the heap.
    pub(super) fn multiply_magnitude_by_digit(
        &self,
        heap: &mut Heap,
        digit_multiplier: isize,
    ) -> IntegerRef {
        let first_product = (digit_multiplier as u128) * (self.low_digit(heap) as u128);
        let mut carry = first_product >> BIGINT_DIGIT_BITS;
        let result = IntegerRef::from_stored_cell(
            heap,
            (first_product & BIGINT_DIGIT_MASK as u128) as isize,
            None,
        );
        let mut tail_slot = result.get_ref();
        let mut current = self.next_digit_cell(heap);

        while let Some(cell) = current {
            let product = (digit_multiplier as u128) * (cell.digit_word(heap) as u128) + carry;
            let next = IntegerRef::from_stored_cell(
                heap,
                (product & BIGINT_DIGIT_MASK as u128) as isize,
                None,
            );
            heap[tail_slot].tail = next.get_ref();
            tail_slot = next.get_ref();
            carry = product >> BIGINT_DIGIT_BITS;
            current = cell.next_digit_cell(heap);
        }

        if carry != 0 {
            let next = IntegerRef::from_stored_cell(heap, carry as isize, None);
            heap[tail_slot].tail = next.get_ref();
        }

        result
    }

    /// Multiplies a non-zero magnitude by a power of the bigint digit base.
    ///
    /// This is Miranda's `shift`, which prepends zero cells to model
    /// multiplication by `IBASE^n` during schoolbook multiplication.
    /// Mutation/allocation: non-mutating with respect to the input integer; allocates fresh leading zero cells and reuses the existing tail chain.
    pub(super) fn multiply_magnitude_by_digit_base_power(
        &self,
        heap: &mut Heap,
        count: usize,
    ) -> IntegerRef {
        let mut shifted = *self;

        for _ in 0..count {
            shifted = IntegerRef::from_stored_cell(heap, 0, Some(shifted));
        }

        shifted
    }

    /// Divides a non-negative magnitude by one base digit and returns quotient and remainder.
    ///
    /// This is Miranda's `shortdiv`, whose purpose is to provide the single-digit
    /// fast path used by signed division and modulus while preserving Miranda remainder semantics.
    /// Mutation/allocation: non-mutating with respect to the input integer; allocates fresh quotient and remainder integers on the heap.
    pub(super) fn divide_magnitude_by_digit(
        &self,
        heap: &mut Heap,
        digit_divisor: isize,
    ) -> (IntegerRef, IntegerRef) {
        let mut digits = self.collect_magnitude_digits(heap);
        let remainder = divide_digit_vec_by_small(&mut digits, digit_divisor);

        let quotient = Self::build_integer_from_digits(heap, &digits, 0);
        let remainder_integer = IntegerRef::from_i64(heap, remainder as i64);

        (quotient, remainder_integer)
    }

    /// Divides one non-negative magnitude by another and returns quotient and remainder.
    ///
    /// This is Miranda's `longdiv`, the multi-digit helper underlying `bigdiv`
    /// and `bigmod` for divisors larger than one heap digit.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh quotient and remainder integers on the heap.
    pub(super) fn divide_magnitudes_long(
        &self,
        heap: &mut Heap,
        other: IntegerRef,
    ) -> (IntegerRef, IntegerRef) {
        let divisor_digits = other.collect_magnitude_digits(heap);
        let mut remainder_digits = self.collect_magnitude_digits(heap);

        if compare_digit_vectors(&remainder_digits, &divisor_digits) < 0 {
            return (
                IntegerRef::from_i64(heap, 0),
                Self::build_integer_from_digits(heap, &remainder_digits, 0),
            );
        }

        let max_shift = remainder_digits.len() - divisor_digits.len();
        let mut quotient_digits = vec![0; max_shift + 1];

        for shift in (0..=max_shift).rev() {
            let shifted_divisor = shift_digits_left(&divisor_digits, shift);
            if compare_digit_vectors(&remainder_digits, &shifted_divisor) < 0 {
                continue;
            }

            let quotient_digit =
                largest_fitting_quotient_digit(&remainder_digits, &divisor_digits, shift);
            if quotient_digit == 0 {
                continue;
            }

            let scaled_divisor = shift_digits_left(
                &multiply_digit_slice_by_digit(&divisor_digits, quotient_digit),
                shift,
            );
            remainder_digits = subtract_larger_magnitude_digits(&remainder_digits, &scaled_divisor);
            quotient_digits[shift] = quotient_digit;
        }

        normalize_digit_vec(&mut quotient_digits);

        (
            Self::build_integer_from_digits(heap, &quotient_digits, 0),
            Self::build_integer_from_digits(heap, &remainder_digits, 0),
        )
    }

    /// Counts how many heap digits are present in a trusted integer chain.
    ///
    /// This is Miranda's `len` helper so multiplication, division, and
    /// normalization logic can reason about magnitude size without raw tail walking.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(super) fn digit_count(&self, heap: &Heap) -> usize {
        let mut count = 1;
        let mut current = *self;

        while let Some(next) = current.next_digit_cell(heap) {
            count += 1;
            current = next;
        }

        count
    }

    /// Returns the most-significant digit of a trusted integer chain.
    ///
    /// This is Miranda's `msd`, which exists to support long-division scaling
    /// and size comparisons in the faithful arithmetic helpers.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(super) fn most_significant_digit(&self, heap: &Heap) -> isize {
        let mut current = *self;

        while let Some(next) = current.next_digit_cell(heap) {
            current = next;
        }

        current.digit_word(heap)
    }

    /// Returns the two most-significant digits packed into one host value.
    ///
    /// This is Miranda's `ms2d`, whose purpose is to estimate quotient digits
    /// during long division while preserving the original algorithm shape.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(super) fn most_significant_two_digits(&self, heap: &Heap) -> isize {
        let mut lower = self.digit_word(heap);
        let mut current = self
            .next_digit_cell(heap)
            .expect("most_significant_two_digits requires at least two digits");

        while let Some(next) = current.next_digit_cell(heap) {
            lower = current.digit_word(heap);
            current = next;
        }

        current.digit_word(heap) * BIGINT_DIGIT_BASE + lower
    }

    /// Compares the unsigned magnitudes of two trusted integers.
    ///
    /// This helper exists so signed operations at the `IntegerRef` layer can reuse
    /// the same magnitude ordering logic as the faithful arithmetic helpers.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(super) fn compare_magnitude_ordering(&self, heap: &Heap, other: IntegerRef) -> isize {
        compare_digit_vectors(
            &self.collect_magnitude_digits(heap),
            &other.collect_magnitude_digits(heap),
        )
    }
}

/// Compares two little-endian magnitude digit vectors.
///
/// This helper exists so heap-backed magnitude comparison and long-division scratch
/// work can share one ordering routine without duplicating the same slice walk.
/// Mutation/allocation: non-mutating; reads host-side digit slices and allocates nothing.
fn compare_digit_vectors(left: &[isize], right: &[isize]) -> isize {
    if left.len() != right.len() {
        return if left.len() > right.len() { 1 } else { -1 };
    }

    for index in (0..left.len()).rev() {
        if left[index] != right[index] {
            return if left[index] > right[index] { 1 } else { -1 };
        }
    }

    0
}

/// Subtracts the smaller magnitude digit vector from the larger one and trims high zeroes.
///
/// This helper exists to preserve the normalized result contract of Miranda's `big_sub`
/// while keeping borrow handling separate from sign selection.
/// Mutation/allocation: mutates only temporary digit-vector state local to the helper; does not touch heap data.
fn subtract_larger_magnitude_digits(larger: &[isize], smaller: &[isize]) -> Vec<isize> {
    let mut result = Vec::with_capacity(larger.len());
    let mut borrow = 0isize;

    for index in 0..larger.len() {
        let minuend = larger[index];
        let subtrahend = smaller.get(index).copied().unwrap_or(0);
        let difference = minuend - subtrahend - borrow;

        if difference < 0 {
            result.push(difference + BIGINT_DIGIT_BASE);
            borrow = 1;
        } else {
            result.push(difference);
            borrow = 0;
        }
    }

    while result.len() > 1 && result.last() == Some(&0) {
        result.pop();
    }

    result
}

/// Finds the largest quotient digit whose scaled divisor still fits in the remainder.
///
/// This helper exists so long division can choose one base-digit quotient limb at a
/// time without relying on repeated unit subtraction.
/// Mutation/allocation: non-mutating; reads only temporary digit slices and allocates nothing.
fn largest_fitting_quotient_digit(
    remainder_digits: &[isize],
    divisor_digits: &[isize],
    shift: usize,
) -> isize {
    let mut low = 0isize;
    let mut high = BIGINT_DIGIT_MASK;

    while low < high {
        let mid = (low + high + 1) / 2;
        let candidate =
            shift_digits_left(&multiply_digit_slice_by_digit(divisor_digits, mid), shift);

        if compare_digit_vectors(&candidate, remainder_digits) <= 0 {
            low = mid;
        } else {
            high = mid - 1;
        }
    }

    low
}

#[cfg(test)]
mod tests {
    use super::IntegerRef;
    use crate::big_num::constants::{BIGINT_DIGIT_BASE, BIGINT_DIGIT_MASK, SIGN_BIT_MASK};
    use crate::data::{Heap, Tag, Value};

    fn integer_from_digits(heap: &mut Heap, digits: &[isize]) -> IntegerRef {
        assert!(!digits.is_empty());

        let mut tail = 0;

        for &digit in digits.iter().rev() {
            let tail_value = if tail == 0 {
                Value::Data(0)
            } else {
                Value::Reference(tail)
            };
            tail = match heap.put_ref(Tag::Int, Value::Data(digit), tail_value) {
                Value::Reference(reference) => reference,
                _ => unreachable!(),
            };
        }

        IntegerRef::new(heap, Value::Reference(tail)).unwrap()
    }

    fn collect_digits(heap: &Heap, integer: IntegerRef) -> Vec<isize> {
        integer.collect_magnitude_digits(heap)
    }

    #[test]
    fn digit_count_counts_one_for_single_cell_and_more_for_longer_chains() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[1, 2, 3]);

        assert_eq!(integer.digit_count(&heap), 3);
    }

    #[test]
    fn most_significant_digit_reads_last_cell() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[1, 2, 5]);

        assert_eq!(integer.most_significant_digit(&heap), 5);
    }

    #[test]
    fn most_significant_two_digits_pack_the_two_highest_digits() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[1, 2, 5]);

        assert_eq!(
            integer.most_significant_two_digits(&heap),
            5 * BIGINT_DIGIT_BASE + 2
        );
    }

    #[test]
    fn multiply_magnitude_by_digit_handles_single_digit_products() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[7]);
        let product = integer.multiply_magnitude_by_digit(&mut heap, 3);

        assert_eq!(collect_digits(&heap, product), vec![21]);
    }

    #[test]
    fn multiply_magnitude_by_digit_carries_into_new_high_digit() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[BIGINT_DIGIT_MASK]);
        let product = integer.multiply_magnitude_by_digit(&mut heap, 2);

        assert_eq!(
            collect_digits(&heap, product),
            vec![BIGINT_DIGIT_MASK - 1, 1]
        );
    }

    #[test]
    fn multiply_magnitude_by_digit_handles_multi_cell_inputs() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[1, 1]);
        let product = integer.multiply_magnitude_by_digit(&mut heap, 3);

        assert_eq!(collect_digits(&heap, product), vec![3, 3]);
    }

    #[test]
    fn multiply_magnitude_by_digit_base_power_prepends_zero_digits() {
        let mut heap = Heap::default();
        let integer = integer_from_digits(&mut heap, &[4, 5]);
        let shifted = integer.multiply_magnitude_by_digit_base_power(&mut heap, 2);

        assert_eq!(collect_digits(&heap, shifted), vec![0, 0, 4, 5]);
    }

    #[test]
    fn add_magnitudes_with_result_sign_adds_digits_without_sign() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[5, 1]);
        let right = integer_from_digits(&mut heap, &[7, 2]);
        let sum = left.add_magnitudes_with_result_sign(&mut heap, right, 0);

        assert_eq!(collect_digits(&heap, sum), vec![12, 3]);
        assert!(!sum.is_negative(&heap));
    }

    #[test]
    fn add_magnitudes_with_result_sign_applies_negative_root_sign() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[5]);
        let right = integer_from_digits(&mut heap, &[7]);
        let sum = left.add_magnitudes_with_result_sign(&mut heap, right, SIGN_BIT_MASK);

        assert_eq!(collect_digits(&heap, sum), vec![12]);
        assert!(sum.is_negative(&heap));
    }

    #[test]
    fn add_magnitudes_with_result_sign_carries_across_multiple_digits() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[BIGINT_DIGIT_MASK, BIGINT_DIGIT_MASK]);
        let right = integer_from_digits(&mut heap, &[1]);
        let sum = left.add_magnitudes_with_result_sign(&mut heap, right, 0);

        assert_eq!(collect_digits(&heap, sum), vec![0, 0, 1]);
    }

    #[test]
    fn subtract_magnitudes_keeps_positive_when_left_is_larger() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[5, 1]);
        let right = integer_from_digits(&mut heap, &[3]);
        let difference = left.subtract_magnitudes(&mut heap, right);

        assert_eq!(collect_digits(&heap, difference), vec![2, 1]);
        assert!(!difference.is_negative(&heap));
    }

    #[test]
    fn subtract_magnitudes_borrows_across_digits() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[0, 1]);
        let right = integer_from_digits(&mut heap, &[1]);
        let difference = left.subtract_magnitudes(&mut heap, right);

        assert_eq!(collect_digits(&heap, difference), vec![BIGINT_DIGIT_MASK]);
        assert_eq!(difference.digit_count(&heap), 1);
    }

    #[test]
    fn subtract_magnitudes_returns_canonical_zero_for_equal_inputs() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[5, 1]);
        let right = integer_from_digits(&mut heap, &[5, 1]);
        let zero = left.subtract_magnitudes(&mut heap, right);

        assert!(zero.is_zero(&heap));
        assert!(zero.is_single_cell(&heap));
    }

    #[test]
    fn subtract_magnitudes_sets_negative_sign_when_right_is_larger() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[3]);
        let right = integer_from_digits(&mut heap, &[5, 1]);
        let difference = left.subtract_magnitudes(&mut heap, right);

        assert_eq!(collect_digits(&heap, difference), vec![2, 1]);
        assert!(difference.is_negative(&heap));
    }

    #[test]
    fn subtract_magnitudes_normalizes_trailing_zero_digits() {
        let mut heap = Heap::default();
        let left = integer_from_digits(&mut heap, &[0, 1]);
        let right = integer_from_digits(&mut heap, &[0]);
        let difference = left.subtract_magnitudes(&mut heap, right);

        assert_eq!(collect_digits(&heap, difference), vec![0, 1]);
    }

    #[test]
    fn divide_magnitude_by_digit_returns_quotient_and_remainder() {
        let mut heap = Heap::default();
        let value = integer_from_digits(&mut heap, &[8, 1]);
        let (quotient, remainder) = value.divide_magnitude_by_digit(&mut heap, 2);

        assert_eq!(collect_digits(&heap, quotient), vec![16388]);
        assert_eq!(remainder.to_i64_lossy(&heap), 0);
    }

    #[test]
    fn divide_magnitude_by_digit_keeps_non_zero_remainder() {
        let mut heap = Heap::default();
        let value = integer_from_digits(&mut heap, &[10]);
        let (quotient, remainder) = value.divide_magnitude_by_digit(&mut heap, 3);

        assert_eq!(collect_digits(&heap, quotient), vec![3]);
        assert_eq!(remainder.to_i64_lossy(&heap), 1);
    }

    #[test]
    fn divide_magnitude_by_digit_handles_multi_digit_quotients() {
        let mut heap = Heap::default();
        let value = integer_from_digits(&mut heap, &[0, 1]);
        let (quotient, remainder) = value.divide_magnitude_by_digit(&mut heap, 7);

        assert_eq!(collect_digits(&heap, quotient), vec![4681]);
        assert_eq!(remainder.to_i64_lossy(&heap), 1);
    }

    #[test]
    fn divide_magnitude_by_digit_preserves_canonical_zero_quotient() {
        let mut heap = Heap::default();
        let value = integer_from_digits(&mut heap, &[1]);
        let (quotient, remainder) = value.divide_magnitude_by_digit(&mut heap, 2);

        assert!(quotient.is_zero(&heap));
        assert!(remainder.is_single_cell(&heap));
        assert_eq!(remainder.to_i64_lossy(&heap), 1);
    }

    #[test]
    fn divide_magnitudes_long_returns_exact_result_when_divisible() {
        let mut heap = Heap::default();
        let dividend = integer_from_digits(&mut heap, &[0, 2]);
        let divisor = integer_from_digits(&mut heap, &[0, 1]);
        let (quotient, remainder) = dividend.divide_magnitudes_long(&mut heap, divisor);

        assert_eq!(collect_digits(&heap, quotient), vec![2]);
        assert!(remainder.is_zero(&heap));
    }

    #[test]
    fn divide_magnitudes_long_returns_remainder_when_not_divisible() {
        let mut heap = Heap::default();
        let dividend = integer_from_digits(&mut heap, &[5, 1]);
        let divisor = integer_from_digits(&mut heap, &[3]);
        let (quotient, remainder) = dividend.divide_magnitudes_long(&mut heap, divisor);

        assert_eq!(quotient.to_i64_lossy(&heap), 10924);
        assert_eq!(remainder.to_i64_lossy(&heap), 1);
    }

    #[test]
    fn divide_magnitudes_long_returns_zero_quotient_when_dividend_is_smaller() {
        let mut heap = Heap::default();
        let dividend = integer_from_digits(&mut heap, &[3]);
        let divisor = integer_from_digits(&mut heap, &[5, 1]);
        let (quotient, remainder) = dividend.divide_magnitudes_long(&mut heap, divisor);

        assert!(quotient.is_zero(&heap));
        assert_eq!(collect_digits(&heap, remainder), vec![3]);
    }

    #[test]
    fn divide_magnitudes_long_handles_equal_operands() {
        let mut heap = Heap::default();
        let dividend = integer_from_digits(&mut heap, &[5, 1]);
        let divisor = integer_from_digits(&mut heap, &[5, 1]);
        let (quotient, remainder) = dividend.divide_magnitudes_long(&mut heap, divisor);

        assert_eq!(quotient.to_i64_lossy(&heap), 1);
        assert!(remainder.is_zero(&heap));
    }
}
