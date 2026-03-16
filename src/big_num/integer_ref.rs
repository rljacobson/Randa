//! Heap proxy for Miranda `Tag::Int` objects.

use super::constants::{BIGINT_DIGIT_BASE, SIGN_BIT_MASK};
use crate::data::{api::HeapObjectProxy, Heap, RawValue, Tag, Value};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub(crate) struct IntegerRef {
    reference: RawValue,
}

impl IntegerRef {
    /// Validates and lifts a generic runtime value into an integer proxy.
    ///
    /// This is the checked bigint ingress constructor for sites that start from a generic
    /// `Value`; only `Value::Reference` pointing at `Tag::Int` is admitted.
    /// Mutation/allocation: non-mutating; reads heap shape only and allocates nothing.
    pub(super) fn new(heap: &Heap, value: Value) -> Option<Self> {
        match value {
            Value::Reference(reference)
                if heap[RawValue::from(Value::Reference(reference))].tag == Tag::Int =>
            {
                Some(IntegerRef {
                    reference: RawValue::from(Value::Reference(reference)),
                })
            }
            _ => None,
        }
    }

    /// Allocates one trusted integer cell from an already-encoded head word and optional tail.
    ///
    /// This is the low-level bigint-owned constructor for places that genuinely manipulate
    /// Miranda integer cell shape directly while still keeping tail links typed as `IntegerRef`.
    /// Mutation/allocation: allocates one fresh `Tag::Int` cell on the heap; does not mutate any
    /// existing integer cells.
    pub(super) fn from_stored_cell(
        heap: &mut Heap,
        head_word: RawValue,
        tail: Option<IntegerRef>,
    ) -> Self {
        let tail_value = tail.map_or(Value::Data(0), |integer| Value::from(integer.reference));

        match heap.put_ref(Tag::Int, Value::Data(head_word), tail_value) {
            Value::Reference(reference) => IntegerRef {
                reference: RawValue::from(Value::Reference(reference)),
            },
            _ => unreachable!(),
        }
    }

    /// Returns the raw head word of this trusted integer cell.
    ///
    /// This is the primitive trusted read used by higher-level digit and sign helpers,
    /// keeping heap-shape knowledge localized to the proxy.
    /// Mutation/allocation: non-mutating; reads an existing heap cell and allocates nothing.
    pub(super) fn raw_head_word(&self, heap: &Heap) -> RawValue {
        heap[self.reference].head
    }

    /// Extracts the least-significant digit from a first-cell head word.
    ///
    /// This mirrors Miranda's `digit0(x)` macro and exists so trusted code can
    /// read the first digit while masking off the sign bit in normalized integers.
    /// Mutation/allocation: non-mutating; inspects one raw word and allocates nothing.
    fn extract_low_digit_from_signed_head(head: RawValue) -> RawValue {
        head & super::constants::BIGINT_DIGIT_MASK
    }

    /// Encodes a one-cell integer payload using Miranda's sign-bit convention.
    ///
    /// This mirrors the heap-shape behavior behind Miranda's `stosmallint(x)` and
    /// is used for trusted single-cell construction and bytecode compatibility.
    /// Mutation/allocation: non-mutating; computes one raw word and allocates nothing.
    fn encode_single_cell_integer_head(value: RawValue) -> RawValue {
        if value < 0 {
            SIGN_BIT_MASK | (-value)
        } else {
            value
        }
    }

    /// Returns the least-significant digit stored in the first cell.
    ///
    /// This mirrors Miranda's `digit0(x)` macro and exists because the first cell
    /// carries both sign and digit payload in normalized heap integers.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn low_digit(&self, heap: &Heap) -> RawValue {
        Self::extract_low_digit_from_signed_head(self.raw_head_word(heap))
    }

    /// Returns the raw digit word stored in this cell head.
    ///
    /// This mirrors Miranda's `digit(x)` macro and is used by helpers that need the
    /// full stored word rather than the masked first-digit payload.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn digit_word(&self, heap: &Heap) -> RawValue {
        self.raw_head_word(heap)
    }

    /// Returns the next cell in the little-endian digit chain, if one exists.
    ///
    /// This mirrors Miranda's `rest(x)` traversal and exists so callers can walk
    /// trusted integer chains without manual tail-link surgery.
    /// Mutation/allocation: non-mutating; returns another proxy over existing heap data and allocates nothing.
    pub(super) fn next_digit_cell(&self, heap: &Heap) -> Option<IntegerRef> {
        let tail = heap[self.reference].tail;
        if tail == 0 {
            None
        } else {
            Some(IntegerRef { reference: tail })
        }
    }

    /// Returns whether this integer cell links to a higher-order digit cell.
    ///
    /// This is a convenience query used by normalization and shape-sensitive logic
    /// to distinguish one-cell integers from multi-cell integers.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    fn has_more_digits(&self, heap: &Heap) -> bool {
        self.next_digit_cell(heap).is_some()
    }

    /// Returns whether this trusted integer occupies exactly one heap cell.
    ///
    /// This is the Rust-side equivalent of the Miranda implementation's "small int"
    /// fast path, while remaining the same runtime integer type.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn is_single_cell(&self, heap: &Heap) -> bool {
        !self.has_more_digits(heap)
    }

    /// Returns whether this trusted integer is canonical zero.
    ///
    /// This exists to enforce the normalization rule that zero is represented by one
    /// non-negative cell and that negative zero must never be treated as normalized zero.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn is_zero(&self, heap: &Heap) -> bool {
        self.raw_head_word(heap) == 0 && heap[self.reference].tail == 0
    }

    /// Returns whether this trusted integer carries Miranda's negative sign bit.
    ///
    /// This mirrors Miranda's `neg(x)` query and supports sign-sensitive arithmetic
    /// and comparison without duplicating bit-level heap logic outside the proxy.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn is_negative(&self, heap: &Heap) -> bool {
        (self.raw_head_word(heap) & SIGN_BIT_MASK) != 0
    }

    /// Returns whether this trusted integer is non-negative under Miranda's sign convention.
    ///
    /// This mirrors Miranda's `poz(x)` behavior, which intentionally includes canonical zero.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn is_non_negative(&self, heap: &Heap) -> bool {
        !self.is_negative(heap)
    }

    /// Returns whether this trusted integer is a natural number in Miranda's sense.
    ///
    /// This matches `isnat(x)` in the C implementation: an integer value with tag
    /// `INT` whose sign is non-negative.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    fn is_natural_number(&self, heap: &Heap) -> bool {
        self.is_non_negative(heap)
    }

    /// Decodes the signed host value of a one-cell integer.
    ///
    /// This mirrors Miranda's `getsmallint(x)` and exists for scalar fast paths and
    /// tests that specifically reason about the canonical one-cell representation.
    /// Mutation/allocation: non-mutating; reads existing heap data and allocates nothing.
    pub(super) fn decode_single_cell_value(&self, heap: &Heap) -> RawValue {
        if self.is_negative(heap) {
            -Self::extract_low_digit_from_signed_head(self.raw_head_word(heap))
        } else {
            self.raw_head_word(heap)
        }
    }

    /// Compares two trusted integers using Miranda's signed ordering.
    ///
    /// This is Miranda's `bigcmp`, exposed so callers can observe Miranda ordering semantics
    /// through the proxy rather than through raw digit-chain traversal.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(crate) fn compare(&self, heap: &Heap, other: IntegerRef) -> isize {
        if self.is_negative(heap) && other.is_non_negative(heap) {
            return -1;
        }

        if self.is_non_negative(heap) && other.is_negative(heap) {
            return 1;
        }

        let magnitude_ordering = self.compare_magnitude_ordering(heap, other);
        if self.is_negative(heap) {
            -magnitude_ordering
        } else {
            magnitude_ordering
        }
    }

    /// Produces the additive inverse of this trusted integer.
    ///
    /// This is Miranda's `bignegate`, including the normalization rule that canonical
    /// zero remains zero rather than becoming negative zero.
    /// Mutation/allocation: returns the existing zero proxy unchanged for canonical zero; otherwise
    /// allocates one fresh root cell and reuses the existing tail chain without mutating it.
    pub(crate) fn negate(&self, heap: &mut Heap) -> IntegerRef {
        if self.is_zero(heap) {
            *self
        } else if self.is_negative(heap) {
            self.copy_with_sign_bit(heap, 0)
        } else {
            self.copy_with_sign_bit(heap, SIGN_BIT_MASK)
        }
    }

    /// Adds two trusted integers with Miranda sign handling.
    ///
    /// This is Miranda's `bigplus`, exposed so arithmetic clients can use a lifted operation
    /// while the implementation preserves Miranda's heap representation semantics.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh result cells on the heap.
    pub(crate) fn add(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        match (self.is_negative(heap), other.is_negative(heap)) {
            (false, false) => self.add_magnitudes_with_result_sign(heap, other, 0),
            (true, true) => self.add_magnitudes_with_result_sign(heap, other, SIGN_BIT_MASK),
            (false, true) => self.subtract_magnitudes(heap, other),
            (true, false) => other.subtract_magnitudes(heap, *self),
        }
    }

    /// Subtracts one trusted integer from another with Miranda sign handling.
    ///
    /// This is Miranda's `bigsub`, including the normalization behavior that trims
    /// redundant high-order zero cells before trusted results escape.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh result cells on the heap.
    pub(crate) fn subtract(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        match (self.is_negative(heap), other.is_negative(heap)) {
            (false, false) => self.subtract_magnitudes(heap, other),
            (false, true) => self.add_magnitudes_with_result_sign(heap, other, 0),
            (true, false) => self.add_magnitudes_with_result_sign(heap, other, SIGN_BIT_MASK),
            (true, true) => other.subtract_magnitudes(heap, *self),
        }
    }

    /// Multiplies two trusted integers using Miranda's quadratic bigint algorithm.
    ///
    /// This is Miranda's `bigtimes`, preserving sign behavior and canonical zero results.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh intermediate and result cells on the heap.
    pub(crate) fn multiply(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        if self.is_zero(heap) || other.is_zero(heap) {
            return IntegerRef::from_i64(heap, 0);
        }

        let mut result = IntegerRef::from_i64(heap, 0);
        let mut shift_count = 0usize;
        let mut current = Some(other);

        while let Some(cell) = current {
            let digit = if shift_count == 0 {
                cell.low_digit(heap)
            } else {
                cell.digit_word(heap)
            };

            if digit != 0 {
                let partial = self.multiply_magnitude_by_digit(heap, digit);
                let shifted = if shift_count == 0 || partial.is_zero(heap) {
                    partial
                } else {
                    partial.multiply_magnitude_by_digit_base_power(heap, shift_count)
                };
                result = result.add_magnitudes_with_result_sign(heap, shifted, 0);
            }

            shift_count += 1;
            current = cell.next_digit_cell(heap);
        }

        if self.is_negative(heap) ^ other.is_negative(heap) {
            result.copy_with_sign_bit(heap, SIGN_BIT_MASK)
        } else {
            result
        }
    }

    /// Divides one trusted integer by another using Miranda's `entier` semantics.
    ///
    /// This is Miranda's `bigdiv`, whose observable behavior differs from Rust/C
    /// truncating division because the quotient rounds toward negative infinity.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh quotient/remainder work and returns a fresh quotient result.
    pub(crate) fn divide(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        self.divide_and_modulus(heap, other).0
    }

    /// Computes the remainder using Miranda's divisor-signed modulus semantics.
    ///
    /// This is Miranda's `bigmod`, preserving the observable rule that the remainder
    /// has the sign of the divisor rather than the dividend.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh quotient/remainder work and returns a fresh remainder result.
    pub(crate) fn modulus(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        self.divide_and_modulus(heap, other).1
    }

    /// Raises this trusted integer to a non-negative integer power.
    ///
    /// This is Miranda's `bigpow`, which uses repeated squaring over the base-`2^15`
    /// digit representation.
    /// Mutation/allocation: non-mutating with respect to the base and exponent inputs; allocates fresh intermediate and result cells on the heap.
    pub(crate) fn power(&self, heap: &mut Heap, other: IntegerRef) -> IntegerRef {
        assert!(
            other.is_non_negative(heap),
            "Miranda bigpow expects a non-negative exponent"
        );

        let mut exponent = other;
        let mut base = *self;
        let mut result = IntegerRef::from_i64(heap, 1);

        while !exponent.is_zero(heap) {
            let (quotient, remainder) = exponent.divide_magnitude_by_digit(heap, 2);
            if !remainder.is_zero(heap) {
                result = result.multiply(heap, base);
            }

            exponent = quotient;
            let keep_squaring = !exponent.is_zero(heap);
            if keep_squaring {
                base = base.multiply(heap, base);
            }
        }

        result
    }

    /// Converts this trusted integer to `f64` using Miranda's little-endian digit accumulation.
    ///
    /// This is Miranda's `bigtodbl`, used for numeric coercions and comparisons while
    /// preserving the original base-`2^15` accumulation order.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(crate) fn to_f64_lossy(self, heap: &Heap) -> f64 {
        let negative = self.is_negative(heap);
        let mut scale = 1.0f64;
        let mut value = self.low_digit(heap) as f64;
        let mut current = self.next_digit_cell(heap);

        while let Some(cell) = current {
            scale *= BIGINT_DIGIT_BASE as f64;
            value += scale * cell.digit_word(heap) as f64;
            current = cell.next_digit_cell(heap);
        }

        if negative {
            -value
        } else {
            value
        }
    }

    /// Constructs an integer from `f64` using Miranda's `entier` semantics.
    ///
    /// This is Miranda's `dbltobig`, which floors first, then converts the absolute
    /// magnitude into base-`2^15` digits before re-applying the sign bit.
    /// Mutation/allocation: allocates a fresh normalized heap integer; does not mutate any existing integer cells.
    pub(crate) fn from_f64_floor(heap: &mut Heap, value: f64) -> IntegerRef {
        let negative = value < 0.0;
        let mut magnitude = value.floor().abs();
        let mut digits = vec![(magnitude % BIGINT_DIGIT_BASE as f64) as isize];
        magnitude = (magnitude - digits[0] as f64) / BIGINT_DIGIT_BASE as f64;

        while magnitude > 0.0 {
            let digit = magnitude % BIGINT_DIGIT_BASE as f64;
            magnitude = (magnitude - digit) / BIGINT_DIGIT_BASE as f64;
            digits.push(digit as isize);
        }

        Self::build_integer_from_digits(heap, &digits, if negative { SIGN_BIT_MASK } else { 0 })
    }

    /// Computes the natural logarithm of a positive trusted integer.
    ///
    /// This is Miranda's `biglog`, which derives a scaled leading mantissa from the
    /// most-significant digits and then adds the base-`2^15` exponent contribution.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(crate) fn natural_log(&self, heap: &Heap) -> f64 {
        assert!(
            !self.is_negative(heap) && !self.is_zero(heap),
            "Miranda biglog requires a positive integer"
        );

        let mut exponent = 0usize;
        let mut mantissa = self.digit_word(heap) as f64;
        let mut current = self.next_digit_cell(heap);

        while let Some(cell) = current {
            exponent += 1;
            mantissa = cell.digit_word(heap) as f64 + mantissa / BIGINT_DIGIT_BASE as f64;
            current = cell.next_digit_cell(heap);
        }

        mantissa.ln() + exponent as f64 * (BIGINT_DIGIT_BASE as f64).ln()
    }

    /// Computes the base-10 logarithm of a positive trusted integer.
    ///
    /// This is Miranda's `biglog10`, which mirrors `biglog` but uses `log10` for
    /// Miranda's decimal-oriented numeric operations.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(crate) fn log10(&self, heap: &Heap) -> f64 {
        assert!(
            !self.is_negative(heap) && !self.is_zero(heap),
            "Miranda biglog10 requires a positive integer"
        );

        let mut exponent = 0usize;
        let mut mantissa = self.digit_word(heap) as f64;
        let mut current = self.next_digit_cell(heap);

        while let Some(cell) = current {
            exponent += 1;
            mantissa = cell.digit_word(heap) as f64 + mantissa / BIGINT_DIGIT_BASE as f64;
            current = cell.next_digit_cell(heap);
        }

        mantissa.log10() + exponent as f64 * (BIGINT_DIGIT_BASE as f64).log10()
    }

    /// Constructs a normalized heap integer from a host `i64`.
    ///
    /// This is Miranda's `sto_int`, giving the bigint subsystem a scalar ingress path
    /// that preserves Miranda's canonical little-endian digit-chain shape.
    /// Mutation/allocation: allocates a fresh normalized heap integer; does not mutate any existing integer cells.
    pub(crate) fn from_i64(heap: &mut Heap, value: i64) -> IntegerRef {
        if value == 0 {
            return IntegerRef::from_stored_cell(heap, 0, None);
        }

        let negative = value < 0;
        let mut magnitude = if negative {
            -(value as i128)
        } else {
            value as i128
        };

        let mut digits = Vec::new();
        while magnitude != 0 {
            digits.push((magnitude % BIGINT_DIGIT_BASE as i128) as isize);
            magnitude /= BIGINT_DIGIT_BASE as i128;
        }

        Self::build_integer_from_digits(heap, &digits, if negative { SIGN_BIT_MASK } else { 0 })
    }

    /// Extracts a lossy or saturating host `i64` from a trusted heap integer.
    ///
    /// This is Miranda's `get_int`, including Miranda's bounded extraction behavior
    /// when the bigint exceeds the representable shifted-digit range.
    /// Mutation/allocation: non-mutating; reads existing heap integers and allocates nothing.
    pub(crate) fn to_i64_lossy(self, heap: &Heap) -> i64 {
        let mut value = 0i64;
        let mut factor = 1i64;
        let mut digits_seen = 0usize;
        let mut current = Some(self);

        while let Some(cell) = current {
            if digits_seen == 4 {
                return if self.is_negative(heap) {
                    -(1i64 << 60)
                } else {
                    1i64 << 60
                };
            }

            let digit = if digits_seen == 0 {
                cell.low_digit(heap) as i64
            } else {
                cell.digit_word(heap) as i64
            };
            value += digit * factor;
            factor <<= 15;
            digits_seen += 1;
            current = cell.next_digit_cell(heap);
        }

        if self.is_negative(heap) {
            -value
        } else {
            value
        }
    }

    /// Copies this integer into a fresh root cell with the requested sign bit.
    ///
    /// This helper exists so signed operations can preserve Miranda's first-cell sign convention
    /// while reusing tails in the same way `bignegate` does in C. Mutation/allocation: returns the
    /// existing zero proxy unchanged for canonical zero; otherwise allocates one fresh root cell
    /// and reuses the existing tail chain without mutating it.
    fn copy_with_sign_bit(&self, heap: &mut Heap, sign_bit: RawValue) -> IntegerRef {
        if self.is_zero(heap) {
            return *self;
        }

        IntegerRef::from_stored_cell(
            heap,
            self.low_digit(heap) | sign_bit,
            self.next_digit_cell(heap),
        )
    }

    /// Computes Miranda's floor-style quotient and divisor-signed remainder together.
    ///
    /// This helper exists so `bigdiv` and `bigmod` can share the same signed-adjustment
    /// logic after magnitude division, matching the observable semantics recorded in the parity docs.
    /// Mutation/allocation: non-mutating with respect to both inputs; allocates fresh quotient/remainder work and result cells on the heap.
    fn divide_and_modulus(&self, heap: &mut Heap, other: IntegerRef) -> (IntegerRef, IntegerRef) {
        assert!(
            !other.is_zero(heap),
            "Miranda division by zero is undefined"
        );

        let self_negative = self.is_negative(heap);
        let other_negative = other.is_negative(heap);
        let mixed_signs = self_negative ^ other_negative;
        let other_is_single_cell = other.is_single_cell(heap);

        let (quotient_magnitude, remainder_magnitude) = if other_is_single_cell {
            let divisor_digit = other.low_digit(heap);
            self.divide_magnitude_by_digit(heap, divisor_digit)
        } else {
            self.divide_magnitudes_long(heap, other)
        };

        if remainder_magnitude.is_zero(heap) {
            let quotient = if mixed_signs {
                quotient_magnitude.copy_with_sign_bit(heap, SIGN_BIT_MASK)
            } else {
                quotient_magnitude
            };
            return (quotient, remainder_magnitude);
        }

        if mixed_signs {
            let one = IntegerRef::from_i64(heap, 1);
            let quotient =
                quotient_magnitude.add_magnitudes_with_result_sign(heap, one, SIGN_BIT_MASK);
            let remainder = other.subtract_magnitudes(heap, remainder_magnitude);
            let remainder = if other_negative {
                remainder.copy_with_sign_bit(heap, SIGN_BIT_MASK)
            } else {
                remainder
            };
            (quotient, remainder)
        } else {
            let remainder = if other_negative {
                remainder_magnitude.copy_with_sign_bit(heap, SIGN_BIT_MASK)
            } else {
                remainder_magnitude
            };
            (quotient_magnitude, remainder)
        }
    }
}

impl HeapObjectProxy for IntegerRef {
    /// Rebuilds a proxy from an existing raw heap reference.
    ///
    /// This trait hook keeps `IntegerRef` aligned with the general `HeapObjectProxy`
    /// pattern used throughout `randa`.
    /// Mutation/allocation: non-mutating; wraps an existing raw reference and allocates nothing.
    fn from_ref(reference: RawValue) -> Self {
        IntegerRef { reference }
    }

    /// Returns the raw heap reference carried by this proxy.
    ///
    /// This exists so typed bigint ownership can interoperate with surrounding heap
    /// APIs without exposing the integer shape itself.
    /// Mutation/allocation: non-mutating; returns stored proxy identity and allocates nothing.
    fn get_ref(&self) -> RawValue {
        self.reference
    }
}

#[cfg(test)]
mod tests {
    use super::IntegerRef;
    use crate::big_num::constants::SIGN_BIT_MASK;
    use crate::data::{api::HeapObjectProxy, Heap, Tag, Value};

    fn int_ref(heap: &mut Heap, head: isize, tail: isize) -> IntegerRef {
        let tail_value = if tail == 0 {
            Value::Data(0)
        } else {
            Value::from(tail)
        };
        let reference = match heap.put_ref(Tag::Int, Value::Data(head), tail_value) {
            Value::Reference(reference) => reference,
            _ => unreachable!(),
        };

        IntegerRef::new(heap, Value::Reference(reference)).unwrap()
    }

    #[test]
    fn new_accepts_integer_references() {
        let mut heap = Heap::default();
        let integer = int_ref(&mut heap, 7, 0);

        assert_eq!(
            IntegerRef::new(&heap, Value::from(integer.get_ref())),
            Some(integer)
        );
    }

    #[test]
    fn new_rejects_non_references() {
        let heap = Heap::default();

        assert_eq!(IntegerRef::new(&heap, Value::Data(7)), None);
        assert_eq!(IntegerRef::new(&heap, Value::None), None);
    }

    #[test]
    fn new_rejects_non_integer_references() {
        let mut heap = Heap::default();
        let non_integer = match heap.put_ref(Tag::Cons, Value::Data(1), Value::Data(0)) {
            Value::Reference(reference) => reference,
            _ => unreachable!(),
        };

        assert_eq!(IntegerRef::new(&heap, Value::Reference(non_integer)), None);
    }

    #[test]
    fn low_digit_extraction_masks_off_sign_bit() {
        let encoded = SIGN_BIT_MASK | 12345;
        assert_eq!(
            IntegerRef::extract_low_digit_from_signed_head(encoded),
            12345
        );
    }

    #[test]
    fn single_cell_positive_value_round_trips() {
        let encoded = IntegerRef::encode_single_cell_integer_head(42);
        let mut heap = Heap::default();
        let integer = int_ref(&mut heap, encoded, 0);

        assert_eq!(encoded, 42);
        assert_eq!(integer.decode_single_cell_value(&heap), 42);
    }

    #[test]
    fn single_cell_negative_value_round_trips() {
        let encoded = IntegerRef::encode_single_cell_integer_head(-42);
        let mut heap = Heap::default();
        let integer = int_ref(&mut heap, encoded, 0);

        assert_eq!(encoded, SIGN_BIT_MASK | 42);
        assert_eq!(integer.decode_single_cell_value(&heap), -42);
    }

    #[test]
    fn zero_is_canonical_single_cell_integer() {
        let mut heap = Heap::default();
        let zero = int_ref(&mut heap, 0, 0);

        assert!(zero.is_zero(&heap));
        assert!(zero.is_single_cell(&heap));
        assert!(zero.is_natural_number(&heap));
        assert!(!zero.is_negative(&heap));
        assert_eq!(zero.decode_single_cell_value(&heap), 0);
    }

    #[test]
    fn negative_single_cell_integer_decodes_sign_from_head() {
        let mut heap = Heap::default();
        let negative = int_ref(
            &mut heap,
            IntegerRef::encode_single_cell_integer_head(-42),
            0,
        );

        assert!(negative.is_negative(&heap));
        assert!(!negative.is_non_negative(&heap));
        assert!(!negative.is_natural_number(&heap));
        assert_eq!(negative.low_digit(&heap), 42);
        assert_eq!(negative.decode_single_cell_value(&heap), -42);
    }

    #[test]
    fn positive_single_cell_integer_is_non_negative() {
        let mut heap = Heap::default();
        let positive = int_ref(&mut heap, 123, 0);

        assert!(positive.is_single_cell(&heap));
        assert!(positive.is_non_negative(&heap));
        assert!(!positive.is_negative(&heap));
        assert_eq!(positive.low_digit(&heap), 123);
        assert_eq!(positive.digit_word(&heap), 123);
        assert_eq!(positive.decode_single_cell_value(&heap), 123);
    }

    #[test]
    fn multi_cell_integer_exposes_next_digit_chain() {
        let mut heap = Heap::default();
        let tail = int_ref(&mut heap, 2, 0);
        let head = int_ref(&mut heap, 1, tail.get_ref());

        assert!(!head.is_single_cell(&heap));
        assert!(head.has_more_digits(&heap));
        assert_eq!(head.next_digit_cell(&heap), Some(tail));
        assert_eq!(head.next_digit_cell(&heap).unwrap().low_digit(&heap), 2);
    }

    #[test]
    fn negative_zero_shape_is_not_treated_as_canonical_zero() {
        let mut heap = Heap::default();
        let negative_zero = int_ref(&mut heap, SIGN_BIT_MASK, 0);

        assert!(!negative_zero.is_zero(&heap));
        assert!(negative_zero.is_negative(&heap));
        assert_eq!(negative_zero.decode_single_cell_value(&heap), 0);
    }

    #[test]
    fn compare_orders_negative_before_positive_and_uses_magnitude_within_sign() {
        let mut heap = Heap::default();
        let negative = IntegerRef::from_i64(&mut heap, -3);
        let positive = IntegerRef::from_i64(&mut heap, 2);
        let larger_negative = IntegerRef::from_i64(&mut heap, -5);

        assert_eq!(negative.compare(&heap, positive), -1);
        assert_eq!(positive.compare(&heap, negative), 1);
        assert_eq!(negative.compare(&heap, larger_negative), 1);
    }

    #[test]
    fn negate_preserves_zero_and_flips_sign_for_non_zero_values() {
        let mut heap = Heap::default();
        let zero = IntegerRef::from_i64(&mut heap, 0);
        let positive = IntegerRef::from_i64(&mut heap, 17);
        let negative = IntegerRef::from_i64(&mut heap, -17);

        assert_eq!(zero.negate(&mut heap).to_i64_lossy(&heap), 0);
        assert_eq!(positive.negate(&mut heap).to_i64_lossy(&heap), -17);
        assert_eq!(negative.negate(&mut heap).to_i64_lossy(&heap), 17);
    }

    #[test]
    fn add_and_subtract_follow_miranda_signed_integer_rules() {
        let mut heap = Heap::default();
        let a = IntegerRef::from_i64(&mut heap, 7);
        let b = IntegerRef::from_i64(&mut heap, -3);

        assert_eq!(a.add(&mut heap, b).to_i64_lossy(&heap), 4);
        assert_eq!(b.add(&mut heap, a).to_i64_lossy(&heap), 4);
        assert_eq!(a.subtract(&mut heap, b).to_i64_lossy(&heap), 10);
        assert_eq!(b.subtract(&mut heap, a).to_i64_lossy(&heap), -10);
    }

    #[test]
    fn multiply_applies_sign_and_zero_rules() {
        let mut heap = Heap::default();
        let a = IntegerRef::from_i64(&mut heap, -12);
        let b = IntegerRef::from_i64(&mut heap, 5);
        let zero = IntegerRef::from_i64(&mut heap, 0);

        assert_eq!(a.multiply(&mut heap, b).to_i64_lossy(&heap), -60);
        assert_eq!(a.multiply(&mut heap, zero).to_i64_lossy(&heap), 0);
    }

    #[test]
    fn divide_and_modulus_follow_miranda_entier_semantics() {
        let mut heap = Heap::default();
        let seven = IntegerRef::from_i64(&mut heap, 7);
        let minus_seven = IntegerRef::from_i64(&mut heap, -7);
        let three = IntegerRef::from_i64(&mut heap, 3);
        let minus_three = IntegerRef::from_i64(&mut heap, -3);

        assert_eq!(seven.divide(&mut heap, three).to_i64_lossy(&heap), 2);
        assert_eq!(seven.modulus(&mut heap, three).to_i64_lossy(&heap), 1);

        assert_eq!(minus_seven.divide(&mut heap, three).to_i64_lossy(&heap), -3);
        assert_eq!(minus_seven.modulus(&mut heap, three).to_i64_lossy(&heap), 2);

        assert_eq!(seven.divide(&mut heap, minus_three).to_i64_lossy(&heap), -3);
        assert_eq!(
            seven.modulus(&mut heap, minus_three).to_i64_lossy(&heap),
            -2
        );

        assert_eq!(
            minus_seven
                .divide(&mut heap, minus_three)
                .to_i64_lossy(&heap),
            2
        );
        assert_eq!(
            minus_seven
                .modulus(&mut heap, minus_three)
                .to_i64_lossy(&heap),
            -1
        );
    }

    #[test]
    fn from_i64_and_to_i64_lossy_round_trip_small_values_and_saturate_large_values() {
        let mut heap = Heap::default();
        let value = IntegerRef::from_i64(&mut heap, -123456);

        assert_eq!(value.to_i64_lossy(&heap), -123456);

        let d4 = int_ref(&mut heap, 1, 0);
        let d3 = int_ref(&mut heap, 1, d4.get_ref());
        let d2 = int_ref(&mut heap, 1, d3.get_ref());
        let d1 = int_ref(&mut heap, 1, d2.get_ref());
        let huge = int_ref(&mut heap, SIGN_BIT_MASK | 1, d1.get_ref());
        assert_eq!(huge.to_i64_lossy(&heap), -(1i64 << 60));
    }

    #[test]
    fn from_i64_encodes_negative_single_cell_shape_internally() {
        let mut heap = Heap::default();
        let value = IntegerRef::from_i64(&mut heap, -42);

        assert_eq!(heap[value.get_ref()].tag, Tag::Int);
        assert_eq!(heap[value.get_ref()].head, SIGN_BIT_MASK | 42);
        assert_eq!(heap[value.get_ref()].tail, 0);
    }

    #[test]
    fn from_i64_keeps_negative_short_boundary_in_single_cell_shape() {
        let mut heap = Heap::default();
        let value = IntegerRef::from_i64(&mut heap, -128);

        assert_eq!(heap[value.get_ref()].tag, Tag::Int);
        assert_eq!(heap[value.get_ref()].head, SIGN_BIT_MASK | 128);
        assert_eq!(heap[value.get_ref()].tail, 0);
    }

    #[test]
    fn power_uses_non_negative_integer_exponents() {
        let mut heap = Heap::default();
        let base = IntegerRef::from_i64(&mut heap, -3);
        let exponent = IntegerRef::from_i64(&mut heap, 3);

        assert_eq!(base.power(&mut heap, exponent).to_i64_lossy(&heap), -27);
    }

    #[test]
    fn compare_recognizes_equality_and_multi_cell_ordering() {
        let mut heap = Heap::default();
        let equal_left = IntegerRef::from_i64(&mut heap, 40_000);
        let equal_right = IntegerRef::from_i64(&mut heap, 40_000);
        let larger = IntegerRef::from_i64(&mut heap, 70_000);

        assert_eq!(equal_left.compare(&heap, equal_right), 0);
        assert_eq!(equal_left.compare(&heap, larger), -1);
        assert_eq!(larger.compare(&heap, equal_left), 1);
    }

    #[test]
    fn signed_multi_cell_addition_and_subtraction_round_trip_through_i64() {
        let mut heap = Heap::default();
        let left = IntegerRef::from_i64(&mut heap, 70_000);
        let right = IntegerRef::from_i64(&mut heap, -40_000);

        assert_eq!(left.add(&mut heap, right).to_i64_lossy(&heap), 30_000);
        assert_eq!(left.subtract(&mut heap, right).to_i64_lossy(&heap), 110_000);
        assert_eq!(
            right.subtract(&mut heap, left).to_i64_lossy(&heap),
            -110_000
        );
    }

    #[test]
    fn multi_cell_multiplication_and_division_round_trip_through_i64() {
        let mut heap = Heap::default();
        let left = IntegerRef::from_i64(&mut heap, 70_000);
        let right = IntegerRef::from_i64(&mut heap, 123);

        let product = left.multiply(&mut heap, right);
        assert_eq!(product.to_i64_lossy(&heap), 8_610_000);

        assert_eq!(product.divide(&mut heap, right).to_i64_lossy(&heap), 70_000);
        assert_eq!(product.modulus(&mut heap, right).to_i64_lossy(&heap), 0);
    }

    #[test]
    fn integer_ref_exposes_text_boundary_entry_points() {
        let mut heap = Heap::default();
        let parsed = IntegerRef::parse_hex_text(&mut heap, "-0xff");

        assert_eq!(parsed.to_i64_lossy(&heap), -255);
        assert_eq!(parsed.format_decimal_text(&mut heap), "-255");
        assert_eq!(parsed.format_hex_text(&mut heap), "-0xff");
        assert_eq!(parsed.format_octal_text(&mut heap), "-0o377");
    }

    #[test]
    fn integer_ref_exposes_bytecode_boundary_entry_points() {
        let mut heap = Heap::default();
        let short = IntegerRef::decode_short_bytecode(&mut heap, 0xff);
        let long = IntegerRef::decode_int_x_bytecode(&mut heap, &[1, 2, 3, -1]);

        assert_eq!(short.to_i64_lossy(&heap), -1);
        assert_eq!(short.encode_for_dump_bytecode(&heap), vec![-1]);
        assert_eq!(long.encode_for_dump_bytecode(&heap), vec![1, 2, 3, -1]);
    }

    #[test]
    fn to_f64_lossy_preserves_sign_and_multi_cell_magnitude() {
        let mut heap = Heap::default();
        let positive = IntegerRef::from_i64(&mut heap, 70_000);
        let negative = IntegerRef::from_i64(&mut heap, -70_000);

        assert_eq!(positive.to_f64_lossy(&heap), 70_000.0);
        assert_eq!(negative.to_f64_lossy(&heap), -70_000.0);
    }

    #[test]
    fn from_f64_floor_matches_entier_style_behavior() {
        let mut heap = Heap::default();

        assert_eq!(
            IntegerRef::from_f64_floor(&mut heap, 3.7).to_i64_lossy(&heap),
            3
        );
        assert_eq!(
            IntegerRef::from_f64_floor(&mut heap, -3.7).to_i64_lossy(&heap),
            -4
        );
        assert_eq!(
            IntegerRef::from_f64_floor(&mut heap, -0.5).to_i64_lossy(&heap),
            -1
        );
        assert_eq!(
            IntegerRef::from_f64_floor(&mut heap, -4.0).to_i64_lossy(&heap),
            -4
        );
        assert_eq!(
            IntegerRef::from_f64_floor(&mut heap, 0.0).to_i64_lossy(&heap),
            0
        );
    }

    #[test]
    fn to_i64_lossy_preserves_exact_60_bit_boundary_and_saturates_beyond_it() {
        let mut heap = Heap::default();
        let max_exact = IntegerRef::from_i64(&mut heap, (1i64 << 60) - 1);
        let min_exact = IntegerRef::from_i64(&mut heap, -(1i64 << 60) + 1);
        let positive_saturating = IntegerRef::decode_int_x_bytecode(&mut heap, &[0, 0, 0, 0, 1]);
        let negative_saturating =
            IntegerRef::decode_int_x_bytecode(&mut heap, &[(SIGN_BIT_MASK as isize), 0, 0, 0, 1]);

        assert_eq!(max_exact.to_i64_lossy(&heap), (1i64 << 60) - 1);
        assert_eq!(min_exact.to_i64_lossy(&heap), -(1i64 << 60) + 1);
        assert_eq!(positive_saturating.to_i64_lossy(&heap), 1i64 << 60);
        assert_eq!(negative_saturating.to_i64_lossy(&heap), -(1i64 << 60));
    }

    #[test]
    fn divide_and_modulus_do_not_adjust_exact_mixed_sign_division() {
        let mut heap = Heap::default();
        let minus_six = IntegerRef::from_i64(&mut heap, -6);
        let six = IntegerRef::from_i64(&mut heap, 6);
        let three = IntegerRef::from_i64(&mut heap, 3);
        let minus_three = IntegerRef::from_i64(&mut heap, -3);

        assert_eq!(minus_six.divide(&mut heap, three).to_i64_lossy(&heap), -2);
        assert_eq!(minus_six.modulus(&mut heap, three).to_i64_lossy(&heap), 0);
        assert_eq!(six.divide(&mut heap, minus_three).to_i64_lossy(&heap), -2);
        assert_eq!(six.modulus(&mut heap, minus_three).to_i64_lossy(&heap), 0);
    }

    #[test]
    fn logarithm_helpers_match_host_results_for_small_values() {
        let mut heap = Heap::default();
        let value = IntegerRef::from_i64(&mut heap, 1000);

        let natural_log = value.natural_log(&heap);
        let base10_log = value.log10(&heap);

        assert!((natural_log - 1000f64.ln()).abs() < 1e-10);
        assert!((base10_log - 3.0).abs() < 1e-10);
    }

    #[test]
    fn logarithm_helpers_handle_multi_cell_values() {
        let mut heap = Heap::default();
        let value = IntegerRef::from_i64(&mut heap, 70_000);

        assert!((value.natural_log(&heap) - 70_000f64.ln()).abs() < 1e-10);
        assert!((value.log10(&heap) - 70_000f64.log10()).abs() < 1e-10);
    }
}
