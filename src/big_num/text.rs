//! Dormant text parse and format support for Miranda integers.

use super::constants::{
    DECIMAL_CHUNK_BASE, DECIMAL_CHUNK_WIDTH, HEX_CHUNK_BASE, OCTAL_CHUNK_BASE, SIGN_BIT_MASK,
};
use super::integer_ref::IntegerRef;
use super::internal::{
    add_small_to_digit_vec_in_place, divide_digit_vec_by_small,
    multiply_digit_vec_by_small_in_place,
};
use crate::data::Heap;

impl IntegerRef {
    /// Parses a decimal numeral into a trusted integer proxy.
    ///
    /// This is the typed bigint-owned decimal text ingress path.
    /// Mutation/allocation: allocates a fresh normalized heap integer; does not mutate any existing integer cells.
    pub(crate) fn parse_decimal_text(heap: &mut Heap, text: &str) -> Self {
        Self::parse_text_with_radix(heap, text, 10, DECIMAL_CHUNK_BASE)
    }

    /// Parses a hexadecimal numeral into a trusted integer proxy.
    ///
    /// This is the typed bigint-owned hexadecimal text ingress path.
    /// Mutation/allocation: allocates a fresh normalized heap integer; does not mutate any existing integer cells.
    pub(crate) fn parse_hex_text(heap: &mut Heap, text: &str) -> Self {
        Self::parse_text_with_radix(
            heap,
            &Self::strip_radix_prefix_preserving_sign(text, "0x"),
            16,
            HEX_CHUNK_BASE,
        )
    }

    /// Parses an octal numeral into a trusted integer proxy.
    ///
    /// This is the typed bigint-owned octal text ingress path.
    /// Mutation/allocation: allocates a fresh normalized heap integer; does not mutate any existing integer cells.
    pub(crate) fn parse_octal_text(heap: &mut Heap, text: &str) -> Self {
        Self::parse_text_with_radix(
            heap,
            &Self::strip_radix_prefix_preserving_sign(text, "0o"),
            8,
            OCTAL_CHUNK_BASE,
        )
    }

    /// Formats this trusted integer as decimal text.
    ///
    /// This is the typed bigint-owned decimal text egress path.
    /// Mutation/allocation: non-mutating with respect to the heap integer; allocates only temporary host-side formatting buffers.
    pub(crate) fn format_decimal_text(&self, heap: &mut Heap) -> String {
        if self.is_zero(heap) {
            return "0".to_string();
        }

        let negative = self.is_negative(heap);
        let mut digits = self.collect_magnitude_digits(heap);
        let mut chunks = Vec::new();

        while !(digits.len() == 1 && digits[0] == 0) {
            let remainder = divide_digit_vec_by_small(&mut digits, DECIMAL_CHUNK_BASE);
            chunks.push(remainder);
        }

        let mut rendered = String::new();
        if negative {
            rendered.push('-');
        }

        if let Some(last) = chunks.pop() {
            rendered.push_str(&last.to_string());
        }
        while let Some(chunk) = chunks.pop() {
            rendered.push_str(&format!("{:0width$}", chunk, width = DECIMAL_CHUNK_WIDTH));
        }

        rendered
    }

    /// Formats this trusted integer as hexadecimal text.
    ///
    /// This is the typed bigint-owned hexadecimal text egress path.
    /// Mutation/allocation: non-mutating with respect to the heap integer; allocates only temporary host-side formatting buffers.
    pub(crate) fn format_hex_text(&self, heap: &mut Heap) -> String {
        self.format_as_radix_text(heap, 16, "0x")
    }

    /// Formats this trusted integer as octal text.
    ///
    /// This is the typed bigint-owned octal text egress path.
    /// Mutation/allocation: non-mutating with respect to the heap integer; allocates only temporary host-side formatting buffers.
    pub(crate) fn format_octal_text(&self, heap: &mut Heap) -> String {
        self.format_as_radix_text(heap, 8, "0o")
    }

    /// Removes an expected radix prefix after an optional leading minus sign.
    /// Mutation/allocation: non-mutating with respect to heap data; may allocate a fresh host-side `String`.
    fn strip_radix_prefix_preserving_sign(text: &str, prefix: &str) -> String {
        if let Some(rest) = text.strip_prefix('-') {
            if let Some(without_prefix) = rest.strip_prefix(prefix) {
                return format!("-{without_prefix}");
            }
            return text.to_string();
        }

        text.strip_prefix(prefix).unwrap_or(text).to_string()
    }

    /// Parses text in the given radix using Miranda-style chunked multiply-add conversion.
    /// Mutation/allocation: allocates a fresh normalized heap integer and mutates only temporary host-side digit vectors.
    fn parse_text_with_radix(
        heap: &mut Heap,
        text: &str,
        radix: isize,
        chunk_limit: isize,
    ) -> Self {
        let negative = text.starts_with('-');
        let unsigned_text = text.trim_start_matches('-');
        let mut digits = vec![0isize];
        let chars: Vec<char> = unsigned_text.chars().collect();
        let mut index = 0usize;

        while index < chars.len() {
            let mut chunk_value = Self::digit_value(chars[index]);
            let mut factor = radix;
            index += 1;

            while index < chars.len() && factor < chunk_limit {
                chunk_value = radix * chunk_value + Self::digit_value(chars[index]);
                factor *= radix;
                index += 1;
            }

            multiply_digit_vec_by_small_in_place(&mut digits, factor);
            add_small_to_digit_vec_in_place(&mut digits, chunk_value);
        }

        if negative && !(digits.len() == 1 && digits[0] == 0) {
            Self::build_integer_from_digits(heap, &digits, SIGN_BIT_MASK)
        } else {
            Self::build_integer_from_digits(heap, &digits, 0)
        }
    }

    /// Formats this trusted integer in a non-decimal radix using repeated small division.
    /// Mutation/allocation: non-mutating with respect to the heap integer; allocates only temporary host-side formatting buffers.
    fn format_as_radix_text(&self, heap: &mut Heap, radix: isize, prefix: &str) -> String {
        if self.is_zero(heap) {
            return format!("{prefix}0");
        }

        let negative = self.is_negative(heap);
        let mut digits = self.collect_magnitude_digits(heap);
        let mut rendered_digits = Vec::new();

        while !(digits.len() == 1 && digits[0] == 0) {
            let remainder = divide_digit_vec_by_small(&mut digits, radix);
            rendered_digits.push(char::from_digit(remainder as u32, radix as u32).unwrap());
        }

        rendered_digits.reverse();

        let mut rendered = String::new();
        if negative {
            rendered.push('-');
        }
        rendered.push_str(prefix);
        for digit in rendered_digits {
            rendered.push(digit);
        }
        rendered
    }

    /// Converts one textual radix digit into its numeric value.
    /// Mutation/allocation: non-mutating; inspects one host-side character and allocates nothing.
    fn digit_value(c: char) -> isize {
        if c.is_ascii_digit() {
            c as isize - '0' as isize
        } else if c.is_ascii_uppercase() {
            10 + c as isize - 'A' as isize
        } else {
            10 + c as isize - 'a' as isize
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::big_num::integer_ref::IntegerRef;
    use crate::data::Heap;

    #[test]
    fn decimal_formatting_handles_zero_sign_and_multi_cell_values() {
        let mut heap = Heap::default();
        let zero = IntegerRef::from_i64(&mut heap, 0);
        let positive = IntegerRef::from_i64(&mut heap, 70_000);
        let negative = IntegerRef::from_i64(&mut heap, -70_000);

        assert_eq!(zero.format_decimal_text(&mut heap), "0");
        assert_eq!(positive.format_decimal_text(&mut heap), "70000");
        assert_eq!(negative.format_decimal_text(&mut heap), "-70000");
    }

    #[test]
    fn hex_formatting_includes_prefix_and_sign() {
        let mut heap = Heap::default();
        let zero = IntegerRef::from_i64(&mut heap, 0);
        let positive = IntegerRef::from_i64(&mut heap, 255);
        let negative = IntegerRef::from_i64(&mut heap, -255);

        assert_eq!(zero.format_hex_text(&mut heap), "0x0");
        assert_eq!(positive.format_hex_text(&mut heap), "0xff");
        assert_eq!(negative.format_hex_text(&mut heap), "-0xff");
    }

    #[test]
    fn octal_formatting_includes_prefix_and_sign() {
        let mut heap = Heap::default();
        let zero = IntegerRef::from_i64(&mut heap, 0);
        let positive = IntegerRef::from_i64(&mut heap, 511);
        let negative = IntegerRef::from_i64(&mut heap, -511);

        assert_eq!(zero.format_octal_text(&mut heap), "0o0");
        assert_eq!(positive.format_octal_text(&mut heap), "0o777");
        assert_eq!(negative.format_octal_text(&mut heap), "-0o777");
    }

    #[test]
    fn non_decimal_formatting_trims_redundant_leading_zeroes() {
        let mut heap = Heap::default();
        let hex_value = IntegerRef::from_i64(&mut heap, 256);
        let octal_value = IntegerRef::from_i64(&mut heap, 64);

        assert_eq!(hex_value.format_hex_text(&mut heap), "0x100");
        assert_eq!(octal_value.format_octal_text(&mut heap), "0o100");
    }

    #[test]
    fn decimal_parsing_handles_sign_and_multi_cell_values() {
        let mut heap = Heap::default();

        assert_eq!(
            IntegerRef::parse_decimal_text(&mut heap, "0").to_i64_lossy(&heap),
            0
        );
        assert_eq!(
            IntegerRef::parse_decimal_text(&mut heap, "70000").to_i64_lossy(&heap),
            70_000
        );
        assert_eq!(
            IntegerRef::parse_decimal_text(&mut heap, "-70000").to_i64_lossy(&heap),
            -70_000
        );
    }

    #[test]
    fn hex_parsing_handles_optional_prefix_and_sign() {
        let mut heap = Heap::default();

        assert_eq!(
            IntegerRef::parse_hex_text(&mut heap, "ff").to_i64_lossy(&heap),
            255
        );
        assert_eq!(
            IntegerRef::parse_hex_text(&mut heap, "0xff").to_i64_lossy(&heap),
            255
        );
        assert_eq!(
            IntegerRef::parse_hex_text(&mut heap, "-0xff").to_i64_lossy(&heap),
            -255
        );
        assert_eq!(
            IntegerRef::parse_hex_text(&mut heap, "00FF").to_i64_lossy(&heap),
            255
        );
    }

    #[test]
    fn octal_parsing_handles_optional_prefix_and_sign() {
        let mut heap = Heap::default();

        assert_eq!(
            IntegerRef::parse_octal_text(&mut heap, "777").to_i64_lossy(&heap),
            511
        );
        assert_eq!(
            IntegerRef::parse_octal_text(&mut heap, "0o777").to_i64_lossy(&heap),
            511
        );
        assert_eq!(
            IntegerRef::parse_octal_text(&mut heap, "-0o777").to_i64_lossy(&heap),
            -511
        );
        assert_eq!(
            IntegerRef::parse_octal_text(&mut heap, "00077").to_i64_lossy(&heap),
            63
        );
    }

    #[test]
    fn formatting_and_parsing_round_trip_for_supported_radices() {
        let mut heap = Heap::default();
        let value = IntegerRef::from_i64(&mut heap, -70_000);

        let decimal = value.format_decimal_text(&mut heap);
        let hex = value.format_hex_text(&mut heap);
        let octal = value.format_octal_text(&mut heap);

        assert_eq!(
            IntegerRef::parse_decimal_text(&mut heap, &decimal).to_i64_lossy(&heap),
            -70_000
        );
        assert_eq!(
            IntegerRef::parse_hex_text(&mut heap, &hex).to_i64_lossy(&heap),
            -70_000
        );
        assert_eq!(
            IntegerRef::parse_octal_text(&mut heap, &octal).to_i64_lossy(&heap),
            -70_000
        );
    }
}
