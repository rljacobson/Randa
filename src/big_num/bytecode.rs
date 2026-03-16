//! Bytecode codec support for Miranda integers.

use super::integer_ref::IntegerRef;
use crate::data::{api::HeapObjectProxy, Heap};

impl IntegerRef {
    /// Decodes Miranda's compact `SHORT_X` integer payload into a trusted proxy.
    ///
    /// This is the typed bigint-owned entry point for short integer bytecode admission.
    /// Mutation/allocation: allocates a fresh normalized heap integer; does not mutate any existing integer cells.
    pub(crate) fn decode_short_bytecode(heap: &mut Heap, encoded: u8) -> Self {
        Self::from_i64(heap, i8::from_ne_bytes([encoded]) as i64)
    }

    /// Decodes Miranda's `INT_X` word stream into a trusted proxy.
    ///
    /// This is the typed bigint-owned entry point for multi-word integer bytecode admission.
    /// The input may include the trailing `-1` sentinel; if present, it is consumed.
    /// The first payload word is allowed to be `-1`, matching the active loader's
    /// compatibility note.
    /// Mutation/allocation: allocates a fresh heap integer chain matching the serialized payload; does not mutate any pre-existing integer cells.
    pub(crate) fn decode_int_x_bytecode(heap: &mut Heap, words: &[isize]) -> Self {
        let payload_len = if words.len() > 1 && words.last() == Some(&-1) {
            words.len() - 1
        } else {
            words.len()
        };

        assert!(payload_len > 0, "INT_X requires at least one payload word");

        let root = IntegerRef::from_stored_cell(heap, words[0], None);
        let mut cursor = root.get_ref();

        for &word in &words[1..payload_len] {
            let next = IntegerRef::from_stored_cell(heap, word, None);
            heap[cursor].tail = next.get_ref();
            cursor = next.get_ref();
        }

        root
    }

    /// Encodes this trusted integer for Miranda dump emission.
    ///
    /// This is the typed bigint-owned bytecode egress path for `SHORT_X`/`INT_X` payloads.
    /// Mutation/allocation: non-mutating with respect to the heap integer; allocates only the returned host-side word vector.
    pub(crate) fn encode_for_dump_bytecode(&self, heap: &Heap) -> Vec<isize> {
        if self.is_single_cell(heap) {
            let value = self.decode_single_cell_value(heap);
            if (-127..=127).contains(&value) {
                return vec![value];
            }
        }

        let mut words = vec![self.raw_head_word(heap)];
        let mut current = self.next_digit_cell(heap);

        while let Some(cell) = current {
            words.push(cell.raw_head_word(heap));
            current = cell.next_digit_cell(heap);
        }

        words.push(-1);
        words
    }
}

#[cfg(test)]
mod tests {
    use crate::big_num::{constants::SIGN_BIT_MASK, integer_ref::IntegerRef};
    use crate::data::{Heap, Tag, Value};

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
    fn short_bytecode_decode_sign_extends_into_single_cell_integer() {
        let mut heap = Heap::default();

        assert_eq!(
            IntegerRef::decode_short_bytecode(&mut heap, 0x7f).to_i64_lossy(&heap),
            127
        );
        assert_eq!(
            IntegerRef::decode_short_bytecode(&mut heap, 0xff).to_i64_lossy(&heap),
            -1
        );
        assert_eq!(
            IntegerRef::decode_short_bytecode(&mut heap, 0x80).to_i64_lossy(&heap),
            -128
        );
    }

    #[test]
    fn int_x_decode_reconstructs_multi_cell_chain_from_payload_and_sentinel() {
        let mut heap = Heap::default();
        let integer = IntegerRef::decode_int_x_bytecode(&mut heap, &[1, 2, 3, -1]);

        assert_eq!(integer.low_digit(&heap), 1);
        assert_eq!(integer.next_digit_cell(&heap).unwrap().digit_word(&heap), 2);
        assert_eq!(
            integer
                .next_digit_cell(&heap)
                .unwrap()
                .next_digit_cell(&heap)
                .unwrap()
                .digit_word(&heap),
            3
        );
    }

    #[test]
    fn int_x_decode_allows_first_payload_word_to_be_negative_one() {
        let mut heap = Heap::default();
        let integer = IntegerRef::decode_int_x_bytecode(&mut heap, &[-1, -1]);

        assert!(integer.is_single_cell(&heap));
        assert_eq!(integer.raw_head_word(&heap), -1);
        assert_eq!(integer.encode_for_dump_bytecode(&heap), vec![-1, -1]);
    }

    #[test]
    fn int_x_decode_accepts_payload_without_trailing_sentinel() {
        let mut heap = Heap::default();
        let integer = IntegerRef::decode_int_x_bytecode(&mut heap, &[1, 2, 3]);

        assert_eq!(integer.encode_for_dump_bytecode(&heap), vec![1, 2, 3, -1]);
    }

    #[test]
    fn dump_encoding_uses_short_payload_for_small_single_cell_values() {
        let mut heap = Heap::default();
        let positive = IntegerRef::from_i64(&mut heap, 42);
        let negative = IntegerRef::from_i64(&mut heap, -42);

        assert_eq!(positive.encode_for_dump_bytecode(&heap), vec![42]);
        assert_eq!(negative.encode_for_dump_bytecode(&heap), vec![-42]);
    }

    #[test]
    fn dump_encoding_keeps_documented_short_range_boundary() {
        let mut heap = Heap::default();
        let max_short = IntegerRef::from_i64(&mut heap, 127);
        let min_documented_short = IntegerRef::from_i64(&mut heap, -127);
        let beyond_documented_short = IntegerRef::from_i64(&mut heap, -128);

        assert_eq!(max_short.encode_for_dump_bytecode(&heap), vec![127]);
        assert_eq!(
            min_documented_short.encode_for_dump_bytecode(&heap),
            vec![-127]
        );
        assert_eq!(
            beyond_documented_short.encode_for_dump_bytecode(&heap),
            vec![SIGN_BIT_MASK | 128, -1]
        );
    }

    #[test]
    fn dump_encoding_uses_int_x_word_stream_for_non_short_or_multi_cell_values() {
        let mut heap = Heap::default();
        let multi = IntegerRef::decode_int_x_bytecode(&mut heap, &[1, 2, 3, -1]);
        let large_single = IntegerRef::from_i64(&mut heap, 200);

        assert_eq!(multi.encode_for_dump_bytecode(&heap), vec![1, 2, 3, -1]);
        assert_eq!(large_single.encode_for_dump_bytecode(&heap), vec![200, -1]);
    }

    #[test]
    fn dump_encoding_preserves_signed_first_word_for_negative_heap_values() {
        let mut heap = Heap::default();
        let negative = int_ref(&mut heap, SIGN_BIT_MASK | 5, 0);

        assert_eq!(negative.encode_for_dump_bytecode(&heap), vec![-5]);
    }
}
