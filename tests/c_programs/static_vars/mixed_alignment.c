/* Regression: file-scope statics of mixed alignment, smaller-aligned before larger-aligned.
 *
 * The emitter tracked each static's .data offset manually (`offset += alignment`) instead of
 * using the offset that `append_section_data` returns. When a 4-byte static (int/unsigned)
 * precedes an 8-byte static (long/unsigned long), the section gets padded to an 8-byte
 * boundary, so the real offset jumps past where the manual counter points — and the later
 * symbol resolved into the padding, loading garbage.
 *
 * Trigger is order-dependent: the smaller-aligned static MUST come first (4 bytes, then pad
 * to 8). `big`/`big_u` use values that don't fit in 32 bits, so a wrong (truncated/garbage)
 * load can't coincidentally compare equal. Returns 4 when every static loads correctly.
 */
int small = 5;
long big = 6442450941L; /* 0x1_8000_003D — bit 33 set, so truncation/garbage would differ */
unsigned int small_u = 7u;
unsigned long big_u = 12884901890ul; /* 0x3_0000_0002 — bits above 32 set */

int main(void) {
    return (small == 5) + (big == 6442450941L) + (small_u == 7u) + (big_u == 12884901890ul);
}
