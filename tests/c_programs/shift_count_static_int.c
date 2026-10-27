/* Shift count is a memory-resident int while the shifted value is a long.
 * Shift operands are deliberately not converted to a common type, so the
 * fixup that moves the count into CX must load it at the count's own width
 * (4 bytes). Loading at the shifted value's width reads past the 4-byte
 * static; the runtime result is still correct (only CL is consumed), so the
 * width is asserted by test_shift_count_load_width in tests/runner.rs. */
static int scount = 3;

int main(void) {
    long lv = 2l << scount;
    return (int)lv;
}