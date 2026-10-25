/* Negating a double uses `xorps` with a sign-bit mask from .rodata, and `xorps` reads 16 bytes.
   The mask must be stored as a full 16-byte entry so the read never runs past .rodata.
   test_double_negation_mask_size in tests/runner.rs checks the entry size; this file also runs.
   Returns 5. */
double neg(double x) {
    return -x;
}

int main(void) {
    double d = neg(2.5) + -2.5; /* -5.0 */
    return (int) -d;
}
