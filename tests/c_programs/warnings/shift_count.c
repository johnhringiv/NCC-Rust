/* -Wshift-count-overflow / -Wshift-count-negative: a constant shift count that is negative
   or >= the width of the left operand's type. NCC masks shift counts at runtime, so these
   compile with warnings and run cleanly to 0; the dedicated test asserts the diagnostics
   via --validate. */
int main(void) {
    int a = 1 << 32;    /* int width 32: count >= width (overflow) */
    int b = 5 >> 32;    /* right shift, same overflow */
    int c = 1 << -1;    /* negative count */
    long d = 1L << 64;  /* long width 64: count >= width */
    int e = 1;
    e <<= 40;           /* compound shift: count >= width (32) */
    return 0;
}
