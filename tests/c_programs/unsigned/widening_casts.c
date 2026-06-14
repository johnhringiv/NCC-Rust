/* Regression: widening casts must extend by the SOURCE's signedness, not the destination's.
 * emit_cast keyed on the wrong operand, so mixed-sign widenings were wrong:
 *   - uint -> long  must ZERO-extend (value preserved): (long)UINT_MAX == 4294967295
 *   - int  -> ulong must SIGN-extend:                    (unsigned long)(-1) == ULONG_MAX
 * Locals (not constants) so the conversion runs in codegen, not constant folding.
 */
int main(void) {
    unsigned int a = 4294967295u; /* UINT_MAX */
    int b = -1;
    long la = (long)a;                  /* zero-extend */
    unsigned long lb = (unsigned long)b; /* sign-extend */
    return (la == 4294967295L) + (lb == 18446744073709551615UL); /* 2 when both correct */
}