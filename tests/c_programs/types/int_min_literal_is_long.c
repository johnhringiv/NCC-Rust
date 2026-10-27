/* `2147483648` doesn't fit int, so C types it `long`, and `-2147483648` is a `long` too (the
   minus is an operator, not part of the literal). Arithmetic on it is done in 64 bits.
   Returns the number of checks that held; expected result is 3. */
int main(void) {
    int passed = 0;
    int c = 1;

    /* long arithmetic: no 32-bit wrap */
    long x = -2147483648 - 1;
    if (x < 0) passed = passed + 1;

    /* common type of long and unsigned int is long, so the result stays negative */
    long v = c ? -2147483648 : 4294967295u;
    if (v < 0) passed = passed + 1;

    /* as a long, negating it again gives +2^31; as an int it would wrap back to INT_MIN */
    if (-2147483648 / -1 > 0) passed = passed + 1;

    return passed; /* expect 3 */
}
