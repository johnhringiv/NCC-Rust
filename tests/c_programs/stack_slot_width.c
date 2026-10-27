/* A stack slot must be sized by its variable's type, not by the width of the first instruction
   that touches it. In both cases below, a loop puts a narrower read of a `long` earlier in the
   code than its first write:
   - `(int)x` reads only the low 4 bytes of `x`;
   - `1 << c` with `long c` reads the count at the shifted value's (int) width.
   With 4-byte slots, the later 8-byte writes clobbered the neighbouring slot.
   Returns the number of checks that held; expected result is 2. */
int truncate_first(void) {
    long x;
    int keep;
    int r;
    for (int i = 0; i < 2; i = i + 1) {
        if (i == 1) {
            keep = 7;
            r = (int) x;
            x = 1311768464867721217l; /* high half nonzero: clobbers `keep` if x's slot is 4 bytes */
            return keep + r;          /* 7 + 3 */
        }
        x = 3;
    }
    return 0;
}

int shift_count_first(void) {
    long c;
    int r;
    for (int i = 0; i < 2; i = i + 1) {
        if (i == 1) {
            r = 1 << c;                 /* writes r's temp, which sat in c's upper half */
            return (int) (c >> 32);     /* 5 */
        }
        c = 21474836483l; /* 5 * 2^32 + 3 */
    }
    return 0;
}

int main(void) {
    int passed = 0;
    if (truncate_first() == 10) passed = passed + 1;
    if (shift_count_first() == 5) passed = passed + 1;
    return passed; /* expect 2 */
}
