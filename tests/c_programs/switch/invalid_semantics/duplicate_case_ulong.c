/* Regression: duplicate-case detection at 64-bit width. Case labels are compared after
 * conversion to the switch's controlling type. In an `unsigned long` switch, `-1` converts
 * to ULONG_MAX, colliding with the explicit ULONG_MAX case -> must be a semantic error.
 * (The 32-bit half is covered by Sandler's switch_duplicate_cases; this pins the 64-bit half
 * of SwitchIntType::as_i64.)
 */
int main(void) {
    unsigned long x = 0ul;
    switch (x) {
        case -1: return 1;                     /* converts to 18446744073709551615 */
        case 18446744073709551615UL: return 2; /* same value -> duplicate */
        default: return 0;
    }
}