/* Regression: unsigned division uses div (+ zeroed RDX), not idiv (+ cdq); and a constant
 * divisor must be materialized into a register (the Div(Imm) fixup, parallel to Idiv(Imm)).
 * 4000000000u is negative if misread as signed, so signed division would give wrong results.
 */
int main(void) {
    unsigned u = 4000000000u;
    unsigned q = u / 7u; /* 571428571 */
    unsigned r = u % 7u; /* 3 */
    return (q == 571428571u) + (r == 3u); /* 2 */
}