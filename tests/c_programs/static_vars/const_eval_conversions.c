/* Constant folding must convert operands the same way runtime code does.
 * Each static is checked against the same expression computed at runtime. */

/* ?: converts the taken branch to the common type of both branches (unsigned int here). */
static int ternary_cmp = (1 ? -1 : 0u) < 0;          /* -1 becomes 4294967295u: 0 */
static long ternary_div = (1 ? -6 : 0u) / 2;         /* 4294967290u / 2 = 2147483645 */
static double ternary_double = 1 ? 3 : 0.5;          /* 3.0 */

/* The divisor is converted to double before the zero check: 1.0 / 0.0 is +inf. */
static double inf = 1.0 / 0;

/* && and || don't evaluate the right side once the left side decides the result,
 * and neither does the untaken branch of ?:. */
static int or_skips = 1 || 1 / 0;
static int and_skips = 0 && 1 / 0;
static int ternary_skips = 1 ? 5 : 1 / 0;

int main(void) {
    int one = 1;
    unsigned int zero_u = 0u;
    int zero = 0;
    int checks = 0;

    if (ternary_cmp == ((one ? -1 : zero_u) < 0)) checks = checks + 1;
    if (ternary_div == (one ? -6 : zero_u) / 2) checks = checks + 1;
    if (ternary_div == 2147483645) checks = checks + 1;
    if (ternary_double == (one ? 3 : 0.5)) checks = checks + 1;
    if (inf == 1.0 / zero) checks = checks + 1;
    if (inf > 1e308) checks = checks + 1;
    if (or_skips == 1 && and_skips == 0 && ternary_skips == 5) checks = checks + 1;

    /* Case labels use the same folding: this one is 2147483645, not -3. */
    switch (2147483645) {
    case (1 ? -6 : 0u) / 2:
        checks = checks + 1;
        break;
    default:
        break;
    }
    return checks;
}
