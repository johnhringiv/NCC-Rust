/* NCC-specific deterministic edge: INT_MIN / -1 (and % -1) overflows the quotient, and x86
   `idiv` raises #DE (SIGFPE) instead of wrapping — the only integer instruction that refuses
   to. NCC's README guarantees signed overflow wraps, so codegen special-cases divisor -1:
   quotient = neg(dividend) (wraps MIN -> MIN), remainder = 0. Const-eval already wrapped via
   `overflowing_div`/`rem`; these checks lock runtime and const-eval to the same values.

   Covers both codegen paths: a runtime-variable divisor (cmp/branch) and a constant -1 divisor
   (decided at compile time, no branch). Non-overflowing divisions guard against the wrap path
   or branch polarity being wrong for ordinary values. Returns checks passed; expect 13. */

int int_min(void) {
    return -2147483647 - 1; // INT_MIN, built without an out-of-range literal
}

long long_min(void) {
    return -9223372036854775807L - 1L; // LONG_MIN
}

static int s_div = (-2147483647 - 1) / -1;             /* const-eval wraps to INT_MIN (-Woverflow) */
static int s_rem = (-2147483647 - 1) % -1;             /* 0 */
static long s_ldiv = (-9223372036854775807L - 1L) / -1; /* LONG_MIN */

int main(void) {
    int passed = 0;

    int m = int_min();
    long lm = long_min();
    int neg1 = -1;    /* runtime-variable divisors: force the cmp/branch path */
    long lneg1 = -1;

    /* overflow, runtime divisor: must wrap, not SIGFPE */
    if (m / neg1 == int_min()) passed = passed + 1;
    if (m % neg1 == 0) passed = passed + 1;
    if (lm / lneg1 == long_min()) passed = passed + 1;
    if (lm % lneg1 == 0) passed = passed + 1;

    /* overflow, constant divisor: compile-time-decided path */
    if (m / -1 == int_min()) passed = passed + 1;
    if (m % -1 == 0) passed = passed + 1;
    if (lm / -1 == long_min()) passed = passed + 1;

    /* no overflow, divisor -1: wrap path must still compute correct values */
    if (7 / neg1 == -7) passed = passed + 1;
    if (7 % neg1 == 0) passed = passed + 1;

    /* no overflow, divisor != -1: branch must fall through to plain idiv */
    if (10 / -3 == -3) passed = passed + 1;
    if (10 % -3 == 1) passed = passed + 1;

    /* const-eval results must match the runtime rules above */
    if (s_div == int_min()) passed = passed + 1;
    if (s_rem == 0 && s_ldiv == long_min()) passed = passed + 1;

    return passed; /* expect 13 */
}
