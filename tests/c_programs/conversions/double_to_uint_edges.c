/* NCC-specific deterministic edge: converting an out-of-range or NaN `double` to `unsigned int`
   is undefined behavior in standard C. NCC defines it as the unsigned "integer indefinite" =
   the target type's minimum, i.e. 0 — matching the README's float-cast contract. Companion to
   double_to_int_edges.c (signed targets); this locks in the u32 path, both the runtime lowering
   and const-eval (static initializers), which must agree.

   Checks use discriminating comparisons rather than bare casts so a wrong nonzero result (e.g.
   the old low-32-bits behavior: (unsigned int)1e12 -> 3567587328) can't alias the expected 0 in
   the 8-bit exit code. Returns the number of checks that held; expected result is 11. */

static unsigned int s_big = (unsigned int) 1e12;          /* out of range -> 0 */
static unsigned int s_neg = (unsigned int) -1e30;         /* out of range -> 0 */
static unsigned int s_max = (unsigned int) 4294967295.0;  /* UINT_MAX, exactly representable */

int main(void) {
    int passed = 0;

    double big = 1e12;         /* above u32 range, within i64 range (old code kept low 32 bits) */
    double huge = 1e30;        /* above i64 range too (indefinite path) */
    double neg = -1e30;        /* far below */
    double inf = 2e308;        /* rounds to +infinity (also fires -Woverflow at compile time) */
    double nan = 0.0 / 0.0;    /* NaN */
    double two32 = 4294967296.0; /* 2^32: first value out of range */

    /* invalid conversions: all must yield 0 */
    if ((unsigned int) big == 0u) passed = passed + 1;
    if ((unsigned int) huge == 0u) passed = passed + 1;
    if ((unsigned int) neg == 0u) passed = passed + 1;
    if ((unsigned int) inf == 0u) passed = passed + 1;
    if ((unsigned int) nan == 0u) passed = passed + 1;
    if ((unsigned int) two32 == 0u) passed = passed + 1;

    /* valid conversions: truncate toward zero */
    if ((unsigned int) 4294967295.0 == 4294967295u) passed = passed + 1; /* last in-range value */
    if ((unsigned int) -0.75 == 0u) passed = passed + 1;                 /* truncates to 0: valid */

    /* static (const-eval) results must match the runtime rules above */
    if (s_big == 0u) passed = passed + 1;
    if (s_neg == 0u) passed = passed + 1;
    if (s_max == 4294967295u) passed = passed + 1;

    return passed; /* expect 11 */
}