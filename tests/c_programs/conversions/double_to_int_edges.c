/* NCC-specific deterministic edge: converting an out-of-range, +infinity, or NaN `double` to a
   signed integer is undefined behavior in standard C (so the book's suite can't test it). NCC
   defines it as the x86 `cvttsd2si` "integer indefinite" = the target type's minimum value.

   Checks use discriminating comparisons (not bare casts returned directly) because the process
   exit code is only 8 bits — INT_MIN/LONG_MIN have a low byte of 0, which would otherwise be
   indistinguishable from a (wrong) result of 0. Returns the number of checks that held; all
   conversions below must yield the type minimum, so the expected result is 7. */

int int_min(void) {
    return -2147483647 - 1; // INT_MIN, built without an out-of-range literal
}

long long_min(void) {
    return -9223372036854775807L - 1L; // LONG_MIN
}

int main(void) {
    int passed = 0;

    double big = 1e30;       // far above INT/LONG range
    double neg_big = -1e30;  // far below
    double inf = 2e308;      // rounds to +infinity (also fires -Woverflow at compile time)
    double nan = 0.0 / 0.0;  // NaN

    // double -> int : every out-of-range / inf / NaN value is the "indefinite" INT_MIN
    if ((int) big == int_min()) passed = passed + 1;
    if ((int) neg_big == int_min()) passed = passed + 1;
    if ((int) inf == int_min()) passed = passed + 1;
    if ((int) nan == int_min()) passed = passed + 1;

    // double -> long : likewise LONG_MIN
    if ((long) big == long_min()) passed = passed + 1;
    if ((long) inf == long_min()) passed = passed + 1;
    if ((long) nan == long_min()) passed = passed + 1;

    return passed; // expect 7
}
