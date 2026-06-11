/* -Wdiv-by-zero: a constant zero divisor warns at compile time.
   The divisions sit in a never-taken branch (NCC has no constant propagation, so `n`
   is a runtime value) — so the program compiles with warnings but runs cleanly to 0.
   The dedicated test asserts the diagnostics via --validate. */
int main(void) {
    int n = 0;
    if (n) {
        int a = 5 / 0;   /* division by zero */
        int b = 7 % 0;   /* remainder by zero */
        a /= 0;          /* division by zero (compound assignment) */
        return a + b;
    }
    return 0;
}
