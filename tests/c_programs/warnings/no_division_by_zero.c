/* Divisors that are non-constant, or non-zero constants, must NOT trigger
   -Wdiv-by-zero. Runs cleanly to 0. */
int main(void) {
    int d = 5;
    int x = 10 / d;    /* non-constant divisor — no warning */
    int y = 8 / 4;     /* non-zero constant divisor — no warning */
    return x - y;      /* 2 - 2 = 0 */
}
