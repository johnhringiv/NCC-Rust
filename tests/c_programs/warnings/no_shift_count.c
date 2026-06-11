/* In-range constant shift counts and non-constant counts must NOT trigger a shift-count
   warning. Runs cleanly to 0. */
int main(void) {
    int n = 5;
    int a = 1 << 5;     /* in range for int (< 32) */
    int b = 1 << n;     /* non-constant count */
    long c = 1L << 40;  /* in range for long (< 64) — width follows the left operand */
    return 0;
}
