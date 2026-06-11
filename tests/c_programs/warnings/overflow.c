/* -Woverflow: constant folds that leave the result type, in static initializers (folded at
   compile time, so the program runs cleanly to 0).

   Regression: `b`, `c`, and `d` used to PANIC the compiler — `-INT_MIN`, `INT_MIN / -1`, and
   `INT_MIN % -1` overflow Rust's checked arithmetic. They now fold with deterministic two's-
   complement wrapping and warn instead of crashing. */
int a = 2147483647 + 1;        /* INT_MAX + 1 */
int b = -(int)2147483648;      /* -INT_MIN (previously: negate-overflow panic) */
int c = (int)2147483648 / -1;  /* INT_MIN / -1 (previously: div-overflow panic) */
int d = (int)2147483648 % -1;  /* INT_MIN % -1 (previously: rem-overflow panic) */

int main(void) { return 0; }
