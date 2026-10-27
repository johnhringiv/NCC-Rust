/* In-range constant folds must NOT trigger -Woverflow. Runs cleanly to 0. */
int a = 2147483647;     /* INT_MAX itself — fits */
int b = 1000 * 1000;    /* 1,000,000 — fits int */
int c = 5 + 3;
int main(void) { return 0; }
/* Overflow in an operand that is never evaluated doesn't warn (matches gcc). */
int d = 1 || 2147483647 + 1;
int e = 0 && 2147483647 + 1;
int f = 1 ? 0 : 2147483647 + 1;
