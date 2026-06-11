/* In-range constant folds must NOT trigger -Woverflow. Runs cleanly to 0. */
int a = 2147483647;     /* INT_MAX itself — fits */
int b = 1000 * 1000;    /* 1,000,000 — fits int */
int c = 5 + 3;
int main(void) { return 0; }
