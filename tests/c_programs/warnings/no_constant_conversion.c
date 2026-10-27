/* Explicit casts and in-range / widening conversions must NOT trigger -Wconstant-conversion. */
int a = (int)2147483648;   /* explicit cast — silenced */
long b = 2147483648;       /* fits long (no narrowing) */
int c = 100;               /* fits int */
int main(void) {
    int d = (int)2147483648; /* explicit cast in an automatic local — silenced */
    long e = 2147483648;     /* fits long */
    int f = d;               /* not a constant */
    return 0;
}
