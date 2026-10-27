/* -Wconstant-conversion on case labels: each case value converts to the switch's type, and a
   narrowing that changes the value warns (gcc reports these as -Woverflow). Sign-only changes
   and widenings don't warn. Expect exactly 3 warnings, on lines 9, 11, and 17. Runs to 0. */
int pick(int i, unsigned int u, long l) {
    int r = 0;
    switch (i) {
    case 1:
        r = 1;
    case 4294967298l:             /* long -> int: 4294967298 becomes 2 (warns) */
        r = 2;
    case 4294967299ul:            /* unsigned long -> int: becomes 3 (warns) */
        r = 3;
    case 4294967295u:             /* unsigned int -> int: sign change only, -1 (no warning) */
        r = 4;
    }
    switch (u) {
    case 8589934590l:             /* long -> unsigned int: becomes 4294967294 (warns) */
        r = 5;
    case -3:                      /* int -> unsigned int: sign change only (no warning) */
        r = 6;
    }
    switch (l) {
    case -1:                      /* int -> long: widening (no warning) */
        r = 7;
    case 4294967295u:             /* unsigned int -> long: widening (no warning) */
        r = 8;
    }
    return r;
}

int main(void) {
    return pick(0, 0u, 0l);
}
