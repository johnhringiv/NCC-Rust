/* Integer literals with a leading 0 are octal. Unsuffixed octal literals also take unsigned
   types when the value only fits there (int, unsigned int, long, unsigned long), unlike decimal
   literals (int, long). Floating literals are always decimal, even with leading zeros.
   Returns the number of checks that held; expected result is 10. */
int main(void) {
    int passed = 0;

    if (010 == 8) passed = passed + 1;
    if (0777 == 511) passed = passed + 1;
    if (00 == 0) passed = passed + 1;
    if (017l == 15l && 017u == 15u && 017ul == 15ul) passed = passed + 1;

    /* 037777777777 = 2^32 - 1 fits unsigned int but not int: it is unsigned, so > 0 ... */
    if (037777777777 > 0) passed = passed + 1;
    /* ... and negating it wraps as unsigned int: -(2^32 - 1) = 1 */
    if (-037777777777 == 1) passed = passed + 1;
    /* the decimal spelling is a long instead, so it stays negative */
    if (-4294967295 < 0) passed = passed + 1;

    /* 2^63 fits unsigned long only (as an octal literal it has a type; in decimal it wouldn't) */
    if (01000000000000000000000 == 9223372036854775808ul) passed = passed + 1;

    /* octal-suffixed long that doesn't fit long becomes unsigned long */
    if (0777777777777777777777l > 0) passed = passed + 1;

    /* floating literals with leading zeros are decimal; 8 and 9 are fine there */
    if (08.5 == 8.5 && 09e1 == 90.0) passed = passed + 1;

    return passed; /* expect 10 */
}
