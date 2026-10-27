/* Hex integer literals (0x/0X prefix, either letter case). Like octal, unsuffixed hex literals
   take unsigned types when the value only fits there (int, unsigned int, long, unsigned long).
   Returns the number of checks that held; expected result is 10. */
int main(void) {
    int passed = 0;

    if (0x10 == 16 && 0XfF == 255 && 0xABCDEF == 11259375) passed = passed + 1;
    if (0x10u == 16u && 0x10L == 16l && 0x10ul == 16ul && 0X10LU == 16ul) passed = passed + 1;

    /* digits that look like an exponent or octal-invalid digits are plain hex digits */
    if (0x1e5 == 485 && 0x89 == 137) passed = passed + 1;
    /* a hex digit run ends the literal: 1 - 0x1 is a subtraction */
    if (1 - 0x1 == 0) passed = passed + 1;

    /* 0x80000000 fits unsigned int but not int: it is unsigned, so negating it stays positive */
    if (-0x80000000 > 0) passed = passed + 1;
    /* -(2^32 - 1) wraps as unsigned int to 1 */
    if (-0xFFFFFFFF == 1) passed = passed + 1;
    /* the decimal spelling of 2^31 is a long instead, so it stays negative */
    if (-2147483648 < 0) passed = passed + 1;

    /* 0x100000000 needs 64 bits: long, so it doesn't truncate */
    if (0x100000000 / 2 == 2147483648l) passed = passed + 1;

    /* 2^64 - 1 fits unsigned long only */
    if (0xFFFFFFFFFFFFFFFF == 18446744073709551615ul) passed = passed + 1;
    /* hex-suffixed long that doesn't fit long becomes unsigned long */
    if (0x8000000000000000l > 0) passed = passed + 1;

    return passed; /* expect 10 */
}
