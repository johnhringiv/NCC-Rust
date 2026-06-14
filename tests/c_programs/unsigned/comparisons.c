/* Regression: unsigned comparisons must use unsigned condition codes (setb/seta/jb/ja),
 * not signed (setl/setg). Codegen took signedness from the comparison's result (always int)
 * instead of its operands. 4294967294u reads as -2 if compared as signed.
 * Locals so the compare runs in codegen, not constant folding.
 */
int main(void) {
    unsigned u = 4294967294u; /* -2 if misread as signed */
    unsigned hundred = 100u;
    /* All true for unsigned; all false if compared as signed -> would return 0 */
    return (u > hundred) + (u >= hundred) + (hundred < u) + (hundred <= u); /* 4 */
}