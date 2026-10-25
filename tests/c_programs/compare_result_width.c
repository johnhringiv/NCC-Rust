/* A comparison's result is always int, even when its operands are 8 bytes wide.
 * Codegen zeroes the result before `setCC`; that zeroing must use the result's
 * width (movl), not the operands' (movq). A stack slot's size is set by the first
 * instruction that touches it, so the wide zeroing gave every such result an
 * 8-byte slot. That slot was self-consistent (nothing clobbered), so exit codes
 * can't catch it — test_compare_result_width in tests/runner.rs checks the asm.
 * Covers every operand shape that reaches the zeroing: long, unsigned long,
 * double (ordered and ==/!= with the NaN fixup), and `!` on long. */
int main(void) {
    long a = 5;
    long b = 7;
    unsigned long ua = 1ul;
    unsigned long ub = 2ul;
    double d = 1.5;
    double e = 2.5;

    int r1 = a < b;     /* 1 */
    int r2 = ua >= ub;  /* 0 */
    int r3 = d < e;     /* 1 */
    int r4 = d == d;    /* 1 */
    int r5 = d != e;    /* 1 */
    int r6 = !a;        /* 0 */
    int r7 = !(a - 5);  /* 1 */

    return r1 + 2 * r2 + 4 * r3 + 8 * r4 + 16 * r5 + 32 * r6 + 64 * r7; /* 93 */
}
