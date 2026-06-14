/* Regression: right shift selects shr (logical) for unsigned, sar (arithmetic) for signed,
 * and a VARIABLE count must be routed through CL. The shift-count fixup arms originally
 * omitted BitShr, so a variable-count unsigned right shift hit unreachable code.
 * Uses a variable shift count to exercise the CL fixup.
 */
int main(void) {
    unsigned u = 4294967294u; /* 0xFFFFFFFE */
    unsigned count = 1u;
    int s = -2;
    int u_ok = (u >> count) == 2147483647u; /* logical: 0x7FFFFFFF */
    int s_ok = (s >> 1) == -1;              /* arithmetic: sign-preserving */
    return u_ok + s_ok;                     /* 2 */
}