// Test shift with immediate > 255
// This tests the fix_invalid case where shift count > 255
// must be loaded into CX register
//
// Note: Shift counts are masked in x86-64:
//   - For long: count & 63
//   - For int: count & 31
// So 256 & 63 = 0, meaning shift by 0

int main(void) {
    long x = 42L;

    // Shift by 256 - should be masked to 0 for long (256 & 63 = 0)
    long result1 = x << 256;
    if (result1 != 42L) return 1;

    // Shift by 257 - should be masked to 1 for long (257 & 63 = 1)
    long result2 = 10L << 257;
    if (result2 != 20L) return 2;

    // Negative shift count (also triggers the fix)
    // -1 as unsigned is all 1s, masked to 63 for long
    long result3 = 1L << -1;  // -1 & 63 = 63
    if (result3 != (1L << 63)) return 3;

    return 0;
}
