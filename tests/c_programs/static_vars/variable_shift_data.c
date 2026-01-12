// Test coverage for variable shift amounts on static variables
// This exercises BitShl/BitSar with CX -> Data pattern

int g_shl_var = 1;
int g_sar_var = -64;

int main(void) {
    int shift_amount = 3;

    // Variable shift left on static var (CX -> Data)
    g_shl_var = g_shl_var << shift_amount;  // 1 << 3 = 8
    if (g_shl_var != 8) return 1;

    // Variable shift right on static var (CX -> Data)
    g_sar_var = g_sar_var >> shift_amount;  // -64 >> 3 = -8
    if (g_sar_var != -8) return 2;

    // Chain variable shifts
    shift_amount = 2;
    g_shl_var = g_shl_var << shift_amount;  // 8 << 2 = 32
    if (g_shl_var != 32) return 3;

    return 0;
}
