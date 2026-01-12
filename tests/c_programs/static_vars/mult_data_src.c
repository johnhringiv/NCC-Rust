// Test coverage for Mult with Data source operand
// Multiplication where one operand is a static variable

int g_multiplier = 7;
int g_factor = -3;

int main(void) {
    int local = 6;
    int result;

    // Mult with static var as source (Data -> Reg)
    result = local * g_multiplier;  // 6 * 7 = 42
    if (result != 42) return 1;

    // Mult with negative static multiplier
    result = local * g_factor;  // 6 * -3 = -18
    if (result != -18) return 2;

    // Chain multiplications
    result = local * g_multiplier * 2;  // 6 * 7 * 2 = 84
    if (result != 84) return 3;

    // Multiply two values where one comes from static
    local = 10;
    result = g_multiplier * local;  // 7 * 10 = 70
    if (result != 70) return 4;

    return 0;
}
