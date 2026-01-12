// Test coverage for Idiv with Data operand
// Division and modulo where divisor is a static variable

int g_divisor = 7;
int g_neg_divisor = -5;

int main(void) {
    int dividend = 100;
    int result;

    // Idiv with positive static divisor
    result = dividend / g_divisor;  // 100 / 7 = 14
    if (result != 14) return 1;

    // Modulo with positive static divisor
    result = dividend % g_divisor;  // 100 % 7 = 2
    if (result != 2) return 2;

    // Idiv with negative static divisor
    result = dividend / g_neg_divisor;  // 100 / -5 = -20
    if (result != -20) return 3;

    // Modulo with negative static divisor
    result = dividend % g_neg_divisor;  // 100 % -5 = 0
    if (result != 0) return 4;

    // Negative dividend with static divisor
    dividend = -100;
    result = dividend / g_divisor;  // -100 / 7 = -14 (truncated toward zero)
    if (result != -14) return 5;

    result = dividend % g_divisor;  // -100 % 7 = -2
    if (result != -2) return 6;

    return 0;
}
