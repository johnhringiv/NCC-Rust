// Test coverage for Data operand patterns in emit_iced.rs
// Exercises direct operations on static/file-scope variables

int g_add = 10;
int g_sub = 20;
int g_and = 255;
int g_or = 15;
int g_xor = 170;
int g_shl = 1;
int g_sar = -16;
int g_neg = 5;
int g_not = 0;
int g_cmp = 42;
int g_setcc = 99;
int g_idiv_dividend = 100;
int g_idiv_divisor = 7;
int g_mov_imm = 0;

int main(void) {
    int result = 0;
    int local_val = 3;

    // Test 1: Mov Imm -> Data (direct constant store to static var)
    g_mov_imm = 123;
    if (g_mov_imm != 123) return 1;

    // Test 2: Binary Add with Reg -> Data and Imm -> Data
    g_add = g_add + local_val;  // generates load, add reg, store
    g_add = g_add + 5;          // Imm -> Data for add
    if (g_add != 18) return 2;

    // Test 3: Binary Sub with Reg -> Data and Imm -> Data
    g_sub = g_sub - local_val;  // Reg -> Data
    g_sub = g_sub - 2;          // Imm -> Data
    if (g_sub != 15) return 3;

    // Test 4: Binary BitAnd with Reg -> Data and Imm -> Data
    g_and = g_and & local_val;  // local_val = 3, 255 & 3 = 3
    g_and = g_and & 1;          // Imm -> Data: 3 & 1 = 1
    if (g_and != 1) return 4;

    // Test 5: Binary BitOr with Reg -> Data and Imm -> Data
    g_or = g_or | local_val;    // 15 | 3 = 15
    g_or = g_or | 240;          // Imm -> Data: 15 | 240 = 255
    if (g_or != 255) return 5;

    // Test 6: Binary BitXor with Reg -> Data and Imm -> Data
    g_xor = g_xor ^ local_val;  // 170 ^ 3 = 169
    g_xor = g_xor ^ 9;          // Imm -> Data: 169 ^ 9 = 160
    if (g_xor != 160) return 6;

    // Test 7: Shift left with Imm -> Data
    g_shl = g_shl << 4;         // 1 << 4 = 16
    if (g_shl != 16) return 7;

    // Test 8: Arithmetic shift right with Imm -> Data
    g_sar = g_sar >> 2;         // -16 >> 2 = -4 (arithmetic shift preserves sign)
    if (g_sar != -4) return 8;

    // Test 9: Unary Neg on Data
    g_neg = -g_neg;             // -5
    if (g_neg != -5) return 9;

    // Test 10: Unary Not on Data
    g_not = ~g_not;             // ~0 = -1
    if (g_not != -1) return 10;

    // Test 11: Cmp with Data operands and various combinations
    // Cmp(Imm, Data) - comparing immediate with static var
    if (g_cmp != 42) return 11;

    // Test 12: Idiv with Data operand (divisor is static var)
    result = g_idiv_dividend / g_idiv_divisor;  // 100 / 7 = 14
    if (result != 14) return 12;

    // All tests passed
    return 0;
}
