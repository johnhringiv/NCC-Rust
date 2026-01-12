// Test coverage for Cmp instruction with Data operands
// Tests various comparison patterns involving static variables

int g_val = 50;
int g_other = 30;

int main(void) {
    int local = 40;

    // Cmp(Reg, Data) - compare register with static var
    // Generated when we compare local var with static
    if (local > g_val) return 1;  // 40 > 50 is false, shouldn't return
    if (local < g_val) {
        // 40 < 50 is true
    } else {
        return 2;
    }

    // Cmp(Data, Reg) - compare static var with register
    // Order matters in codegen
    if (g_val > local) {
        // 50 > 40 is true
    } else {
        return 3;
    }

    // Cmp(Imm, Data) - compare immediate with static var
    // This tests the pattern at emit_iced.rs:691-694
    if (g_val == 50) {
        // true
    } else {
        return 4;
    }

    if (g_val != 100) {
        // true, 50 != 100
    } else {
        return 5;
    }

    // Cmp two static vars (both memory - gets rewritten but still uses Data)
    if (g_val > g_other) {
        // 50 > 30 is true
    } else {
        return 6;
    }

    // Edge cases
    if (g_val >= 50) {
        // true
    } else {
        return 7;
    }

    if (g_val <= 50) {
        // true
    } else {
        return 8;
    }

    return 0;
}
