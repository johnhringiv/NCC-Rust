// Test coverage for SetCC with Data operand
// Stores comparison results directly to static variables

int g_eq = 99;
int g_ne = 99;
int g_lt = 99;
int g_le = 99;
int g_gt = 99;
int g_ge = 99;

int main(void) {
    int a = 5;
    int b = 10;

    // SetCC E (equal) to Data
    g_eq = (a == a);
    if (g_eq != 1) return 1;

    // SetCC NE (not equal) to Data
    g_ne = (a != b);
    if (g_ne != 1) return 2;

    // SetCC L (less than) to Data
    g_lt = (a < b);
    if (g_lt != 1) return 3;

    // SetCC LE (less or equal) to Data
    g_le = (a <= b);
    if (g_le != 1) return 4;

    // SetCC G (greater than) to Data
    g_gt = (b > a);
    if (g_gt != 1) return 5;

    // SetCC GE (greater or equal) to Data
    g_ge = (b >= a);
    if (g_ge != 1) return 6;

    // Test false cases
    g_eq = (a == b);
    if (g_eq != 0) return 7;

    g_lt = (b < a);
    if (g_lt != 0) return 8;

    return 0;
}
